module PressureSolve

open MACGrid

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Double.Solvers.Iterative

type CellType = Solid | Air | Fluid

let fluidCells m n cellType =
    seq { 
        for i in 0..m-1 do
            for j in 0..n-1 do
                match (cellType i j) with
                | Fluid -> yield (i,j)
                | _ -> () }
    
let divergences (grid:MACGrid) cellType =        
    let m,n = grid.size()
    fluidCells m n cellType
    |> Seq.map (fun (i,j) -> grid.divergence(i, j))        


let project(t, grid:MACGrid, cellType) =
    let m,n = grid.size()
    let u,v = grid.u(), grid.v()            

    let fcs = 
        fluidCells m n cellType        
        |> Seq.mapi (fun row (i,j) -> (row, i, j))
        |> Seq.toList

    let rowLookup = dict (fcs |> Seq.map (fun (row, i, j) ->(i,j), row))

    let numberOfNonSolidNeighbors i j =
        [(i+1,j);(i-1,j);(i,j+1);(i,j-1)]
        |> Seq.sumBy (fun (a,b) -> match cellType a b with Solid -> 0 | _ -> 1)
            
    let A = 
        SparseMatrix.OfIndexed(
            fcs.Length, 
            fcs.Length, 
            seq { for (row,i,j) in fcs do
                      yield (row, row, float(numberOfNonSolidNeighbors i j))
                      for (k,l) in [(i+1,j);(i-1,j);(i,j+1);(i,j-1)] do
                          match cellType k l with
                          | Fluid -> yield (row, rowLookup.[k,l], -1.0)
                          | _ -> () })
    //printfn "A = %A" A

    let adjustedDivergence i j =
        let u1 = match cellType (i+1) j with Solid -> 0.0 | _ -> u.[i+1,j]
        let u0 = match cellType (i-1) j with Solid -> 0.0 | _ -> u.[i,j]
        let v1 = match cellType i (j+1) with Solid -> 0.0 | _ -> v.[i,j+1]
        let v0 = match cellType i (j-1) with Solid -> 0.0 | _ -> v.[i,j]
        -(u1 - u0 + v1 - v0) / grid.cellSize()

    let d = DenseVector.ofSeq(fcs |> Seq.map (fun (row, i, j) -> adjustedDivergence i j))

    let idx i j = (i*n)+j               

    let b = d * 1000.0 * grid.cellSize()**2.0 / t
    //printfn "b = %A" b

    let fluidPressures = 
        // MlkBiCgStab seems to require at least two rows
        if A.RowCount > 1 
        then
            //let solver = MlkBiCgStab()
            //let solver = TFQMR()
            let solver = GpBiCg()
            let result = solver.Solve(A, b)
            match box solver.IterationResult with
            | :? MathNet.Numerics.LinearAlgebra.Generic.Solvers.Status.CalculationFailure -> 
                    DenseVector.zeroCreate A.RowCount :> Vector
            | _ -> result                        
        else
            DenseVector.ofList([ b.[0] / A.[0,0] ]) :> Vector
            

    //printfn "p = %A" fluidPressures

    //printfn "rowLookup = %A" rowLookup

    // Apply pressure update   

    let u_pressureGradient ui uj =
        match (cellType (ui-1) uj, cellType ui uj) with
        | Fluid, Fluid -> fluidPressures.[rowLookup.[ui,uj]] - fluidPressures.[rowLookup.[ui-1,uj]]
        | Air, Fluid   -> fluidPressures.[rowLookup.[ui,uj]]
        | Fluid, Air   -> -fluidPressures.[rowLookup.[ui-1,uj]]
        | Fluid, Solid | Solid, Fluid -> 
            1000.0 / t * grid.cellSize() * grid.u().[ui,uj]
        | _, _ -> failwith "Error"

    let v_pressureGradient vi vj =
        match (cellType vi (vj-1), cellType vi vj) with
        | Fluid, Fluid -> fluidPressures.[rowLookup.[vi,vj]] - fluidPressures.[rowLookup.[vi,vj-1]]
        | Air, Fluid   -> fluidPressures.[rowLookup.[vi,vj]]
        | Fluid, Air   -> -fluidPressures.[rowLookup.[vi,vj-1]]
        | Fluid, Solid | Solid, Fluid -> 
            1000.0 / t * grid.cellSize() * grid.v().[vi,vj]
        | _, _ -> failwith "Error"
        

    let isFluid i j = match cellType i j with Fluid -> true | _ -> false
    let u_bordersFluidCell ui uj =
        isFluid ui uj || (ui > 0 && isFluid (ui-1) uj)
    let v_bordersFluidCell vi vj =
        isFluid vi vj || (vj > 0 && isFluid vi (vj-1))
        
    let u' = grid.u() |> Array2D.mapi (fun i j u ->    
        if u_bordersFluidCell i j 
        then u - t / 1000.0 * (u_pressureGradient i j) / grid.cellSize()
        else 0.0)
    let v' = grid.v() |> Array2D.mapi (fun i j v ->     
        if v_bordersFluidCell i j
        then v - t / 1000.0 * (v_pressureGradient i j) / grid.cellSize()
        else 0.0)
    let grid' = MACGrid(u',v',grid.cellSize())
    
    let divergences = divergences grid' cellType
    // printfn "divergences: %A" divergences
    let totalDiv = Seq.sum divergences
    if (totalDiv > 0.01)
    then failwithf "Divergence is non-zero: %f" totalDiv
    
    grid'
    