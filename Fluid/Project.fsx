#r "../packages/MathNet.Numerics.2.5.0/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.2.5.0/lib/net40/MathNet.Numerics.FSharp.dll"

#load "Types.fs"
#load "MACGrid.fs"

open MACGrid

open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Double.Solvers.Iterative
    
let divergences (grid:MACGrid) =
    let u,v = grid.u(), grid.v()
    let m,n = grid.size()
    DenseVector.init (m*n) (fun ix -> 
        let i = ix / n
        let j = ix % n
        -(u.[i+1,j] - u.[i,j] + v.[i,j+1] - v.[i,j]) / grid.cellSize())    


let project(t, grid:MACGrid) =
    let m,n = grid.size()
    let d = divergences grid    

    let isFluid i j = i >= 0 && i < m && j >= 0 && j < n

    let idx i j = (i*n)+j

    let A = SparseMatrix.OfIndexed(m*n,m*n,
                seq {
                    for i in 0..m-1 do
                        for j in 0..n-1 do
                            let row = idx i j                                                      
                            yield (row, row, 4.0)
                            for (k,l) in [(i+1,j);(i-1,j);(i,j+1);(i,j-1)] do
                                if isFluid k l then
                                    yield (row, idx k l, -1.0)
                })

    let b = d * 1000.0 * grid.cellSize()**2.0 / t
    let solver = MlkBiCgStab()
    let p = solver.Solve(A, b)
    let pressure i j =
        if i < 0 || i >= m || j < 0 || j >= n
        then 0.0
        else p.[idx i j]
    
    // Apply pressure update
    let u' = grid.u() |> Array2D.mapi (fun i j u ->        
        u - t / 1000.0 * (pressure i j - pressure (i-1) j) / grid.cellSize())
    let v' = grid.v() |> Array2D.mapi (fun i j v ->        
        v - t / 1000.0 * (pressure i j - pressure i (j-1)) / grid.cellSize())
    MACGrid(u',v',grid.cellSize())
    


// Test
let rnd = System.Random()
let grid = MACGrid(20, 20, 1.0, fun _ -> float(rnd.Next(-10,10)), float(rnd.Next(-10,10)) )
let g2 = project(0.1, grid)
divergences g2


// Small test
let u = array2D [[4.0; -5.0]
                 [-6.0; -3.0]
                 [-8.0; 4.0]]
let v = array2D [[-3.0; 9.0; -10.0]
                 [-1.0; 0.0; -7.0]]
let grid = MACGrid(u, v, 1.0)                   
let g2 = project(0.1, grid)
divergences g2

let f = Array2D.init 20 20 (fun i j -> 0.0)

let f = g2.advect(f)
vff.Layers <- [| Quantity(f, 0.0, 40.0, System.Drawing.Color.Blue) |]