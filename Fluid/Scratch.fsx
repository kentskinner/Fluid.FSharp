#load "Types.fs"
#load "MACGrid.fs"
#load "Program.fs"
#load "Visualization.fs"

open Fluid
open MACGrid


                            
let phi_circle c r = fun i j ->
    let x = float(i) + 0.5
    let y = float(j) + 0.5
    sqrt ( (x-fst c)**2.0 + (y-snd c)**2.0 ) - r

//let phi x y = sqrt ( (x-4.0)**2.0 + (y-4.0)**2.0 ) - 3.0

let field = Array2D.init 20 20 (fun i j -> phi (float(i), float(j)))
let cells = makeContourCells field

let p = findContour cells.[3,7] cells

let contourPoints (signedField:ScalarField) (contour:ContouringCell list) =
    let cellPoint cell =
        let f = snd cell.lineSegments.Head
        let i,j=cell.i,cell.j
        let corner1,corner2 = match f with
                    | 0 -> (i,j+1),(i+1,j+1)
                    | 1 -> (i+1,j),(i+1,j+1)
                    | 2 -> (i,j),(i+1,j)
                    | 3 -> (i,j),(i,j+1)
                    | _ -> failwith ""        
        let v1 = signedField.[fst corner1, snd corner1]
        let v2 = signedField.[fst corner2, snd corner2]
        let p = -v1/(v2-v1)
        let dx = p * float(fst corner2 - fst corner1)
        let dy = p * float(snd corner2 - snd corner1)
        printfn "f=%d,i=%d,j=%d,corner1=%A,corner2=%A,v1=%f,v2=%f,p=%f,dx=%f,dy=%f" f i j corner1 corner2 v1 v2 p dx dy
        float(fst corner1) + 0.5 + dx, float(snd corner1) + 0.5 + dy
    contour |> List.map cellPoint

let fn (x,y) = 5.0 * sin (0.5 * y), 5.0 * cos (0.2 * (x))

let macGrid = MACGrid(200, 200, 0.2, fn)

macGrid.advectVelocity()
vff.VectorField <- sample (macGrid.velocityField())

vff.Layers <- [| LevelSet(contourPoints p) |];;

let findContours signedField =
    let cells = makeContourCells signedField
    let nonEmptyCells = seq {
        let m,n = Array2D.length1 cells, Array2D.length2 cells
        for i in 0..m-1 do
            for j in 0..n-1 do
                let lineSegments = cells.[i,j].lineSegments
                if lineSegments <> []
                then yield cells.[i,j] }
    let firstNonEmptyCell = nonEmptyCells |> Seq.head
    let contour = findContour firstNonEmptyCell cells
    contourPoints signedField contour

let circle x y r = findContours (Array2D.init 20 20 (phi_circle (x, y) r))

open Visualization
    
let draw contourPts (vff:VectorFieldForm) =     
    vff.Layers <- [| LevelSet(contourPts) |]

let drawSignedField signedField vff = draw (findContours signedField) vff
                    

let applyPressure (grid:MACGrid) =
    grid

(**************************************************************************)

#r "../packages/MathNet.Numerics.2.5.0/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.2.5.0/lib/net40/MathNet.Numerics.FSharp.dll"

#load "Types.fs"
#load "Visualization.fs"

#load "MACGrid.fs"
open MACGrid

#load "PressureSolve.fs"
#load "VelocityExtrapolation.fs"
#load "Simulation.fs"    

open Simulation

let rec markerParticleTracker(particles) =
    {
        isFluid = fun i j -> 
            particles |> Seq.exists (fun (x,y) -> abs (float(i) + 0.5 - x) <= 0.5 &&
                                                  abs (float(j) + 0.5 - y) <= 0.5)
        advect = fun grid t ->
            let p' = particles |> Seq.map (fun (x,y) ->
                let (vx,vy) = grid.velocityAtPt(x,y)
                (x + t * vx, y + t * vy) ) |> Seq.cache
            markerParticleTracker(p')
    }

let randomMarkerParticleTracker() =
    let rnd = new System.Random(123)
    let particles = Array.init 400 (fun i ->
        5.0 + 10.0 * rnd.NextDouble(),
        5.0 + 10.0 * rnd.NextDouble())
    markerParticleTracker(particles)

// let signedDistance = Array2D.init 20 20 (phi_circle (10.0, 10.0) 4.0)

let grid = MACGrid(20, 20, 1.0)
let surface = randomMarkerParticleTracker()

open Visualization
let vff = VectorFieldForm()
vff.Show()

let fluidColor = System.Drawing.Color.FromArgb(200, System.Drawing.Color.Blue)

let grid, surface = Simulation.step(grid, surface)
let f = Array2D.init 20 20 (fun i j -> if surface.isFluid i j then 1.0 else 0.0)
vff.VectorField <- grid.velocityField()
vff.Layers <- [| Quantity(f, 0.0, 1.0, fluidColor) |]