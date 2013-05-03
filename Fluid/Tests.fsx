#r "../packages/MathNet.Numerics.2.5.0/lib/net40/MathNet.Numerics.dll"
#r "../packages/MathNet.Numerics.FSharp.2.5.0/lib/net40/MathNet.Numerics.FSharp.dll"

#load "Types.fs"
open Types
#load "Visualization.fs"

#load "MACGrid.fs"
open MACGrid

#load "PressureSolve.fs"
#load "VelocityExtrapolation.fs"
#load "Simulation.fs"
#load "LevelSet.fs"
#load "MarchingSquares.fs"
open Simulation

let clamp minValue maxValue x = min (max x minValue) maxValue

let rec markerParticleTracker(particles) =    
    {
        particles = particles
        addParticles = fun newParticles -> markerParticleTracker(newParticles @ particles)
        isFluid = fun i j ->
            particles |> Seq.exists (fun (x,y) ->
                abs (float(i) + 0.5 - x) <= 0.5 && abs (float(j) + 0.5 - y) <= 0.5)
        advect = fun grid boundary t ->
            let m,n = grid.size()
            // TODO: use Runge-Kutta
            // TODO: this assumes solid boundaries around grid.
            let max_x = float(m) * grid.cellSize() - epsilon
            let max_y = float(n) * grid.cellSize() - epsilon
            let p' = 
                particles 
                |> Seq.map (fun (x,y) ->
                    let dt = t / 10.0
                    let mutable x' = x
                    let mutable y' = y
                    for i in 1..10 do
                        let (vx,vy) = grid.velocityAtPt(x',y')
                        x' <- clamp 0.0 max_x (x' + dt * vx)
                        y' <- clamp 0.0 max_y (y' + dt * vy)
                        // Keep particle out of solid regions
                        if boundary.isSolid (int(x')) (int(y'))
                        then let cellsAlongSolidNormal = 
                                // TODO: this is a fake implementation
                                let x = int(x')
                                let y_start = int(y') + 1
                                seq { for y in y_start..n-1 do
                                          yield x,y }
                             match cellsAlongSolidNormal |> Seq.tryFind (fun (x,y) -> not(boundary.isSolid x y)) with
                             | Some(x,y)    -> y' <- float(y + 1) - epsilon
                             | None         -> failwith "Could not find a non-solid cell to move particle to"
                    x',y' ) 
                |> Seq.toList            
            markerParticleTracker(p')
    }

let rnd = System.Random(123)
let randomParticlesForCell i j =
    let jitter() = rnd.NextDouble() * 0.48 - 0.24
    // Determine cell center
    let cx, cy = float(i) + 0.5, float(j) + 0.5
    [ (cx - 0.25, cy - 0.25);
      (cx - 0.25, cy + 0.25);
      (cx + 0.25, cy - 0.25);
      (cx + 0.25, cy + 0.25) ]
    |> List.map (fun (x,y) -> x + jitter(), y + jitter())

//let grid = MACGrid(20, 25, 1.0)
//
//let boundary = {
//    isSolid = fun i j ->
//        let m,n = grid.size()
//        if i < 0 || i >= m || j < 0
//        then true
//        else false
//            //3 < i && i < 18 && 5 < j && j < 10
//}

//let particles : (float*float)list = 
//  [ ]
  //  [ for j in 0..(snd (grid.size())-3) do
   //     for i in 5..10 do
    //        yield! randomParticlesForCell i j ]
    
//let surface = markerParticleTracker(particles)

let t = 0.05

// Step through simulation
let steps boundary initialGrid initialSurface = 
    Seq.unfold (fun (grid, surface) ->     
        let s' = surface
        //if surface.isFluid 10 24
        //then surface
        //else surface.addParticles (randomParticlesForCell 10 24)
        let g',s'' = Simulation.step(grid, s', boundary, t)
        Some((g',s''), (g',s''))) (initialGrid, initialSurface)


open Visualization
open System.Drawing
let fluidColor = Color.FromArgb(200, Color.Blue)
let vff = new VectorFieldForm()
vff.Show()

let makeAnim (m:int) (n:int) initialFluidCells boundary =
    let grid = MACGrid(m, n, 1.0)
    let particles = [ for (i,j) in initialFluidCells do
                          yield! randomParticlesForCell i j ]   
    let surface = markerParticleTracker(particles)
    steps boundary grid surface |> Seq.cache

let metaballs m n surface =    
    Array2D.init m n (fun i j ->
        let cx = float(i) + 0.5
        let cy = float(j) + 0.5
        
        let v =
            surface.particles
            |> Seq.sumBy (fun (x,y) ->                
                let distSquared = (x-cx)**2.0+(y-cy)**2.0
                let contrib =
                    if distSquared > epsilon
                    then 1.0 / distSquared
                    else 10000.0 
                min contrib 4.0
                    )
        4.0-v)

let simple m n surface =
    Array2D.init m n (fun i j ->
        if surface.isFluid i j then -1.0 else 1.0)

let smooth m n surface =
    let s = simple m n surface
    s |> Array2D.mapi (fun i j v ->
            let avg = neighboringCells s i j
                      |> Seq.averageBy (fun (i,j) -> s.[i,j])
            0.5 * avg + 0.5 * v)

let makeFrame boundary (grid:MACGrid, surface) =
    let m,n = grid.size()
    let sd = LevelSet.constructSignedDistance (smooth m n surface)
    let contours = MarchingSquares.findContours sd
    let velocityField = grid.velocityField()
    let particles = surface.particles |> Seq.toList
    (contours, velocityField, particles)

let drawFrame (vff:VectorFieldForm) solid (contours, velocityField, particles) =
    vff.VectorField <- velocityField
    vff.Layers <- [| LevelSet(contours);
                     Particles(particles, Color.LightBlue);
                     Quantity(solid, 0.0, 1.0, Color.DarkGray);                        
                        |]

let animate_timed(vff:VectorFieldForm, frames, boundary) =    
    let (_,vf,_) = frames |> Seq.head
    let m,n = dimensions vf
    let solid = Array2D.init m n (fun i j -> 
        if boundary.isSolid i j then 1.0 else 0.0)
    frames |> Seq.iter (fun frame ->
        drawFrame vff solid frame

        System.Windows.Forms.Application.DoEvents()

        System.Threading.Thread.Sleep(50)
        )

let boundary = {
    isSolid = fun i j ->
        let m,n = 20, 30
        i < 0 || i >= m || j < 0 }
let anim = makeAnim 20 30 [ for i in 1..6 do
                               for j in 5..25 do
                                  yield (i,j) ] boundary
           |> Seq.cache
let frames = anim |> Seq.map (makeFrame boundary) |> Seq.cache

animate_timed(vff, frames |> Seq.take 500, boundary)