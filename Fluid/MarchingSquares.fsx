#load "Types.fs"
#load "Visualization.fs"
#load "MarchingSquares.fs"

open System.Drawing
open Visualization

let testField = 
    let circles = [ (5.0, 8.0, 4.0);
                    (15.2, 16.1, 3.7);
                    (16.5, 5.3, 1.9); ]
    Array2D.init 20 20 (fun i j -> 
        // Find cell center
        let cx = float(i) + 0.5
        let cy = float(j) + 0.5
        let distancesToCircles = 
            circles |> Seq.map (fun (x,y,r) ->
                sqrt ( (cx - x)**2.0 + (cy - y)**2.0 ) - r)
        distancesToCircles |> Seq.min)

let testFieldSmall =
    array2D [ [ 1.5; 0.5; 0.5 ]; [0.5; -0.5; -0.5 ]; [1.5; 0.5; 0.5 ] ] 

let testFieldRandom(seed) = 
    let rnd = System.Random(seed)
    Array2D.init 5 5 (fun i j -> rnd.NextDouble() * 2.0 - 1.0)
let randomField = testFieldRandom(123)

let m,n = Types.dimensions randomField
let vff = new VectorFieldForm()
vff.VectorField <- Array2D.create m n (0.0,0.0)
vff.Show()

let drawContours (vff:VectorFieldForm) field =    
    let contours = MarchingSquares.findContours field
    vff.Layers <- [| LevelSet(contours) |]

drawContours vff randomField
drawContours vff (testFieldRandom(11))




