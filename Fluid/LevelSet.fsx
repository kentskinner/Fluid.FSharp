#load "Types.fs"
#load "Visualization.fs"
#load "LevelSet.fs"
open System.Drawing
open Visualization
open LevelSet

// Grid for a "question mark" shape
let implicitSurfaceGrid = 
    array2D [ 
      [1.0; 1.0; 1.0;1.0; 1.0;   1.0; 1.0; 1.0; 1.0;1.0; ];
      [0.4; 0.1; 0.7;1.0; 0.3;  -0.2;-0.1; 0.7; 1.0;1.0; ];
      [0.7;-0.3;-0.1;0.6; 0.1;  -0.4;-0.5;-0.1; 0.7;1.0; ];
      [1.0; 0.5; 0.2;0.6;-0.2;  -0.1; 0.5; 0.1;-0.1;0.7; ];
      [1.0; 1.0; 1.0;1.0; 0.8;   0.2; 0.7; 0.7;-0.1;0.1; ]; ]

let gps = findGridPointsNearSurface implicitSurfaceGrid

let sd = constructSignedDistance implicitSurfaceGrid

#load "MarchingSquares.fs"
let contours = MarchingSquares.findContours sd

let vff = new VectorFieldForm()
//vff.Layers <- [| Particles(gps |> Seq.map (fun (_,_,_,pt) -> pt), Color.Black) |]
vff.Layers <- [| LevelSet(contours) |]
vff.Show()