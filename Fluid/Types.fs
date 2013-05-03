module Types

type ScalarField = float [,]
type VectorField = (float * float) [,]

let epsilon = 0.0000001


let dimensions array = Array2D.length1 array, Array2D.length2 array

let neighboringCells array i j =
    let m,n = dimensions array
    seq { if i > 0 then yield i-1,j
          if i < m-1 then yield i+1,j
          if j > 0 then yield i,j-1
          if j < n-1 then yield i,j+1 }

let distance (x1,y1) (x2,y2) = sqrt ( (x2-x1)**2.0 + (y2-y1)**2.0 )