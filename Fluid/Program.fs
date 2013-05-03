module Fluid

open Types

let euler (x,y) (t:float) (u,v) = (x - t * u, y - t * v)

let bilinearInterpolate (x1:float) x2 y1 y2 q11 q12 q21 q22 x y =
    (q11*(x2-x)*(y2-y) + q21*(x-x1)*(y2-y) + q12*(x2-x)*(y-y1) + q22*(x-x1)*(y-y1)) / ((x2-x1)*(y2-y1))
        
let sample grid =
    let l1 = Array2D.length1 grid
    let l2 = Array2D.length2 grid
    let n = l1 / 30
    if n <= 1
    then grid
    else
        Array2D.init (l1/n) (l2/n) (fun i j -> grid.[i * n, j * n])



type Cell = (int * int * (int * int) list)







let marchingSquares field =
    let m,n = Array2D.length1 field, Array2D.length2 field
    let bitmap = makeBitmap field
    let cells = Array2D.init (m-1) (n-1) (fun i j ->
        let number = 
            1 * bitmap.[i,j] +
            2 * bitmap.[i+1,j] +
            4 * bitmap.[i+1,j+1] +
            8 * bitmap.[i,j+1]
        i, j, contourLines.[number] )

    let cellSeq = seq {
        for i in 0..m-1 do
            for j in 0..n-1 do
                yield cells.[i,j] }

    let start = cellSeq |> Seq.find (fun (_,_,lines) -> not lines.IsEmpty)
    let (i,j,startEdges) = start
    let edge = snd startEdges.[0]    

    0
        