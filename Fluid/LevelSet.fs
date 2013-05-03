module LevelSet

open Types

let circle m n cx cy r = Array2D.init m n (fun i j ->
                            let x = float(i) + 0.5
                            let y = float(j) + 0.5
                            sqrt ( (x - cx)**2.0 + (y - cy)**2.0 ) - r)

let findGridPointsNearSurface (implicitSurfaceGrid : ScalarField) =

    // Find where the linear interpolate is zero between (i1,j1) and (i2,j2)
    let findZeroDistanceAndPoint i1 j1 i2 j2 =
        let v1 = implicitSurfaceGrid.[i1,j1]
        let v2 = implicitSurfaceGrid.[i2,j2]
        let distance = v1 / (v1 - v2)
        let point = float(i1) + 0.5 + distance * float(i2 - i1), 
                    float(j1) + 0.5 + distance * float(j2 - j1)
        distance, point
    
    let m,n = dimensions implicitSurfaceGrid
    seq { for i in 0..m-1 do
              for j in 0..n-1 do
                  let v = implicitSurfaceGrid.[i,j]
                  let neighbors = neighboringCells implicitSurfaceGrid i j
                  let candidateClosestPoints =
                    [ for ni, nj in neighbors do
                        let nv = implicitSurfaceGrid.[ni,nj]
                        if sign nv <> sign v then
                            yield findZeroDistanceAndPoint i j ni nj ]
                  if not(candidateClosestPoints.IsEmpty)
                  then let distance, closestPoint = candidateClosestPoints |> List.minBy fst
                       yield i, j, distance * float(sign v), closestPoint }
                  
let updateDistance sd i j dsign =
    let neighbors = Seq.append (neighboringCells sd i j) [(i,j)]    
    let distances = 
        [ for (ni, nj) in neighbors do
              match sd.[ni,nj] with
              | None -> ()
              | Some(neighbourDistance, closestPoint) ->
                  let x_ij = float(i) + 0.5, float(j) + 0.5
                  let d_ij = distance x_ij closestPoint
//                  if d_ij < abs neighbourDistance 
 //                 then printfn "d_ij = %f, nd = %f" d_ij neighbourDistance
//                       sd.[ni,nj] <- None
                  yield d_ij, closestPoint ]
    //printfn "(%d,%d) neighbors: %A" i j distances
    if not(distances.IsEmpty)
    then let dist, pt = distances |> List.minBy fst
         sd.[i,j] <- Some(dist * float(dsign), pt)
         //printfn "sd.[%d,%d] <- %A" i j (sd.[i,j])

let constructSignedDistance implicitSurfaceGrid =  
    let m,n = dimensions implicitSurfaceGrid   
    let cellsNearSurface = findGridPointsNearSurface implicitSurfaceGrid
    if Seq.isEmpty cellsNearSurface
    then Array2D.create m n 1.0
    else 
        let sd : (float*(float*float)) option [,] = Array2D.create m n None
        cellsNearSurface |> Seq.iter (fun (i,j,distance,closestPoint) -> 
            sd.[i,j] <- Some(distance, closestPoint))

        //printfn "Initial: %A" sd

        for sweep in 0..1 do
            for i in 0..m-1 do
                for j in 0..n-1 do
                    updateDistance sd i j (sign implicitSurfaceGrid.[i,j])
            //printfn "%A" sd
            for i in m-1..-1..0 do
                for j in 0..n-1 do
                    updateDistance sd i j (sign implicitSurfaceGrid.[i,j])
            //printfn "%A" sd
            for i in 0..m-1 do
                for j in n-1..-1..0 do
                    updateDistance sd i j (sign implicitSurfaceGrid.[i,j])
            //printfn "%A" sd
            for i in m-1..-1..0 do
                for j in n-1..-1..0 do
                    updateDistance sd i j (sign implicitSurfaceGrid.[i,j])
            //printfn "%A" sd
        sd |> Array2D.map (fun d -> fst d.Value)

