module MarchingSquares

open Types

type Direction = Forward | Backward

type Edge = Top | Right | Bottom | Left
    with member x.oppositeEdge = match x with
                                 | Top      -> Bottom
                                 | Bottom   -> Top
                                 | Right    -> Left
                                 | Left     -> Right

type Segment = 
    {
        edge1 : Edge
        edge2 : Edge
        i     : int
        j     : int
    }
    member x.EntryEdge(direction) = 
        match direction with
        | Forward -> x.edge1
        | Backward -> x.edge2
    member x.ExitEdge(direction) = 
        match direction with
        | Forward -> x.edge2
        | Backward -> x.edge1

type ContouringCell = 
    {
        i               : int
        j               : int        
        lineSegments    : (Edge*Edge) list
    }

let bitmapValue v = if v > 0.0 then 1 else 0

let lookupContour (field:ScalarField) i j binaryIndex =    
    let contourLinesTable =
        [| // Cases 0-3
           [];
           [ (Bottom,Left) ];
           [ (Right,Bottom) ];
           [ (Right,Left) ];

           // Cases 4-7
           [ (Top,Right) ];
           [ (Top,Left); (Bottom,Right) ];
           [ (Top,Bottom) ];
           [ (Top,Left) ];
       
           // Cases 8-11
           [ (Left,Top) ];
           [ (Bottom,Top) ];
           [ (Right,Top); (Left,Bottom) ];
           [ (Right,Top) ];
       
           // Cases 12-15
           [ (Left,Right) ];
           [ (Bottom,Right) ];
           [ (Left,Bottom) ];
           []; |]
    if binaryIndex = 5 || binaryIndex = 10
    then let avgDataValue = (field.[i,j] + field.[i+1,j] + field.[i,j+1] + field.[i+1,j+1]) / 4.0
         let centerContourLevel = bitmapValue avgDataValue
         match binaryIndex, centerContourLevel with
         | 5, 0 -> [ (Top,Right); (Bottom,Left) ]
         | 5, 1 -> [ (Top,Left); (Bottom,Right) ]
         | 10, 0 -> [ (Left,Top); (Right,Bottom) ]
         | 10, 1 -> [ (Right,Top); (Left,Bottom) ]
         | _ -> failwith "Error"
    else contourLinesTable.[binaryIndex]

let makeBitmap field = field |> Array2D.map bitmapValue



let makeContourCells field =
    let m,n = dimensions field
    let bitmap = makeBitmap field
    let binaryIndex i j = 
        1 * bitmap.[i,j] + 
        2 * bitmap.[i+1,j] + 
        4 * bitmap.[i+1,j+1] +
        8 * bitmap.[i,j+1]

    let grid = Array2D.init (m-1) (n-1) (fun i j ->               
        { i = i; 
          j = j; 
          lineSegments = lookupContour field i j (binaryIndex i j);
        })

    let segments = 
        Set.ofSeq (
            seq { for i in 0..m-2 do
                    for j in 0..n-2 do
                      let lineSegments = lookupContour field i j (binaryIndex i j)                      
                      yield! (lineSegments |> Seq.map (fun (e1,e2) ->
                        { i = i; j = j; edge1 = e1; edge2 = e2 })) })
    grid,segments

let boundaryCells m n i_start j_start =
    let bcs = 
        List.concat [ ([0..n-1]     |> List.map (fun j -> (0,j)));   // Left edge
                      ([1..m-1]     |> List.map (fun i -> (i,n-1))); // Top edge
                      ([n-2..-1..0] |> List.map (fun j -> (m-1,j))); // Right edge
                      ([m-2..-1..1] |> List.map (fun i -> (i,0)));   // Bottom edge
                      ] |> List.toArray
    let start_index = bcs |> Array.findIndex (fun (i,j) -> (i,j) = (i_start,j_start))
    Array.append
        (Array.sub bcs start_index (bcs.Length - start_index))
        (Array.sub bcs 0 start_index)
              

let nextSegment direction (currentSegment:Segment) (segments:Set<Segment>) =
    let exitEdge = currentSegment.ExitEdge direction
    let i_next, j_next =
        let i,j = currentSegment.i, currentSegment.j
        match exitEdge with
        | Top       -> i,j+1
        | Right     -> i+1,j
        | Bottom    -> i,j-1
        | Left      -> i-1,j
    let adjacentSegment = 
        segments |> Seq.tryFind (fun segment ->        
            segment.i = i_next 
            && segment.j = j_next
            && segment.EntryEdge direction = exitEdge.oppositeEdge)
    if adjacentSegment.IsSome
    then adjacentSegment
    else None
//    else let m,n = 1 + (segments |> Seq.map (fun seg -> seg.i) |> Seq.max),
//                   1 + (segments |> Seq.map (fun seg -> seg.j) |> Seq.max)
//         let bcs = boundaryCells m n currentSegment.i currentSegment.j
//         let segmentsOnBoundary = 
//             bcs |> Seq.skip 1 
//                 |> Seq.collect (fun (i,j) -> segments |> Seq.filter (fun seg -> (seg.i,seg.j) = (i,j)))
//         Some(segmentsOnBoundary |> Seq.head)
                  

let isClosed (contour:Segment list) =
    contour.Length > 1 && contour.[0] = contour.[contour.Length - 1]

let findContour (segments : Set<Segment>) =
    let followContour direction (start:Segment) = [ 
        let current = ref (Some(start))
        let count = ref 0        
        while (!current).IsSome do
            let currentSegment = current.Value.Value
            yield currentSegment

            current := nextSegment direction currentSegment segments 

            // Closed loop?
            if !current = Some(start)
            then yield start
                 current := None 

            // Infinte loop?
            incr count
            if !count > 10000 then
                failwithf "Probable infinite loop found in contour starting at %A" start     
             ]
    let start = segments |> Seq.head
    let forwardContour = followContour Forward start
    let contour =
        if isClosed forwardContour
        then forwardContour    // return closed contour
        else
            let backContour = followContour Backward start
            (backContour |> List.rev) @ forwardContour.Tail
    contour, segments - (Set.ofList contour)

let contourPoints (field:Types.ScalarField) (contour:Segment list) =
    let interpolate i j edge = 
        let getFieldValue = Array2D.get field
        let corner1,corner2 = 
            match edge with
            | Top       -> (i,j+1),(i+1,j+1)
            | Right     -> (i+1,j),(i+1,j+1)
            | Bottom    -> (i,j),(i+1,j)
            | Left      -> (i,j),(i,j+1)
        let v1 = corner1 ||> getFieldValue
        let v2 = corner2 ||> getFieldValue
        let p = -v1/(v2-v1)
        let dx = p * float(fst corner2 - fst corner1)
        let dy = p * float(snd corner2 - snd corner1)
        //printfn "f=%d,i=%d,j=%d,corner1=%A,corner2=%A,v1=%f,v2=%f,p=%f,dx=%f,dy=%f" f i j corner1 corner2 v1 v2 p dx dy
        float(fst corner1) + 0.5 + dx, float(snd corner1) + 0.5 + dy
            
    let points = contour |> List.map (fun segment -> 
        interpolate segment.i segment.j segment.edge2)
    if isClosed contour
    then points
    else let firstSegment = contour |> Seq.head
         let firstPoint = interpolate firstSegment.i firstSegment.j firstSegment.edge1
         firstPoint :: points

let extendField field minValue =
    let m,n = dimensions field
    let extendedField =
        Array2D.init (m+2) (n+2) (fun i j ->
            match i,j with
            | 0,_ -> minValue
            | _,0 -> minValue
            | i,j when i=m+1 || j=n+1 -> minValue            
            | _ -> field.[i-1,j-1])
    extendedField

let findContours (field:ScalarField) =   

    let extendedField = extendField field 0.1
    
    let rec findContoursUtil segments =
        if Set.isEmpty segments
        then []
        else let contour, remainingSegments = findContour segments
             contour :: findContoursUtil remainingSegments

    let _, segments = makeContourCells extendedField
    let contours = findContoursUtil segments
    let offset points =
        points |> List.map (fun (x,y) -> (x - 1.0, y - 1.0))
    contours |> List.map (contourPoints extendedField >> offset)    
