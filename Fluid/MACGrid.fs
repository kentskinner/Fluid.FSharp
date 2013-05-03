module MACGrid

open Types

let euler (x,y) (t:float) (u,v) = (x - t * u, y - t * v)

let bilinearInterpolate (x1:float) x2 y1 y2 q11 q12 q21 q22 x y =
    (q11*(x2-x)*(y2-y) + q21*(x-x1)*(y2-y) + q12*(x2-x)*(y-y1) + q22*(x-x1)*(y-y1)) / ((x2-x1)*(y2-y1))

let u_position cellSize i j = (float(i) * cellSize, (float(j)+0.5) * cellSize)
let v_position cellSize i j = ((float(i)+0.5) * cellSize, float(j) * cellSize)

type MACGrid(_u, _v, cellSize) =
   
    let cell_center i j = (float(i)+0.5)*cellSize, (float(j)+0.5)*cellSize             
    let m = Array2D.length1 _v
    let n = Array2D.length2 _u              

    let u_velocity i j = let i1 = max 0 (i-1)
                         let i2 = min (m-1) i
                         (_u.[i,j], (_v.[i1,j] + _v.[i1,j+1] + _v.[i2,j] + _v.[i2,j+1]) / 4.0)

    let v_velocity i j = let j1 = max 0 (j-1)
                         let j2 = min (n-1) j
                         ((_u.[i,j1] + _u.[i+1,j1] + _u.[i,j2] + _u.[i+1,j2]) / 4.0, _v.[i,j])

    let constrain (x,y) =
        min (max x 0.0) (float(m) * cellSize),
        min (max y 0.0) (float(n) * cellSize)

    let u_at_pt (x,y) =
        let x,y = constrain (x,y)

        let i = min (int(x / cellSize)) (m-1)
        let j = min (int((y - cellSize / 2.0) / cellSize)) (n-2)
        let x1 = float(i) * cellSize
        let x2 = x1 + cellSize
        let y1 = float(j) * cellSize + cellSize/2.0
        let y2 = y1 + cellSize
        bilinearInterpolate x1 x2 y1 y2 _u.[i,j] _u.[i,j+1] _u.[i+1,j] _u.[i+1,j+1] x y
        
    let v_at_pt (x,y) =        
        let x,y = constrain (x,y)

        let i = min (int((x - cellSize/2.0) / cellSize)) (m-2)
        let j = min (int(y / cellSize)) (n-1)
        let x1 = float(i) * cellSize + cellSize/2.0
        let x2 = x1 + cellSize
        let y1 = float(j) * cellSize
        let y2 = y1 + cellSize
        bilinearInterpolate x1 x2 y1 y2 _v.[i,j] _v.[i,j+1] _v.[i+1,j] _v.[i+1,j+1] x y             
        
    let interpolate (field:ScalarField) position =
        let x,y = constrain position
        let i = min (int((x - cellSize/2.0) / cellSize)) (m-2)
        let j = min (int((y - cellSize/2.0) / cellSize)) (n-2)
        let x1 = float(i) * cellSize + cellSize/2.0
        let x2 = x1 + cellSize
        let y1 = float(j) * cellSize + cellSize/2.0
        let y2 = y1 + cellSize
        bilinearInterpolate x1 x2 y1 y2 field.[i,j] field.[i,j+1] field.[i+1,j] field.[i+1,j+1] x y

    new(m,n,cellSize,velocityFn) =
        let u = Array2D.init (m+1) n (fun i j -> 
            let x,y = u_position cellSize i j
            fst (velocityFn (x,y)))
        let v = Array2D.init m (n+1) (fun i j -> 
            let x,y = v_position cellSize i j
            snd (velocityFn (x,y)))
        MACGrid(u,v,cellSize) 

    new(m:int,n:int,cellSize) = MACGrid(m,n,cellSize,fun (x,y) -> (0.0,0.0))

    // Divergence of grid cell i j
    member g.divergence(i,j) =
        -(_u.[i+1,j] - _u.[i,j] + _v.[i,j+1] - _v.[i,j]) / cellSize

    member g.u() = _u
    member g.v() = _v
    member g.size() = m,n
    member g.cellSize() = cellSize

    member g.map_v(v_fn) = MACGrid(_u, _v |> Array2D.map v_fn, cellSize)

    member g.velocityField() =
        Array2D.init m n (fun i j -> (_u.[i,j]+_u.[i+1,j])/2.0, (_v.[i,j]+_v.[i,j+1])/2.0)

    member g.velocityAtPt(pt) = (u_at_pt pt, v_at_pt pt)

    member g.cellFaces() =
        seq {
            // u-samples
            for i in 0..m do
                for j in 0..(n-1) do
                    let x = float(i) * cellSize
                    let y = (float(j)+0.5) * cellSize
                    let i1 = max 0 (i-1)
                    let i2 = min (m-1) i
                    yield (x,y),(_u.[i,j], 
                                 (_v.[i1,j] + _v.[i1,j+1] + _v.[i2,j] + _v.[i2,j+1]) / 4.0)
            // v-samples
            for i in 0..(m-1) do
                for j in 0..n do
                    let x = (float(i)+0.5) * cellSize
                    let y = float(j) * cellSize                    
                    let j1 = max 0 (j-1)
                    let j2 = min (n-1) j
                    yield (x,y),((_u.[i,j1] + _u.[i+1,j1] + _u.[i,j2] + _u.[i+1,j2]) / 4.0, 
                                 _v.[i,j])
        }    

    member g.advect(velocityField:MACGrid,t) =
        let u_next = velocityField.u() |> Array2D.mapi (fun i j u ->
            let position = u_position cellSize i j
            let velocity = u_velocity i j
            let oldPosition = euler position t velocity
            fst (velocityField.velocityAtPt(oldPosition)))
        
        let v_next = velocityField.v() |> Array2D.mapi (fun i j v ->
            let position = v_position cellSize i j
            let velocity = v_velocity i j
            let oldPosition = euler position t velocity
            snd (velocityField.velocityAtPt(oldPosition)))
        MACGrid(u_next,v_next,cellSize)

    member g.advect(field:ScalarField,t) =
        let velocityField = g.velocityField()
        field |> Array2D.mapi (fun i j f ->
            let position = cell_center i j
            let velocity = velocityField.[i,j]
            let oldPosition = euler position t velocity
            interpolate field oldPosition)