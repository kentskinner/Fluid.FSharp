module VelocityExtrapolation

open MACGrid

let private neighboringValues (field:float option [,]) i j =         
        let m,n = Array2D.length1 field, Array2D.length2 field  
        seq { if i > 0   then yield (i-1,j)
              if i < m-1 then yield (i+1,j)
              if j > 0   then yield (i,j-1)
              if j < n-1 then yield (i,j+1) }        
        |> Seq.choose (fun (i,j) -> field.[i,j])
        |> Seq.toList

let rec private extrapolateOptionField field =
    let hasInvalid = field |> Seq.cast<float option> |> Seq.exists (fun v -> v.IsNone)
    if not(hasInvalid)
    then field
    else
        let field' = 
            field |> Array2D.mapi (fun i j (vo:float option) ->
                if vo.IsSome
                then vo
                else let values = neighboringValues field i j
                     if values.IsEmpty
                     then None
                     else Some(List.average values))
        extrapolateOptionField field' 

let extrapolate (grid:MACGrid) isFluid =       
    let u' = 
        grid.u() 
        |> Array2D.mapi (fun ui uj value ->
                let isValid = isFluid ui uj || (ui > 0 && isFluid (ui-1) uj)
                if isValid then Some(value) else None)        
        |> extrapolateOptionField
        |> Array2D.map (fun opt -> opt.Value)
    let v' =
        grid.v()
        |> Array2D.mapi (fun vi vj value ->
                let isValid = isFluid vi vj || (vj > 0 && isFluid vi (vj-1))
                if isValid then Some(value) else None)
        |> extrapolateOptionField
        |> Array2D.map (fun opt -> opt.Value)
    MACGrid(u',v',grid.cellSize())
        