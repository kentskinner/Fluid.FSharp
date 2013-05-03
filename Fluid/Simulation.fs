module Simulation

open MACGrid

type Boundary =
    {
        isSolid : int -> int -> bool
    }

type SurfaceTracker =
    {
        isFluid : int -> int -> bool        
        particles : (float*float) seq
        advect : MACGrid -> Boundary -> float -> SurfaceTracker
        addParticles : (float*float) list -> SurfaceTracker
    }

let addBodyForces (grid:MACGrid) t = grid.map_v (fun v -> v - t * 9.81)

let step(grid:MACGrid, surface, boundary, t) =

    // let sw = System.Diagnostics.Stopwatch.StartNew()

    //printfn "-----------------------------------"

    // Make sure the ﬂuid/air signed distance function φn is signed distance if necessary
    
    // Extrapolate the divergence-free velocity ﬁeld from the last time 
    // step into the air region, to get an extended velocity field
    // printfn "Before extrapolation: u = %A v = %A" (grid.u()) (grid.v())
    let extendedVelocityField = VelocityExtrapolation.extrapolate grid surface.isFluid
    //printfn "After extrapolation: u = %A v = %A" (extendedVelocityField.u()) (extendedVelocityField.v())

    // Advect surface through extended velocity field
    //printf "\tt0: %d" sw.ElapsedMilliseconds
    let surface' = surface.advect extendedVelocityField boundary t
    //printf "\tt1: %d" sw.ElapsedMilliseconds

    // Add body forces
    let interimVelocityField = addBodyForces extendedVelocityField t
       
    let vf' = grid.advect(interimVelocityField,t)
    let cellType i j =
        if boundary.isSolid i j
        then PressureSolve.Solid        
        elif surface'.isFluid i j
        then PressureSolve.Fluid
        else PressureSolve.Air

    //printf "\tt2: %d" sw.ElapsedMilliseconds
    let v_next = PressureSolve.project(t, vf', cellType)
    //printfn "\tt3: %d" sw.ElapsedMilliseconds
    v_next, surface'