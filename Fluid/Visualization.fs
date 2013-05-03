module Visualization

open System.Drawing
open System.Drawing.Drawing2D
open System.Windows.Forms

open Types

type Layer =
    | LevelSet of ((float*float) list) list
    | Quantity of ScalarField * float * float * Color
    | Particles of (float*float) seq * Color

let pointf (x:float,y:float) = PointF(float32(x),float32(y))

type VectorFieldForm() as this =
    inherit Form()

    do
        this.SetStyle(ControlStyles.AllPaintingInWmPaint |||
                      ControlStyles.ResizeRedraw |||
                      ControlStyles.OptimizedDoubleBuffer, true)

    let mutable _layers : Layer[] = Array.empty
    let mutable _vectorField = Array2D.init 20 20 (fun i j -> float(i % 10), float(j % 10))

    let gridDimensions() = _vectorField.GetLength(0), _vectorField.GetLength(1)

    let getScale() = 
        let m,n = gridDimensions()
        min (this.ClientSize.Width / m) (this.ClientSize.Height / n)

    let gridSize() =
        let cellSize = getScale()
        let m,n = gridDimensions()
        m * cellSize, n * cellSize

    let drawLayer (g:Graphics) layer = 
        match layer with
        | LevelSet(contours) -> 
            use brush = new SolidBrush(Color.FromArgb(200, Color.Blue))
            use pen = new Pen(Color.FromArgb(200, Color.Blue))
            let scale = float(getScale())
            let gridWidth, gridHeight = gridSize()
            g.SetClip(RectangleF(0.0f,0.0f,float32(gridWidth),float32(gridHeight)))
            use path = new GraphicsPath(FillMode.Winding)
            for points in contours do
                let scaledPoints = 
                    points |> List.map (fun (x,y) -> pointf (scale * x, float(gridHeight) - scale * y))                
                path.AddClosedCurve(scaledPoints.Tail |> List.toArray)
//                if (scaledPoints.[0] = scaledPoints.[scaledPoints.Length - 1])
//                then g.FillClosedCurve(brush, scaledPoints.Tail |> List.toArray)
//                else g.DrawCurve(pen, scaledPoints |> List.toArray)
//                scaledPoints |> List.iter (fun pt ->
//                    g.FillEllipse(Brushes.Black, pt.X - 3.0f, pt.Y - 3.0f, 6.0f, 6.0f))
            g.FillPath(brush, path)
        | Quantity(field, minValue, maxValue, color) ->
            let max_alpha = int(color.A)
            let cellSize = getScale()
            let _, gridHeight = gridSize()
            for i in 0..(field.GetLength(0)-1) do
                for j in 0..(field.GetLength(1)-1) do
                    let a = int( (field.[i,j] - minValue) / (maxValue - minValue) * float(max_alpha))
                    let a = max 0 (min a max_alpha)
                    let brush = new SolidBrush(Color.FromArgb(a, color))
                    g.FillRectangle(brush, i * cellSize, gridHeight - (j+1) * cellSize, cellSize, cellSize)
        | Particles(particles, color) ->
            let cellSize = getScale()
            let _, gridHeight = gridSize()
            let brush = new SolidBrush(color)            
            let scale = float(getScale())
            let scaledPoints = 
                particles |> Seq.map (fun (x,y) -> pointf (scale * x, float(gridHeight) - scale * y))
            scaledPoints |> Seq.iter (fun pt ->
                let radius = 3.0f
                g.FillEllipse(brush, pt.X - radius, pt.Y - radius, radius*2.0f, radius*2.0f))

    //let vectorPen = new Pen(Color.Black, EndCap = Drawing2D.LineCap.ArrowAnchor)       
    let vectorPen = new Pen(Color.Gray)

    override x.OnPaint(e) =
        e.Graphics.Clear(Color.White)
        e.Graphics.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality

        let cellSize = getScale()
        let gridWidth, gridHeight = cellSize * _vectorField.GetLength(0), cellSize * _vectorField.GetLength(1) 
        let scale = float(cellSize) / 10.0
        for i in 0..(_vectorField.GetLength(0)) do
            let x = i * cellSize
            e.Graphics.DrawLine(Pens.LightGray, x, 0, x, gridHeight)
        for j in 0..(_vectorField.GetLength(1)) do
            let y = j * cellSize
            e.Graphics.DrawLine(Pens.LightGray, 0, y, gridWidth, y)          
                
//        _vectorField |> Array2D.iteri (fun i j (u,v) ->
//            let x, y = i * cellSize + cellSize/2, gridHeight - (j * cellSize + cellSize/2)
//            e.Graphics.DrawLine(vectorPen, x, y, x + int(scale * u), y - int(scale * v)) )
            

        _layers |> Array.iter (drawLayer e.Graphics)

    member x.VectorField 
        with set(value) = _vectorField <- value
                          //x.Location <- Point(0,0)                          
                          //x.ClientSize <- Size(20 * _vectorField.GetLength(0), 20 * _vectorField.GetLength(1))
                          x.Invalidate()

    member x.Layers 
        with get() = _layers 
        and set(value) = _layers <- value
                         x.Invalidate()