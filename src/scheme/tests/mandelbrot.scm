#light 
open Microsoft.FSharp.Math
open System

let maxIteration = 100

let modSquared (c : Complex) = c.r * c.r + c.i * c.i

type MandelbrotResult = 
    | DidNotEscape
    | Escaped of int
    
let mandelbrot c = 
    let rec mandelbrotInner z iterations = 
        if(modSquared z >= 4.0)
            then Escaped iterations
        elif iterations = maxIteration
            then DidNotEscape
        else mandelbrotInner ((z * z) + c) (iterations + 1)
    mandelbrotInner c 0

for y in [-1.0..0.1..1.0] do
    for x in [-2.0..0.05..1.0] do
        match mandelbrot (Complex.Create (x, y)) with
        | DidNotEscape -> Console.Write "#"
        | Escaped _ -> Console.Write " "
    Console.WriteLine () 


