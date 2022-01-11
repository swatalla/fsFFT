namespace SignalFS

open System
open LanguagePrimitives

open Functions
open ModifiedBessel
open SignalFunctions

[<AutoOpen>]
module internal WindowFunction = 
    let rectangular' (t: float) (T: float): float = rect(t / T)

    let hanning' (m: float) (n: float): float = 
        0.5 * (1. - cos (2. * Math.PI * m / (n - 1.)))
    
    let hamming' (m: float) (n: float): float = 
        0.54 + (-0.46 * cos (2. * Math.PI * m / (n - 1.)))

module Window =

    let inline private (^^) x n = pown x n

    let inline private (|ZeroToNegativeInf|OnlyZero|OnlyOne|GreaterThanOne|) n = 
        match n with
        | n when n < GenericZero -> ZeroToNegativeInf n
        | n when n = GenericZero -> OnlyZero n
        | n when n = GenericOne -> OnlyOne n
        | _ -> GreaterThanOne n

    let bartlett = function
        | GreaterThanOne n -> 
               let m = n - 1 |> float
               Array.init n (fun k -> (2./m) * (m/2. - abs (float k - (m/2.)))) 
        | OnlyOne _ -> 1.0 |> Array.singleton
        | OnlyZero _ -> Array.empty
        | ZeroToNegativeInf _ -> failwith "Order must be positive"

    let kaiser (n: int) (β: float option) = 
        let B = match β with | Some b -> b | _ -> 0.5 // Math.PI * α
        let m, I0β = n |> float, I0 B |> abs
        let l = (m + 1.) / 2. |> fix |> int
        
        Array.init l (fun x -> 4. * pown (float x + 0.5 * (1. - (m % 2.))) 2)
        |> Array.map (fun k -> (I0 (B * sqrt (1. - k / (pown (m - 1.) 2)))) / I0β)
        |> fun w -> 
            [|yield! w.[(m % 2.) |> int..] |> Array.rev; yield! w|]

    let hamming = function
        | GreaterThanOne n ->
            Array.init n (fun m -> hamming' (float m) (float n))
        | OnlyOne _ -> 1.0 |> Array.singleton
        | OnlyZero _ -> Array.empty
        | ZeroToNegativeInf _ -> failwith "Order must be positive"

    let hanning = function 
        | GreaterThanOne n ->
            Array.init n (fun m -> hanning' (float m) (float n))
        | OnlyOne _ -> 1.0 |> Array.singleton
        | OnlyZero _ -> Array.empty
        | ZeroToNegativeInf _ -> failwith "Order must be positive"






    
    
    let windisp win = win |> Array.iter (fun elem -> printfn "%.4f" elem)

 

