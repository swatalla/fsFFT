namespace SignalFS

open System

open Functions
open LanguagePrimitives

module SignalFunctions =

    let (|EvenSignal|OddSignal|) (x: 'a[]) = if x.Length % 2 = 0 then EvenSignal x else OddSignal x

    let inline rect t =
        match t with
        | x when abs(x) < convert 0.5 -> GenericOne
        | x when abs(x) > convert 0.5 -> GenericZero
        | _ -> abs(t)

    let inline sinc x =
        match x with
        | x when x = GenericZero -> GenericOne
        | _ -> sin(x)/x

    let inline nsinc x =
        match x with
        | x when x = GenericZero -> GenericOne
        | _ -> (convert Math.PI) * x |> fun px -> sin(px)/px


        //let n = [| -1.0..0.1..1.0 |]
    //let inline dirac n (X: array<'a>) =

        //X |> Array.iteri (fun idx item -> if item = GenericZero && round(n'/(convert 2)) = n'/(convert 2) then Y.[idx] <- (convert infinity))