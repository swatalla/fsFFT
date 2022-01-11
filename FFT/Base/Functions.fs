namespace SignalFS

open System
open LanguagePrimitives

// Make these inline instead of static methods

module Functions =

    [<AutoOpen>]
    module Cast =

        type Explicit =
            static member inline (>>=) (_:byte,   _:Explicit) = byte
            static member inline (>>=) (_:int16,  _:Explicit) = int16
            static member inline (>>=) (_:int32,  _:Explicit) = int32
            static member inline (>>=) (_:int64,  _:Explicit) = int64
            static member inline (>>=) (_:single, _:Explicit) = single
            static member inline (>>=) (_:double, _:Explicit) = double
            
        let inline convert value: 'T =
            (Unchecked.defaultof<'T> >>= Unchecked.defaultof<Explicit>) value

    [<AutoOpen>]
    module ActivePattern =
        let inline (|Zero|One|Positive|Negative|) n =
            match n with
            | x when x = GenericZero -> Zero
            | x when x = GenericOne -> One
            | x when x > GenericZero -> Positive
            | _ -> Negative

        let inline (|Greater|Equal|Less|) a b =
            match a,b with
            | x,y when x > y -> Greater
            | x,y when x < y -> Less
            | _ -> Equal

        let inline (|Even|Odd|) x = if x % 2 = 0 then Even else Odd

        let inline (|Power2|_|) (n: 'a) = 
            if (int n &&& (int n - 1)) = 0 && int n <> 0 then 
                Some Power2 
            else 
                None

        let inline (|AllZero|NonZero|) (arr: 'a[]) =
            if arr |> Array.forall (fun x -> x = GenericZero) then AllZero else NonZero

    let inline average (x: seq<'a>): 'a =
        x |> Seq.sum |> fun sum -> sum / (convert 2)

    let inline fac n = 
        match n with
        | z when z < GenericZero -> invalidArg "n" "n must be real and positive"
        | z when z = GenericZero || z = GenericOne -> GenericOne
        | _ -> [|GenericOne..n|] |> Array.reduce (*)

    let inline fix n = if n > GenericZero then floor n else ceil n

    let inline gamma n = 
        match n with
        | z when z <= GenericZero -> infinity
        | _ -> fac (double n - GenericOne)

    let inline half x = x / (convert 2)

    let inline interval a x b = if a < x && x < b then true else false
    
    let inline log2 (x: 'a) = Math.Log(double x, 2.0)

    let inline nextpow2 (x: 'a) = 
        match x with
        | Power2 -> x |> int
        | _ -> 1 <<< ((x |> log2 |> floor |> int) + 1)





