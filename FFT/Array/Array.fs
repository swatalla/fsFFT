namespace SignalFS

open Functions
open LanguagePrimitives

module Array =

    let inline interleave (a: 'a[]) (b: 'a[]) =
        Array.foldBack2 (fun a b xs -> [|yield! [|a; b|]; yield! xs|]) a b [| |]
        // Array.foldBack2 (fun a b xs -> [|yield a; yield b; yield! xs|]) a b [| |]

    let inline isRectangle (m: 'a[][]) =
        let lmax = m |> Array.map (fun x -> x.Length) |> Array.max
        m |> Array.forall (fun x -> x.Length = lmax)

    let inline getColumn (n: int) (m: 'a[][]) =
        if not (m |> isRectangle) then invalidOp "Matrix must be rectangular"
        let nr = m.Length
        Array.init nr (fun r -> m.[r].[n])

    let inline diagonal (n: int) (m:'a[][]): 'a[] =
        if not (m |> isRectangle) then invalidOp "Matrix must be rectangular"
        let axis = m.Length - (abs n)
        Array.init axis (fun i -> if n > 0 then m.[i].[i+n] else m.[i-n].[i])

    let inline variance (arr: 'a[]) =
        let xbar = arr |> Array.average
        arr |> Array.map (fun x -> pown (x - xbar) 2) |> Array.average

    let inline stdDev (arr: 'a[]) =
        arr |> variance |> sqrt

    let inline absDev (arr: 'a[]) =
        let xbar = arr |> Array.average
        arr |> Array.map (fun x -> x - xbar |> abs) 
            |> Array.sum
            |> DivideByInt <| arr.Length

    let zeroPad (size: int) (arr: 'a[]) =
        let newarr = Array.zeroCreate size
        Array.blit arr 0 newarr 0 arr.Length; newarr

    let mapFold2 (mapping: 'T1 -> 'T2 -> 'U) (folding: 'State -> 'U -> 'State) state a b =
           (a, b) ||> Array.map2 mapping |> Array.fold folding state

    let inline dxSpaced (x1: 'a) (x2: 'a) (n: int) =
        match n with
        | Zero -> [| |]
        | One -> [| average [|x1; x2|] |]
        | Positive -> Array.init n (fun i -> x1 + ((x2 - x1) / convert(n - 1)) * (convert i))
        | Negative -> invalidArg "n" "number of linspace elements must be positive"

    let inline padContinuous (size: int) (arr: 'a[]) =
        let cfx = dxSpaced (arr |> Array.last) arr.[0] (size-arr.Length) 
        Array.append arr cfx

    let inline mapReflect (index: int) (count: int) mapping (x: 'a[]) =
        if index > x.Length then invalidArg "n" "index must be less than array length"
        if count > x.Length then invalidArg "d" "count must be less than array length"
        let a1 = x.[..index]
        let a2 = a1 |> Array.rev |> fun a2' -> a2'.[1..count] |> Array.map mapping
        Array.append a1 a2