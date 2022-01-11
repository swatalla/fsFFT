namespace SignalFS

open Functions

module ModifiedBessel =

    let private ε = 1e-6
    let inline private (^^) x n = pown x n
    let inline private (!!) n = n |> fac |> double
    let inline private Γ n = gamma n

    let inline besseli n z =
        let besselSum = 
            Seq.initInfinite (fun k -> 
                match n with
                | 0 -> (pown ((z*z)/4.0) k) / (pown (!!k) 2)
                | _ -> (pown (0.5 * z) n) * (pown ((z*z)/4.0) k) / ((!!k) * Γ(n+k+1)))
            |> Seq.scan (+) 0.0
        let index = 
            besselSum // Should this be limited to Seq.take n samples?
            |> Seq.windowed 2
            |> Seq.map (Seq.reduce (-))
            |> Seq.tryFindIndex (fun x -> abs x < ε)
            |> function 
               | Some idx -> idx + 1 
               | None -> 0
             
        besselSum |> Seq.take index |> Seq.last

    let inline I0 z = besseli 0 z