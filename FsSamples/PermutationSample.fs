module PermutationSample

let permutation list =
    let rec pseq acc (a:Set<int>) =
        seq {
            if a.Count <= 0 then
                yield acc
            else
                for b in a do
                    let c = Set.remove b a
                    yield! pseq (acc@[b]) c
        }
    pseq [] <| set { 0 .. (List.length list - 1) }
    |> Seq.map (List.fold (fun acc i -> acc@[list.[i]]) [])

let run() =
    "abcd".ToCharArray()
    |> Array.toList
    |> permutation
    |> Seq.iter (printfn "%A")
