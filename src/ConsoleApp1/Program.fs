open FSharpPlus
open FSharpPlus.Data
open FSharpPlus.Lens

type Cell =
    | Covered of Cell
    | Number of int
    | Bomb
    override this.ToString() =
        match this with
        | Covered n -> "."
        | Number n -> string n
        | Bomb -> "*"

    member this.Click =
        match this with
        | Covered x -> x
        | _ -> this

type Minefield =
    | Setup of int * int
    | Playing of Map<(int * int), Cell>
    override this.ToString() =
        match this with
        | Setup _ -> "Setup"
        | Playing m ->
            let group = m |> Map.toSeq |> groupBy (fun ((x, _), _) -> x)

            seq {
                for (_, x) in group do
                    for (_, y) in x do
                        yield $"{y}"

                    yield "\n"
            }
            |> fold (fun s -> fun t -> s + t) ""


let cell = Covered Bomb

let minefield =
    seq {
        yield (1, 1), Number 3
        yield (0, 0), Bomb
        yield (1, 0), Number 1
        yield (0, 1), Number 2
    }
    |> Map.ofSeq
    |> Playing

printfn $"{minefield}"
