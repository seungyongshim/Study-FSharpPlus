open FSharpPlus

type Cell =
   | Covered of Cell
   | Number of int
   | Bomb

   override this.ToString() =
      match this with
      | Covered n -> "."
      | Number n -> n |> string
      | Bomb -> "*"

   member this.Click =
      match this with
      | Covered x -> x
      | _ -> this

type Width = Width of int
type Height = Height of int
type Column = Column of int
type Row = Row of int
type BombsPos = BombsPos of seq<(Row * Column)>

type Minefield =
   | Setup of Width * Height * BombsPos
   | Playing of Map<(Row * Column), Cell>

   member this.ToString1() =
      match this with
      | Setup _ -> "Setup"
      | Playing m ->
         let rows =
            m
            |> Map.toSeq
            |> groupBy (fun ((row, _), _) -> row)

         seq {
            for (_, row) in rows do
               for (_, cell) in row do
                  yield $"{cell}"

               yield "\n"
         }
         |> fold (+) ""

   member this.Start =
      match this with
      | Setup (Width w, Height h, BombsPos bombsPos) ->
         let init =
            seq {
               for y in [ 1..h ] do
                  for x in [ 1..w ] do
                     yield (Row y, Column x), Number 0 |> Covered
            }
            |> Map.ofSeq

         let withBombs =
            let setBomb x =
               match x with
               | Some _ -> Bomb |> Some
               | None -> None

            let change state pos = state |> Map.change pos setBomb
            (init, bombsPos) ||> fold change

         Playing(withBombs)
      | _ -> this


let minefield2 =
   seq {
      (Row 1, Column 1), Number 3
      (Row 1, Column 2), Bomb
      (Row 2, Column 1), Number 1
      (Row 2, Column 2), Number 2
   }
   |> Map.ofSeq
   |> Playing

let minefield = Setup(Width 3, Height 2, BombsPos [ (Row 1, Column 1) ])

printfn $"{minefield.Start}"
