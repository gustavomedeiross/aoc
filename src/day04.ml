module Pos = struct
  open Sexplib.Std

  type t = int * int [@@deriving eq, ord, sexp]
end

module Pos_set = struct
  include Set.Make (Pos)

  let create_or_add pos = function
    | None -> Some (singleton pos)
    | Some set -> Some (add pos set)
  ;;
end

module Char_map = Map.Make (Char)

type direction =
  | N
  | NE
  | E
  | SE
  | S
  | SW
  | W
  | NW
[@@deriving enumerate]

let next_letter_pos (x, y) = function
  | N -> x, y - 1
  | NE -> x + 1, y - 1
  | E -> x + 1, y
  | SE -> x + 1, y + 1
  | S -> x, y + 1
  | SW -> x - 1, y + 1
  | W -> x - 1, y
  | NW -> x - 1, y - 1
;;

let ( let* ) opt f =
  match opt with
  | Some x -> f x
  | None -> false
;;

(** Checks whether a word exists in a particular position and direction *)
let rec word_exists chmap word (x, y) dir =
  match word with
  | [] -> true
  | letter :: word ->
    let* positions = Char_map.find_opt letter chmap in
    let* _ = Pos_set.find_opt (x, y) positions in
    let pos = next_letter_pos (x, y) dir in
    word_exists chmap word pos dir
;;

let xmas = "XMAS" |> String.to_seq |> List.of_seq

(* Part 1 *)
let count_xmas chmap =
  let xmas_exists = word_exists chmap xmas in
  chmap
  |> Char_map.find 'X'
  |> Pos_set.to_list
  |> List.map (fun pos ->
    all_of_direction |> List.filter (xmas_exists pos) |> List.length)
  |> List.fold_left ( + ) 0
;;

let string_not_empty = function
  | "" -> false
  | _ -> true
;;

let solve input =
  let chmap =
    input
    |> String.split_on_char '\n'
    |> List.to_seq
    |> Seq.map String.trim
    |> Seq.filter string_not_empty
    |> Seq.map String.to_seq
    |> Seq.mapi (fun y chars -> chars |> Seq.mapi (fun x char -> (x, y), char))
    |> Seq.concat
    |> Seq.fold_left
         (fun chmap (pos, ch) -> Char_map.update ch (Pos_set.create_or_add pos) chmap)
         Char_map.empty
  in
  count_xmas chmap |> Format.printf "Result: %d\n"
;;

let%expect_test _ =
  solve
    {|
     MMMSXXMASM
     MSAMXMSMSA
     AMXSXMAAMM
     MSAMASMSMX
     XMASAMXAMM
     XXAMMXXAMA
     SMSMSASXSS
     SAXAMASAAA
     MAMMMXMMMM
     MXMXAXMASX
     |};
  [%expect "Result: 18"]
;;

let read_file file = In_channel.with_open_bin file In_channel.input_all
let main () = read_file "./input/day04.txt" |> solve
