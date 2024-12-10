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

let string_not_empty = function
  | "" -> false
  | _ -> true
;;

let to_char_list s = s |> String.to_seq |> List.of_seq

(* Part 1 *)
let count_xmas chmap =
  let xmas_exists = word_exists chmap (to_char_list "XMAS") in
  chmap
  |> Char_map.find 'X'
  |> Pos_set.to_list
  |> List.map (fun pos ->
    all_of_direction |> List.filter (xmas_exists pos) |> List.length)
  |> List.fold_left ( + ) 0
;;

(* Part 2 *)

(** Returns all positions that a word is found on a given direction *)
let word_positions chmap dir word =
  let first_letter = String.get word 0 in
  let word_exists pos = word_exists chmap (to_char_list word) pos dir in
  chmap |> Char_map.find first_letter |> Pos_set.filter word_exists
;;

(** Returns all positions that a word is found on a given direction,
    including if the word is potentially reversed *)
let word_positions_rev chmap dir word =
  let rev_word = word |> to_char_list |> List.rev |> List.to_seq |> String.of_seq in
  let word_positions = word_positions chmap dir in
  Pos_set.union (word_positions word) (word_positions rev_word)
;;

let count_x_mas chmap =
  let left_crosses = word_positions_rev chmap SE "MAS" in
  let right_crosses = word_positions_rev chmap SW "MAS" in
  (* We're going to shift all left crosses 2 positions to the right, to see whether it will match a right cross *)
  left_crosses
  |> Pos_set.map (fun (x, y) -> x + 2, y)
  |> Pos_set.inter right_crosses
  |> Pos_set.cardinal
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
  chmap |> count_xmas |> Format.printf "Part 1: %d\n";
  chmap |> count_x_mas |> Format.printf "Part 2: %d\n"
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
  [%expect {|
            Part 1: 18
            Part 2: 9
            |}]
;;

let read_file file = In_channel.with_open_bin file In_channel.input_all
let main () = read_file "./input/day04.txt" |> solve
