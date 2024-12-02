let ( % ) f g x = f (g x)

let is_string_empty = function
  | "" -> true
  | _ -> false
;;

let remove_empty_lines =
  List.filter (fun line -> line |> String.trim |> is_string_empty |> not)
;;

let unzip l =
  let f (xs, ys) (x, y) = x :: xs, y :: ys in
  List.fold_left f ([], []) (List.rev l)
;;

(** Part 1 *)

let solve input =
  let location_ids =
    input
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> remove_empty_lines
    |> List.map (fun line ->
      match line |> String.split_on_char ' ' |> List.filter (not % is_string_empty) with
      | [ a; b ] -> int_of_string a, int_of_string b
      | _ -> raise (Invalid_argument "Malformed line"))
  in
  let xs, ys = location_ids |> unzip in
  let xs, ys = List.sort Int.compare xs, List.sort Int.compare ys in
  List.combine xs ys
  |> List.map (fun (x, y) -> Int.abs (x - y))
  |> List.fold_left ( + ) 0
  |> Format.printf "%i\n"
;;

let%expect_test _ =
  solve {|
     3   4
     4   3
     2   5
     1   3
     3   9
     3   3
     |};
  [%expect "11"]
;;

(** Part 2 *)

module IntMap = Map.Make (Int)

let solve2 input =
  let location_ids =
    input
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> remove_empty_lines
    |> List.map (fun line ->
      match line |> String.split_on_char ' ' |> List.filter (not % is_string_empty) with
      | [ a; b ] -> int_of_string a, int_of_string b
      | _ -> raise (Invalid_argument "Malformed line"))
  in
  let xs, _ys = location_ids |> unzip in
  (* fst is how many times it appears on the left, snd is on the right list *)
  let map = xs |> List.map (fun x -> x, (0, 0)) |> IntMap.of_list in
  let map =
    List.fold_left
      (fun map (x, y) ->
        map
        |> IntMap.update y (function
          | None -> None
          | Some (x, y) -> Some (x, y + 1))
        |> IntMap.update x (function
          | None -> None
          | Some (x, y) -> Some (x + 1, y)))
      map
      location_ids
  in
  let result =
    IntMap.fold (fun location (xs, ys) total -> (location * xs * ys) + total) map 0
  in
  Format.printf "%i\n" result
;;

let%expect_test _ =
  solve2 {|
     3   4
     4   3
     2   5
     1   3
     3   9
     3   3
     |};
  [%expect "31"]
;;

let read_file file = In_channel.with_open_bin file In_channel.input_all
let main () = read_file "./input/day01.txt" |> solve2
