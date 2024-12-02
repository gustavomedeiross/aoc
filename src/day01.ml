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

module IntMap = Map.Make (Int)

let incr = ( + ) 1
let map_fst f (x, y) = f x, y
let map_snd f (x, y) = x, f y

(** Part 1 *)

let solve input =
  let locations =
    input
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> remove_empty_lines
    |> List.map (fun line -> Scanf.sscanf line "%d %d" (fun x y -> x, y))
  in
  let xs, ys = locations |> unzip in
  (* Part 1 *)
  let xs, ys = List.sort Int.compare xs, List.sort Int.compare ys in
  List.combine xs ys
  |> List.map (fun (x, y) -> Int.abs (x - y))
  |> List.fold_left ( + ) 0
  |> Format.printf "Part 1: %i\n";
  (* Part 2 *)
  (* fst is how many times it appears on the left, snd is on the right *)
  let map = xs |> List.map (fun x -> x, (0, 0)) |> IntMap.of_list in
  let count map (x, y) =
    map
    |> IntMap.update x (Option.map (map_fst incr))
    |> IntMap.update y (Option.map (map_snd incr))
  in
  let map = List.fold_left count map locations in
  0
  |> IntMap.fold (fun loc (xs, ys) total -> (loc * xs * ys) + total) map
  |> Format.printf "Part 2: %i\n"
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
  [%expect {|
     Part 1: 11
     Part 2: 31
  |}]
;;

let read_file file = In_channel.with_open_bin file In_channel.input_all
let main () = read_file "./input/day01.txt" |> solve
