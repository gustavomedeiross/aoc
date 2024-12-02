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

let read_file file = In_channel.with_open_bin file In_channel.input_all
let main () = read_file "./input/day01.txt" |> solve
