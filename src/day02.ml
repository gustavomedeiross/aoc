let ( % ) f g x = f (g x)

let is_string_empty = function
  | "" -> true
  | _ -> false
;;

let remove_empty_lines =
  List.filter (fun line -> line |> String.trim |> is_string_empty |> not)
;;

module Report = struct
  open Sexplib.Std

  type t = int list [@@deriving sexp]

  let parse line : t =
    line
    |> String.trim
    |> String.split_on_char ' '
    |> List.filter (not % is_string_empty)
    |> List.map int_of_string
  ;;

  let sort = List.sort Int.compare

  let is_increasing_or_decreasing report =
    let sorted = sort report in
    let is_inc = List.equal Int.equal sorted report in
    let is_dec = List.equal Int.equal (sorted |> List.rev) report in
    is_inc || is_dec
  ;;

  let in_range i n p = i >= n && i <= p

  let rec has_safe_adjacency = function
    | [] | _ :: [] -> true
    | x :: y :: rest ->
      if in_range (Int.abs (x - y)) 1 3
      then true && has_safe_adjacency (y :: rest)
      else false
  ;;

  let is_safe r = is_increasing_or_decreasing r && has_safe_adjacency r
end

let solve input =
  input
  |> String.split_on_char '\n'
  |> remove_empty_lines
  |> List.map Report.parse
  |> List.filter Report.is_safe
  |> List.fold_left (fun total _ -> total + 1) 0
  |> Format.printf "Safe reports: %i\n"
;;

let%expect_test _ =
  solve
    {|
     7 6 4 2 1
     1 2 7 8 9
     9 7 6 2 1
     1 3 2 4 5
     8 6 4 4 1
     1 3 6 7 9
     |};
  [%expect "Safe reports: 2"]
;;

let read_file file = In_channel.with_open_bin file In_channel.input_all
let main () = read_file "./input/day02.txt" |> solve
