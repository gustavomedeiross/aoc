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

  let safe_adj i = i >= 1 && i <= 3

  let rec has_safe_adjacency = function
    | [] | _ :: [] -> true
    | x :: y :: rest ->
      if safe_adj (Int.abs (x - y)) then has_safe_adjacency (y :: rest) else false
  ;;

  let is_safe r = is_increasing_or_decreasing r && has_safe_adjacency r

  (* Part 02 *)

  let rec is_safe_asc_v2 report damps read =
    match report with
    | [] | _ :: [] -> true
    | a :: b :: rest when safe_adj (b - a) -> is_safe_asc_v2 (b :: rest) damps (a :: read)
    | a :: b :: rest when damps > 0 ->
      let can_remove_a =
        match read with
        | [] -> true
        | [ prev ] | prev :: _ -> safe_adj (b - prev)
      in
      if can_remove_a
      then
        is_safe_asc_v2 (b :: rest) (damps - 1) read
        || is_safe_asc_v2 (a :: rest) (damps - 1) read
      else (* remove the current *)
        is_safe_asc_v2 (a :: rest) (damps - 1) read
    | _ -> false
  ;;

  let rec is_safe_desc_v2 report damps read =
    match report with
    | [] | _ :: [] -> true
    | a :: b :: rest when safe_adj (a - b) -> is_safe_desc_v2 (b :: rest) damps (a :: read)
    | a :: b :: rest when damps > 0 ->
      let can_remove_a =
        match read with
        | [] -> true
        | [ prev ] | prev :: _ -> safe_adj (prev - b)
      in
      if can_remove_a
      then
        is_safe_desc_v2 (b :: rest) (damps - 1) read
        || is_safe_desc_v2 (a :: rest) (damps - 1) read
      else (* remove the current *)
        is_safe_desc_v2 (a :: rest) (damps - 1) read
    | _ -> false
  ;;

  (* let rec is_safe_asc_2 report damps dampened = *)
  (*   match report with *)
  (*   | [] | _ :: [] -> true *)
  (*   | a :: b :: c :: rest when safe_adj (b - a) && safe_adj (c - b) -> *)
  (*     is_safe_asc_2 (b :: c :: rest) damps *)
  (*   | a :: _ :: c :: rest when safe_adj (c - a) && damps > 0 -> *)
  (*     is_safe_asc_2 (a :: c :: rest) (damps - 1) *)
  (*   | _ :: b :: c :: rest when safe_adj (c - b) && damps > 0 -> *)
  (*     is_safe_asc_2 (b :: c :: rest) (damps - 1) *)
  (*   | a :: b :: _ :: rest when safe_adj (b - a) && damps > 0 -> *)
  (*     is_safe_asc_2 (a :: b :: rest) (damps - 1) *)
  (*   | a :: b :: rest when safe_adj (b - a) -> is_safe_asc_2 (b :: rest) damps *)
  (*   | a :: _ :: rest when damps > 0 -> is_safe_asc_2 (a :: rest) (damps - 1) *)
  (*   | _ -> false *)
  (* ;; *)

  (* let rec is_safe_desc_2 report damps = *)
  (*   match report with *)
  (*   | [] | _ :: [] -> true *)
  (*   | a :: b :: c :: rest when safe_adj (a - b) && safe_adj (b - c) -> *)
  (*     is_safe_desc_2 (b :: c :: rest) damps *)
  (*   | a :: _ :: c :: rest when safe_adj (a - c) && damps > 0 -> *)
  (*     is_safe_desc_2 (a :: c :: rest) (damps - 1) *)
  (*   | _ :: b :: c :: rest when safe_adj (b - c) && damps > 0 -> *)
  (*     is_safe_desc_2 (b :: c :: rest) (damps - 1) *)
  (*   | a :: b :: _ :: rest when safe_adj (a - b) && damps > 0 -> *)
  (*     is_safe_desc_2 (a :: b :: rest) (damps - 1) *)
  (*   | a :: b :: rest when safe_adj (a - b) -> is_safe_desc_2 (b :: rest) damps *)
  (*   | a :: _ :: rest when damps > 0 -> is_safe_desc_2 (a :: rest) (damps - 1) *)
  (*   | _ -> false *)
  (* ;; *)

  let rec is_safe_asc report damps =
    match report with
    | [] | _ :: [] -> true
    | a :: b :: rest when safe_adj (b - a) -> is_safe_asc (b :: rest) damps
    | a :: _ :: rest when damps >= 1 -> is_safe_asc (a :: rest) (damps - 1)
    | _ -> false
  ;;

  let rec is_safe_desc report damps =
    match report with
    | [] | _ :: [] -> true
    | a :: b :: rest when safe_adj (a - b) -> is_safe_desc (b :: rest) damps
    | a :: _ :: rest when damps >= 1 -> is_safe_desc (a :: rest) (damps - 1)
    | _ -> false
  ;;

  let is_safe_with_damp = function
    | [] | [ _ ] -> true
    | a :: b :: rest ->
      is_safe_asc_v2 (a :: b :: rest) 1 []
      || is_safe_asc_v2 (b :: rest) 0 []
      || is_safe_desc_v2 (a :: b :: rest) 1 []
      || is_safe_desc_v2 (b :: rest) 0 []
  ;;
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

let solve2 input =
  let reports =
    input |> String.split_on_char '\n' |> remove_empty_lines |> List.map Report.parse
  in
  (* let open Sexplib.Std in *)
  (* reports *)
  (* |> List.filter (not % Report.is_safe_with_damp) *)
  (* |> [%sexp_of: Report.t list] *)
  (* |> Sexplib.Sexp.to_string *)
  (* |> Format.printf "Unsafe reports: %s"; *)
  reports
  |> List.filter Report.is_safe_with_damp
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

let%expect_test _ =
  solve2
    {|
     7 6 4 2 1
     1 2 7 8 9
     9 7 6 2 1
     1 3 2 4 5
     8 6 4 4 1
     1 3 6 7 9
     |};
  [%expect "Safe reports: 4"]
;;

let%expect_test _ =
  solve2
    {|
     48 46 47 49 51 54 56
     1 1 2 3 4 5
     1 2 3 4 5 5
     5 1 2 3 4 5
     1 4 3 2 1
     1 6 7 8 9
     1 2 3 4 3
     9 8 7 6 7
     7 10 8 10 11
     29 28 27 25 26 25 22 20
     14 16 18 19 21 24 22 27
     |};
  [%expect "Safe reports: 11"]
;;

let read_file file = In_channel.with_open_bin file In_channel.input_all
let main () = read_file "./input/day02.txt" |> solve2
