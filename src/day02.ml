let ( % ) f g x = f (g x)

let is_string_empty = function
  | "" -> true
  | _ -> false
;;

let remove_empty_lines = List.filter (not % is_string_empty % String.trim)

module Report = struct
  open Sexplib.Std

  type t = int list [@@deriving sexp]

  type order =
    | Asc
    | Desc

  let parse line : t =
    line
    |> String.trim
    |> String.split_on_char ' '
    |> List.filter (not % is_string_empty)
    |> List.map int_of_string
  ;;

  let safe_diff ord (a, b) =
    let n =
      match ord with
      | Asc -> b - a
      | Desc -> a - b
    in
    n >= 1 && n <= 3
  ;;

  let rec is_safe' ord damps report read =
    match report with
    | [] | _ :: [] -> true
    | a :: b :: tail when safe_diff ord (a, b) ->
      is_safe' ord damps (b :: tail) (a :: read)
    | a :: b :: tail when damps > 0 ->
      let continue_with n = is_safe' ord (damps - 1) (n :: tail) read in
      let can_rm_a =
        match read with
        | [] -> true
        | [ last ] | last :: _ -> safe_diff ord (last, b)
      in
      continue_with a || (can_rm_a && continue_with b)
    | _ -> false
  ;;

  let is_safe damps = function
    | [] | [ _ ] -> true
    | report -> is_safe' Asc damps report [] || is_safe' Desc damps report []
  ;;
end

let solve input =
  let reports =
    input |> String.split_on_char '\n' |> remove_empty_lines |> List.map Report.parse
  in
  reports
  |> List.filter (Report.is_safe 0)
  |> List.fold_left (fun total _ -> total + 1) 0
  |> Format.printf "Part 1: %i\n";
  reports
  |> List.filter (Report.is_safe 1)
  |> List.fold_left (fun total _ -> total + 1) 0
  |> Format.printf "Part 2: %i\n"
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
  [%expect {|
            Part 1: 2
            Part 2: 4
            |}]
;;

let%expect_test _ =
  solve
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
  [%expect {|
            Part 1: 0
            Part 2: 11
            |}]
;;

let read_file file = In_channel.with_open_bin file In_channel.input_all
let main () = read_file "./input/day02.txt" |> solve
