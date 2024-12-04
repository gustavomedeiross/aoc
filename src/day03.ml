open Angstrom

type instruction =
  | Mul of int * int
  | Do
  | Dont

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let sep_once s p = p >>= fun fst -> s *> p >>= fun snd -> return (fst, snd)
let integer = take_while1 is_digit >>| int_of_string

let mul =
  string "mul(" *> sep_once (char ',') integer <* string ")" >>| fun (x, y) -> Mul (x, y)
;;

let instruction = mul <|> string "don't" *> return Dont <|> string "do" *> return Do

let parser : instruction list t =
  fix (fun _ ->
    many (instruction >>| (fun x -> Some x) <|> any_char *> return None)
    >>| List.filter_map Fun.id)
;;

let interp instructions =
  let sum, _ =
    List.fold_left
      (fun (sum, enabled) instr ->
        match instr with
        | Do -> sum, true
        | Dont -> sum, false
        | Mul (x, y) -> if enabled then sum + (x * y), enabled else sum, enabled)
      (0, true)
      instructions
  in
  sum
;;

let solve input =
  match parse_string ~consume:All parser input with
  | Ok instructions -> instructions |> interp |> Format.printf "Part 2: %d\n"
  | Error msg -> failwith msg
;;

let%expect_test _ =
  solve "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";
  [%expect {|
            Part 1: 161
            Part 2: 48
            |}]
;;

let read_file file = In_channel.with_open_bin file In_channel.input_all
let main () = read_file "./input/day03.txt" |> solve
