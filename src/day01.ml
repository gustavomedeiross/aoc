let%expect_test _ =
  Format.printf "%d" (1 + 2);
  [%expect {|3|}]
;;

let main () = print_endline "Hello, World!"
