module T = ANSITerminal
open Life.Main

let common k rule =
  T.erase T.Screen;
  T.save_cursor ();
  Random.self_init ();
  let w = loop init_w k rule in
  display w;
  ignore (read_line ());
  T.restore_cursor ();
  print_newline ()

let _ =
  let length = Array.length Sys.argv in
  match length with
  | 3 ->
    let k = int_of_string Sys.argv.(2) in
    let rule = parse Sys.argv.(1) in
    common k rule
  | x when x >= 3 ->
    let k = int_of_string Sys.argv.(length - 1) in
    let args_array = Array.sub Sys.argv 1 (length - 2) in
    let args_list = Array.to_list args_array in
    let rule = parse (String.concat " " args_list) in
    common k rule
  | _ -> failwith "Usage: dune exec S/B life n_rounds"


