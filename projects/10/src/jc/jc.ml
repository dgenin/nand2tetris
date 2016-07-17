
(* Read in the program text from stdin *)
let read_prog ch =
  let s = ref "" in
  try
    while true do
      s := (!s ^ (input_line ch))
    done;
    ""
  with
    End_of_file -> !s;;

let prog =
  match Sys.argv with
    [|_;infile|] ->
    begin
      try
        let ch = open_in infile in
        let prog = (read_prog ch) in
        close_in ch;
        prog
      with
        e -> print_string "error occured"; print_endline (Printexc.to_string e); exit 1
    end
  | _ -> raise (Invalid_argument ("Usage: " ^ Sys.argv.(0) ^ " infile"))
in

let l = new Lexer.lexeme_list prog in

(* Parse program and print the resulting token list *)
let dclasses = Parser.scanner l in
  List.iter (fun c -> Parser.declaration_print c; print_newline ()) dclasses;;