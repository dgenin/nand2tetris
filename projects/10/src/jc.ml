
(* Read in the program text from stdin *)
let read_prog ch =
  let s = ref "" in
  try
    while true do
      (* need to preserve newline for  comments: // *)
      s := (!s ^ (input_line ch) ^ "\n")
    done;
    ""
  with
    End_of_file -> !s;;

(*let prog =
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
in*)

let parse_file infile =
  let ch = open_in infile in
  let prog = (read_prog ch) in
  let l = new Lexer.lexeme_list prog in
  (* Parse program and print the resulting token list *)
  let dclasses = Parser.scanner l in
  List.iter (fun c -> print_endline (Yojson.Safe.prettify (Parser.string_of_declaration c)); ()) dclasses;;

(*Symtab.test ();;*)
(*Vmwriter.test ();;*)
(*Compeng.test ();;*)

let main =
  begin
    let speclist = [("-p", Arg.String (parse_file), "Parse file");
    ("-test_symtab", Arg.Unit (Symtab.test), "Test symtab module");
    ("-test_vmwriter", Arg.Unit (Vmwriter.test), "Test vmwriter module");
    ("-test_compeng", Arg.Unit (Compeng.test ()), "Test compeng module");
    ] in
    let usage_msg = "jc jack compiler" in
    Arg.parse speclist parse_file usage_msg;
  end

let () = main