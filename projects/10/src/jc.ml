
(* Read in the program text from stdin *)
let rec read_prog s =
  try
    let l = (s ^ (input_line stdin)) in read_prog l
  with
    End_of_file -> s;;

let prog = (read_prog "") in
let l = new Lexer.lexeme_list prog in
(* List.map Lexer.lexeme_print l#get_list; *)
(* in declaration_print (parse l);; *)
(* let state_machine = [[(Lsymbol("{"), 1)]; [(Lsymbol("}"),2); (Lany,1)]] in *)

(* Parse program and print the resulting token list *)
let dclass = Parser.scanner l in
  Parser.declaration_print dclass;;
