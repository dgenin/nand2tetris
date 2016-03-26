
(* Read in the program text from stdin *)
let rec read_prog s =
  try
    let l = (s ^ (input_line stdin)) in read_prog l
  with
    End_of_file -> s;;

let prog = (read_prog "") in
let l = Lexer.init_lex prog in
(* in declaration_print (parse l);; *)
(* let state_machine = [[(Lsymbol("{"), 1)]; [(Lsymbol("}"),2); (Lany,1)]] in *)

(* Parse program and print the resulting token list *)
let token_list = Parser.scanner l in
  List.map Lexer.lexeme_print token_list