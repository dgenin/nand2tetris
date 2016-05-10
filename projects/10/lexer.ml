(* Lexeme type *)
type lexeme = Lint of int
        | Lkeyword of string
        | Lident of string
        | Lsymbol of string
        | Lop of char
        | Lstring of string
        | Lcomment of string
        | Lend
        | Lany;;

type string_lexer = {string:string; mutable current:int; size:int } ;;


let lexeme_print l = match l with
    Lint i -> print_string ("<int>" ^ (string_of_int i) ^ "</int> ")
  | Lstring s -> print_string ("<string>" ^ s ^ "</string> ")
  | Lcomment s -> print_string ("<comment>" ^ s ^ "</comment> ")
  | Lop c -> print_string "<op>"; print_char c; print_string "</op>"
  | Lsymbol s -> print_string ("<symbol>" ^  s ^ "</symbol> ")
  | Lident s -> print_string ("<identifier>" ^ s ^ "</identifier> ")
  | Lkeyword s -> print_string ("<keyword>" ^ s ^ "</keyword> ")
  | Lend -> print_string "end "
  | Lany -> print_string "any ";;

let init_lex s = { string=s; current=0; size=String.length s } ;;
let forward cl = cl.current <- cl.current + 1 ;;
let forward_n cl n = cl.current <- cl.current + n ;;
let extract pred cl =
  let st = cl.string and pos = cl.current in
  let rec ext n = if n < cl.size && (pred (String.sub st n (cl.size - n))) then ext (n + 1) else n in
  let res = ext pos
  in cl.current <- res ; String.sub cl.string pos (res - pos) ;;

let extract_int =
  let is_int = function st -> match st.[0] with '0'..'9' -> true | _ -> false
  in function cl -> int_of_string (extract is_int cl) ;;
let extract_ident =
  let is_alpha_num = function st -> match st.[0] with
      'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
      | _ -> false
  in extract is_alpha_num ;;

exception LexerError ;;
 let rec lexer cl =
   let lexer_char c = match c with
       ' '
     | '\n'
     | '\r'
     | '\t'     -> forward cl ; lexer cl
     | 'a'..'z'
     | 'A'..'Z' -> let ident = (extract_ident cl)
           in (match ident with
            "class" | "constructor" | "function"
            | "method" | "field" | "static"
            | "int" | "char" | "boolean"
            | "void" | "true" | "false"
            | "null" | "this" | "if " | "else"
            | "while" | "return" | "var" | "let"
            | "do" -> Lkeyword ident
            | _ -> Lident ident)
     | '0'..'9' -> Lint (extract_int cl)
     | '"'      -> forward cl ;
           let res = Lstring (extract (fun st -> st.[0] <> '"') cl)
           in forward cl ; res
     | '+' | '-' | '*' | '&' | '|' | '=' | '<' | '>' ->
           forward cl; Lop c
     | '(' | ')' | ',' | ';' | '{' | '}' | '.' | '~'
     | '[' | ']'  ->
           forward cl; Lsymbol (String.make 1 c)
     | '/'      -> forward cl;
           if cl.current >= cl.size then Lsymbol (String.make 1 c)
           else (
             let cs = cl.string.[cl.current]
             in match (c, cs) with
            | ('/', '/') -> forward cl;
                    Lcomment (extract (fun st -> st.[0] <> '\r') cl)
            | ('/', '*') -> forward_n cl 2;
                       let res = (Lcomment (extract (fun st -> (String.compare (String.sub st 0 2) "*/") != 0) cl))
                       in forward_n cl 2; res
            | _          -> Lop c
           )
     | _ -> raise LexerError
   in
      if cl.current >= cl.size then Lend
      else lexer_char cl.string.[cl.current] ;;

(* Lex the program *)
let lex_all prog =
  let cl = init_lex prog in
    let rec rec_lex_all cl lex_list =
      let lm = lexer cl in
        match lm with
          Lend -> lex_list
        | _  -> rec_lex_all cl (lex_list@[lm]) in
  rec_lex_all cl [];;

class lexeme_list prog =
    let lexer_list = lex_all prog in
    object (s)
      val mutable index = 0
      val lex_list = lexer_list
      val lex_list_len = List.length lexer_list
      method peek = if index < lex_list_len then List.nth lex_list index else Lend
      method next = let v = List.nth lex_list index in index <- index + 1;
        match v with
        | Lcomment l -> s#next
        | _ -> (v, s#peek)
      method get_list = lex_list
end;;