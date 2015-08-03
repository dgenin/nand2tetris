type lexeme = Lint of int
	    | Lident of string
	    | Lsymbol of string
	    | Lstring of string
	    | Lcomment of string
	    | Lend ;;
type string_lexer = {string:string; mutable current:int; size:int } ;;

let lexeme_print l = match l with
    Lint i -> print_string ("<int>" ^ (string_of_int i) ^ "</int>\n")
  | Lstring s -> print_string ("<string>" ^ s ^ "</string>\n")
  | Lcomment s -> print_string ("<comment>" ^ s ^ "</comment>\n")
  | Lsymbol s -> print_string ("<symbol>" ^ s ^ "</symbol>\n")
  | Lident s -> print_string ("<identifier>" ^ s ^ "</identifier>\n")
  | Lend -> print_string "end\n";;
			      
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
									
(* let l = init_lex "123324" in
    print_int (extract_int l) ;;

let l = init_lex "abac" in
    print_string (extract_ident l) ;;
 *)

(* Check that this is right *)
exception LexerError ;;
 let  rec lexer cl = 
   let lexer_char c = match c with 
       ' ' 
     | '\n' 
     | '\t'     -> forward cl ; lexer cl 
     | 'a'..'z' 
     | 'A'..'Z' -> Lident (extract_ident cl)
     | '0'..'9' -> Lint (extract_int cl)
     | '"'      -> forward cl ; 
                   let res = Lstring (extract (fun st -> st.[0] <> '"') cl) 
                   in forward cl ; res 
     | '+' | '-' | '*' | '&' | '|' | '!' | '=' | '(' | ')'  -> 
                   forward cl; Lsymbol (String.make 1 c)
     | '<' 
     | '>'      -> forward cl; 
                   if cl.current >= cl.size then Lsymbol (String.make 1 c)
                   else  let cs = cl.string.[cl.current] 
                         in ( match (c,cs) with 
                                ('<','=') -> forward cl; Lsymbol "<=" 
                              | ('>','=') -> forward cl; Lsymbol ">="
                              | ('<','>') -> forward cl; Lsymbol "<>"
                              |     _     -> Lsymbol (String.make 1 c) )
     | '/'      -> forward cl;
		   if cl.current >= cl.size then Lsymbol (String.make 1 c)
		   else (
		     let cs = cl.string.[cl.current]
		     in match (c, cs) with
			| ('/', '/') -> forward cl;
					Lcomment (extract (fun st -> st.[0] <> '\n') cl)
			| ('/', '*') -> forward_n cl 2;
				       let res = (Lcomment (extract (fun st -> (String.compare (String.sub st 0 2) "*/") != 0) cl))
				       in forward_n cl 2; res
			| _          -> Lsymbol (String.make 1 c)
		   )
     | _ -> raise LexerError
   in 
      if cl.current >= cl.size then Lend 
      else lexer_char cl.string.[cl.current] ;; 

let rec read_prog s =
  try
    read_prog (s ^ input_line stdin)
  with
    End_of_file -> print_string s;
  s;;

let prog = read_prog ""
in let l = init_lex prog in
lexer l;;
