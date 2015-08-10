
type lexeme = Lint of int
	    | Lkeyword of string
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
  | Lsymbol s -> print_string ("<symbol>" ^  s ^ "</symbol>\n")
  | Lident s -> print_string ("<identifier>" ^ s ^ "</identifier>\n")
  | Lkeyword s -> print_string ("<keyword>" ^ s ^ "</keyword>\n")
  | Lend -> print_string "end\n";;

type unr_op = UMINUS | NOT  ;;
type bin_op = PLUS | MINUS | MULT | DIV
             | EQUAL | LESS | GREAT | DIFF
             | AND | OR  ;;
type brace = LBRACE | RBRACE;;
type brack = LBRACK | RBRACK ;;
type paren = LPAREN | RPAREN ;;
type const = TRUE | FALSE | NULL | THIS ;;

type identifier = string;;

type var_type = INT | CHAR | BOOLEAN | VOID | CLASS of identifier;;
type var_scope = STATIC | FIELD;;
type sub_type = CONSTRUCTOR | FUNCTION | METHOD;;

type expression =
  | Eint_const of int
  | Estr_const of string
  | Ekwd_const of const
  | Eunr_exp of unr_op * expression
  | Ebin_exp of expression * bin_op * expression
  | Evar of identifier
  | Earray_elem of identifier * int;;
(*  | Efunc_call of identifier * expression list;;*)

type ret_val = VAL of expression | None;;

type statement =
  | Slist of statement list
  | Slet of identifier * expression
  | Sif of expression * statement list * statement list
  | Swhile of expression * statement list
  | Sdo of identifier * expression list
  | Sreturn of ret_val;;

type declaration =
  | Dclass of identifier * declaration list * declaration list
  | Dclass_var of var_scope * var_type * identifier
  | Dsub of sub_type * var_type * identifier * identifier list * declaration list * statement list
  | Dsub_var of var_type * identifier list;;

let declaration_print decl =
  match decl with
  | Dclass (class_name, class_vars, class_subs) -> print_string class_name
  | Dclass_var (var_scope, var_type, var_name) -> print_string var_name
  | _ -> print_string "whatever";;


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
			| "while" | "return" -> Lkeyword ident
			| _ -> Lident ident)
     | '0'..'9' -> Lint (extract_int cl)
     | '"'      -> forward cl ;
                   let res = Lstring (extract (fun st -> st.[0] <> '"') cl)
                   in forward cl ; res
     | '+' | '-' | '*' | '&' | '|' | '!' | '='
     | '(' | ')' | ',' | ';' | '{' | '}' | '.'
     | '[' | ']' | '<' | '>' ->
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
			| _          -> Lsymbol (String.make 1 c)
		   )
     | _ -> raise LexerError
   in
      if cl.current >= cl.size then Lend
      else lexer_char cl.string.[cl.current] ;;

let rec read_prog s =
  try
    let l = (s ^ (input_line stdin)) in read_prog l
  with
    End_of_file -> s;;

let rec lex_all cl =
  let lm = lexer cl in
  match lm with
    Lend -> None
  | _    -> lexeme_print lm; lex_all cl;;

(*let prog = (read_prog "")
in let l = init_lex prog
in lex_all l;;*)

exception ParserError of string;;
let rec parse_class_subs prog = [];;

let get_type token =
  match token with
    Lkeyword "int" -> INT |
    Lkeyword "char" -> CHAR |
    Lkeyword "boolean" -> BOOLEAN |
    Lident _ -> CLASS "test"
    | _ -> raise (ParserError "");;

let rec parse_class_vars prog var_defs =
  let get_scope token = match token with Lkeyword "static" -> STATIC | Lkeyword "field" -> FIELD | _ -> raise (ParserError "")
  in let rec get_name prog names = match (lexer prog) with
	Lident name -> get_name prog (name::names)
      | Lsymbol "," -> get_name prog names
      | Lsymbol ";" -> names
      | _ -> raise (ParserError "")
  in
  match (lexer prog) with
    ((Lkeyword "static") as t) | ((Lkeyword "field") as t) -> let vscope = get_scope t and vtype = get_type (lexer prog) in parse_class_vars prog (List.concat [var_defs;(List.map (fun name -> Dclass_var (vscope, vtype, name)) (get_name prog []))])
    | Lkeyword _ -> var_defs
    | _ -> raise (ParserError "");;

let parse_class prog =
  match (lexer prog) with
    Lident class_name -> (match (lexer prog) with
			    Lsymbol "{" -> Dclass (class_name, (parse_class_vars prog []), (parse_class_subs prog))
			  | _ -> raise (ParserError ""))
  | _ -> raise (ParserError "");;


let rec parse prog =
  let token = lexer prog in
  lexeme_print token;
  match token with
    Lkeyword kwd -> (match kwd with
		      "class" -> parse_class prog
		    | _ -> raise (ParserError "Unexpected keyword at top level"))
  | Lcomment _ -> parse prog
  | _ -> raise (ParserError "Unexpected statement at top level");;

let prog = (read_prog "")
in let l = init_lex prog
in declaration_print (parse l);;
