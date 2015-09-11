(* Lexeme type *)
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

(* Read in the program text from stdin *)
let rec read_prog s =
  try
    let l = (s ^ (input_line stdin)) in read_prog l
  with
    End_of_file -> s;;

(* Lex the program *)
let rec lex_all cl =
  let lm = lexer cl in
  match lm with
    Lend -> None
  | _    -> lexeme_print lm; lex_all cl;;

(* Test lexer

let prog = (read_prog "")
in let l = init_lex prog
in lex_all l;;*)

(*** Parser ***)

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
(* subroutine type, return type, subroutine name, parameter list, local variables, body *)
  | Dsub of sub_type * var_type * identifier * identifier list * declaration list * statement list
  | Dsub_var of var_type * identifier list;;

let type_to_string t =
  match t with
    | INT -> "int"
    | CHAR -> "char"
    | BOOLEAN -> "boolean"
    | VOID -> "void"
    | CLASS c -> "class " ^ c;;

let scope_to_string scope =
  match scope with
    | STATIC -> "static"
    | FIELD -> "field";;

let rec declaration_print decl =
  match decl with
  | Dclass (class_name, class_vars, class_subs) -> print_string ("Class: " ^ class_name ^ "\n");
						   print_string "Class variables: \n";
						   List.map declaration_print class_vars;
						   print_string "\nClass subroutines\n";
						   List.map declaration_print class_subs;
						   print_string ""
  | Dclass_var (var_scope, var_type, var_name) -> print_string (var_name ^ " " ^ (type_to_string var_type) ^ " " ^(scope_to_string var_scope) ^ "\n")
  | _ -> print_string "whatever";;

exception ParserError of string;;
(* Parse the class subroutines *)

(* translate type keyword to type type*)
let get_type token =
  match token with
    Lkeyword "int" -> INT |
    Lkeyword "char" -> CHAR |
    Lkeyword "boolean" -> BOOLEAN |
    Lident _ -> CLASS "test"
    | _ -> lexeme_print token; raise (ParserError "Invalid type");;

(* Parse class subroutine definitions *)
let rec parse_class_subs prog =
[Dsub(FUNCTION, INT, "test", [], [], [])]
(*  match (lexer prog) with
  | Lkeyword "function" -> Dsub (FUNCTION, get_type (lexer prog),
  | Lkeyword "method" -> Dsub (METHOD, get_type (lexer prog),
  | Lkeyword "constructor" -> Dsub (CONSTRUCTOR, get_type (lexer prog),*)
;;


(* Parse class variable declarations *)
let rec parse_class_vars prog var_defs =
  (* translate scope keyword into scope type*)
  let get_scope token = match token with
      Lkeyword "static" -> Some STATIC
    | Lkeyword "field" -> Some FIELD
    | _ -> None in
  (* parse the list of variable names *)
  let rec get_name prog names = match (lexer prog) with
	Lident name -> get_name prog (name::names)
      | Lsymbol "," -> get_name prog names
      | Lsymbol ";" -> names
      | _ -> raise (ParserError "Syntax error in class variable declaration") and
  (* save current lexer position in case we have to back track *)
  prev_pos = prog.current in
  match (lexer prog) with
    Lcomment _ -> parse_class_vars prog var_defs
  | _ as t -> match (get_scope t) with
	   (* if the token is a scope definition this is a variable declaration *)
	   | Some (STATIC | FIELD as vscope) -> let vtype = get_type (lexer prog) in
						let more_vars = List.map (fun name -> Dclass_var (vscope, vtype, name)) (get_name prog [])
						in parse_class_vars prog (List.concat [var_defs; more_vars])
	   (* otherwise, we are done with class variable declaration block, so rewind the lexer for the token to be read again *)
	   | None -> prog.current <- prev_pos; var_defs;;

(* Parse class definition *)
let parse_class prog =
  match (lexer prog) with
    Lident class_name -> (match (lexer prog) with
			    Lsymbol "{" -> Dclass (class_name, (parse_class_vars prog []), (parse_class_subs prog))
			  | _ -> raise (ParserError ""))
  | _ -> raise (ParserError "Syntax error in class declaration");;

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
