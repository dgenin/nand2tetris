(* Lexeme type *)
type lexeme = Lint of int
	    | Lkeyword of string
	    | Lident of string
	    | Lsymbol of string
	    | Lstring of string
	    | Lcomment of string
	    | Lend
	    | Lany;;

type string_lexer = {string:string; mutable current:int; size:int } ;;

let lexeme_trans l = match l with
    (Lident _) -> (Lident "*")
  | t -> t;;


let lexeme_print l = match l with
    Lint i -> print_string ("<int>" ^ (string_of_int i) ^ "</int> ")
  | Lstring s -> print_string ("<string>" ^ s ^ "</string> ")
  | Lcomment s -> print_string ("<comment>" ^ s ^ "</comment> ")
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

(*** Recursive lexer ***)
 type transition = lexeme * int;;
 type state = transition list;;
 type state_machine = state list;;

let rec print_state s =
  match s with
    (l,i)::res -> lexeme_print l; print_int i; print_string " + "; print_state res
  | [] -> print_string "+++\n";;

let print_bool b = match b with true -> print_string "true\n" | false -> print_string "false\n";;

let scanner sm cl =
  let next_lexeme() = lexer cl in
  (* get_next_state *)
  let get_next_state transitions lexeme =
    let lexeme = lexeme_trans lexeme in
    (* transition_match *)
    let transition_match lexeme1 transition =
      (* lexeme_eq *)
      let lexeme_eq lexeme2 trans_lex =
	       match trans_lex with
	         Lany -> true
	       | l -> (*print_string "lexeme_eq: ";
	         lexeme_print l;
	         lexeme_print lexeme;
	         print_bool (l = lexeme); *)
	         l = lexeme2 in
      (* lexeme_eq *)
      match transition with (l,i) ->
			 (* print_string "transition_match: ";
			 lexeme_print lexeme;
			 lexeme_print l;
			 print_bool (lexeme_eq lexeme l); *)
			 lexeme_eq lexeme l in
       (* transition_match *)
    (* get_next_state body *)
    let ns = List.filter (transition_match lexeme) transitions in
    print_string "ns = "; print_state ns;
    (* TODO: get rid of Lany eventually *)
    assert ((List.length ns) <= 1 || (match (List.nth ns 1) with (l,i) -> l = Lany) );
    match (List.length ns) with
      0 -> -1
    | _ -> match (List.nth ns 0) with (l, i) -> i in
    (* end of get_next_state body *)
  let rec rec_scanner sm state tokens =
    let lexeme = next_lexeme() in
    let next_state = get_next_state state lexeme in print_string "next_state = "; print_int next_state; print_string "\n";
						    (*print_string "next_state = "; print_int next_state; print_string "\n";*)
						    match next_state with
						      -1 -> raise LexerError
						    | n when n = (List.length sm) -> tokens
						    | n -> rec_scanner sm (List.nth sm n) (List.append tokens [lexeme]) in
  rec_scanner sm [(Lsymbol("{"), 1)] [];;

exception ParserError of string;;
(* Parse the class subroutines *)


(* Print state *)
let print_state state =
  None;;

(* Extracts a code block delimited by {}, which may contain other blocks *)
let extract_code_block cl =
  let st = cl.string in 
  let rec rec_extract_block n =
    let rec ext n =
      if n < cl.size then
	match (String.get st n) with 
	  '{' -> ext (rec_extract_block n)
	| '}' -> n + 1
	| _ -> ext (n + 1)
      else raise (ParserError "Unmatched \"{\"") in
    match (String.get st n) with
      '{' -> ext (n + 1)
    | _  -> raise (ParserError "extract_block called on buffer not starting with \"{\"") in
  init_lex (String.sub st cl.current (rec_extract_block cl.current));;

(* Same as above but uses lexer to walk the program text.
   Looks uglier because it relies on side-effect from lexer operation *)
(*let extract_code_block cl =
  let pos = cl.current in
  let rec rec_extract_block =
    let rec ext = 
      match (lexer cl) with
	Lsymbol "{" -> rec_extract_block; ext
      | Lsymbol "}" -> cl.current
      | _ -> ext in
    match (lexer cl) with
      Lsymbol "{" -> ext
    | _ -> raise (ParserError "extract_block called on buffer not starting with \"{\"") in
  rec_extract_block pos;
  init_lex(String.sub cl.string pos cl.current);
  cl.current <- pos;;*)
      

(* Extract class vars block *)
(*let extract_class_vars cl =
  let st = cl.string in
  let rec rec_extract_class_vars n =
    let rec ext tokens =
      match (lexer cl) with
	Lkeyword "field" | "static" | "int" | "char" | "boolean" as t -> ext tokens::t
	| Lidentifier -> ext tokens::t
	| _ -> tokens dummy, fix it *);; 

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
  | Dsub of sub_type * var_type * identifier * declaration list * declaration list * statement list
  | Dsub_var of var_type * identifier
  | Dsub_param of var_type * identifier
  | Dempty;;


let type_to_string t =
  match t with
    INT -> "int"
  | CHAR -> "char"
  | BOOLEAN -> "boolean"
  | VOID -> "void"
  | CLASS c -> "class " ^ c;;

let sub_type_to_string t =
  match t with
    FUNCTION -> "function"
  | METHOD -> "method"
  | CONSTRUCTOR -> "constructor";;

let scope_to_string scope =
  match scope with
    | STATIC -> "static"
    | FIELD -> "field";;

let rec declaration_print decl =
  match decl with
  | Dclass (class_name, class_vars, class_subs) ->
     print_string ("Class: " ^ class_name ^ "\n");
     print_string "Class variables: \n";
     List.map declaration_print class_vars;
     print_string "\nClass subroutines\n";
     List.map declaration_print class_subs;
     print_string ""
  | Dclass_var (var_scope, var_type, var_name) -> print_string (var_name ^ " " ^ (type_to_string var_type) ^ " " ^(scope_to_string var_scope) ^ "\n")
  | Dsub_param (var_type, var_name) -> print_string ((type_to_string var_type) ^ " " ^ var_name ^ " ")
  | Dsub (sub_type, ret_type, sub_name, param_list, var_list, body) ->
     print_string ("Subroutine: " ^ sub_name ^ "\n");
     print_string ("Scope: " ^ (sub_type_to_string sub_type) ^ "\n");
     print_string ("Returns: " ^ (type_to_string ret_type) ^ "\n");
     print_string ("Params: ");
     List.map declaration_print param_list;
     List.map declaration_print var_list;
     print_string "\n"
  | _ -> print_string "whatever";;


(* translate type keyword to type type*)
let get_type token =
  match token with
    Lkeyword "int" -> INT
  | Lkeyword "char" -> CHAR
  | Lkeyword "boolean" -> BOOLEAN
  | Lkeyword "void" -> VOID
  | Lident _ -> CLASS "test"
  | _ -> lexeme_print token; raise (ParserError "Invalid type");;

let get_ident token =
  match token with
    Lident ident -> ident
  | _ as t -> lexeme_print t; raise (ParserError "Invalid identifier");;

(* Parse subroutine params *)
let parse_sub_params prog =
  (* reads subroutine parameters, including the closing parenthesis *)
  let rec get_params prog param_defs =
    match (lexer prog) with
      Lsymbol ")" -> param_defs
    | Lsymbol "," -> get_params prog param_defs
    | Lcomment _ -> raise (ParserError "Comments not supported in parameter declaration lists")
    | Lkeyword _ as t -> let param_type = get_type t in
			 let param_name = get_ident (lexer prog) in
			 match (lexer prog) with
			   Lsymbol ")" -> List.concat [param_defs; [Dsub_param (param_type, param_name)]]
			 | Lsymbol "," -> get_params prog (List.concat [param_defs; [Dsub_param (param_type, param_name)]])
			 | _ as t -> lexeme_print t; raise (ParserError "Invalid token in subroutine parameter declaration")
  in
  match (lexer prog) with
    Lsymbol "(" -> get_params prog []
  | _ as t -> lexeme_print t; raise (ParserError "Missing open parenthesis in subroutine parameter declaration");;

(* Parse subroutine variables *)
let rec parse_sub_vars prog var_defs =
  (* parse the list of variable names *)
  let rec get_name prog names = match (lexer prog) with
	Lident name -> get_name prog (name::names)
      | Lsymbol "," -> get_name prog names
      | Lsymbol ";" -> names
      | _ as t -> lexeme_print t; raise (ParserError "Syntax error in subroutine variable declaration") and
  prev_pos = prog.current in
  print_string "parsing local variables\n";
  match (lexer prog) with
    Lcomment _ -> parse_sub_vars prog var_defs
  | Lkeyword "var" -> let vtype = get_type (lexer prog) in
		      let more_vars = List.map (fun name -> Dsub_var (vtype, name)) (get_name prog [])
		      in print_string "var keyword\n"; parse_sub_vars prog (List.concat [var_defs; more_vars])
  | _ as l -> lexeme_print l; prog.current <- prev_pos; var_defs
;;

(* Parse subroutine statements *)
let rec parse_if_statement prog statements = statements
and parse_let_statement prog statements =
  match (lexer prog) with
    Lcomment _ -> parse_let_statement prog statements
  | Lsymbol ";" -> statements
  | Lend -> raise (ParserError "Reached end of file in let\n")
  | _ -> parse_let_statement prog statements
and parse_while_statement prog statements =
  match (lexer prog) with
    Lcomment _ -> parse_while_statement prog statements
  | Lsymbol "{" -> print_string "while {"; (match (lexer prog) with
		     Lcomment _ -> parse_let_statement prog statements
		   | Lsymbol "}" -> statements
		   | Lend -> raise (ParserError "Reached end of file in let\n")
		   | _ -> parse_sub_statements prog statements)
  | _ -> parse_while_statement prog statements
and  parse_do_statement prog statements = statements
and  parse_return_statement prog statements = statements
and  parse_sub_statements prog statements =
  match (lexer prog) with
    Lcomment _ -> parse_sub_statements prog statements
  | Lkeyword "if" -> print_string "if\n"; parse_sub_statements prog (List.concat [statements; parse_if_statement prog statements])
  | Lkeyword "let" -> print_string "let\n"; parse_sub_statements prog ( List.concat [statements; parse_let_statement prog statements])
  | Lkeyword "while" -> print_string "while\n"; parse_sub_statements prog ( List.concat [statements; parse_while_statement prog statements])
  | Lkeyword "do" -> print_string "do\n"; parse_sub_statements prog ( List.concat [statements; parse_do_statement prog statements])
  | Lkeyword "return" -> print_string "return\n"; parse_sub_statements prog ( List.concat [statements; parse_return_statement prog statements])
  | Lsymbol "}" -> print_string "}\n"; statements
  | Lend -> raise (ParserError "Missing } in function\n")
  | _ as t -> print_string "parse_sub_statements: "; lexeme_print t; []
;;

(* Parses subroutine body *)
let rec parse_sub_body prog =
  match (lexer prog) with
    Lcomment _ -> parse_sub_body prog
  | Lsymbol "{" -> let vars = parse_sub_vars prog [] in
		   let statements = parse_sub_statements prog [] in
		   (vars, statements)
  | _ -> raise (ParserError "Missing { in function body\n")
;;

(* Parse class subroutine definitions *)
let rec parse_class_subs prog =
  match (lexer prog) with
    Lkeyword "function" -> let ret_type = get_type (lexer prog) in
			   let fun_name = get_ident (lexer prog) in
			   let fun_params = parse_sub_params prog in
			   let (fun_vars, fun_statements) = parse_sub_body prog in
			   [Dsub (FUNCTION, ret_type, fun_name, fun_params, fun_vars, [])]
  | Lkeyword "method" -> [Dsub (METHOD, get_type (lexer prog), get_ident (lexer prog), parse_sub_params prog, parse_sub_vars prog [], [])]
  | Lkeyword "constructor" -> let ret_type = get_type (lexer prog) in
			      let con_name = get_ident (lexer prog) in
			      let con_params = parse_sub_params prog in
			      let con_vars = parse_sub_vars prog [] in
			      [Dsub (CONSTRUCTOR, ret_type, con_name, con_params, con_vars, [])]
  | _ -> []
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
			    Lsymbol "{" -> List.map declaration_print (parse_class_vars prog []); Dclass (class_name, (parse_class_vars prog []), (parse_class_subs prog))
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

let prog = (read_prog "") in
let l = init_lex prog in
(* in declaration_print (parse l);; *)
(* let state_machine = [[(Lsymbol("{"), 1)]; [(Lsymbol("}"),2); (Lany,1)]] in *)
let state_machine = [(*0*) [(Lsymbol("{"), 1)]; 
                     (*1*) [(Lkeyword("static"), 2);(Lsymbol("}"), 5); (Lany,1)]; 
                     (*2*) [(Lkeyword("char"), 3); (Lkeyword("int"), 3); (Lkeyword("boolean"), 3); (Lkeyword("void"), 3)];
                     (*3*) [(Lident("*"), 4)];
                     (*4*) [(Lsymbol(","),3); (Lsymbol(";"),1)]
                     (*5*) (*Done*) ]; in
scanner state_machine l
