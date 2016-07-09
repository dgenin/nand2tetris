open Lexer

(*** Parser ***)

type unr_op = UMINUS | NOT  ;;
type bin_op = PLUS | MINUS | MULT | DIV |
              EQUAL | LESS | GREAT | DIFF |
              AND | OR  ;;
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
     print_endline ("Class: " ^ class_name );
     print_endline "Class variables: ";
     List.iter declaration_print class_vars;
     print_endline "\nClass subroutines";
     List.iter declaration_print class_subs;
     print_string ""
  | Dclass_var (var_scope, var_type, var_name) ->
     print_endline (var_name ^ " " ^ (type_to_string var_type) ^ " " ^(scope_to_string var_scope))
  | Dsub_param (var_type, var_name) | Dsub_var (var_type, var_name) ->
     print_string ((type_to_string var_type) ^ " " ^ var_name ^ " ")
  | Dsub (sub_type, ret_type, sub_name, param_list, var_list, body) ->
     print_endline ("Subroutine: " ^ sub_name );
     print_endline ("Scope: " ^ (sub_type_to_string sub_type) );
     print_endline ("Returns: " ^ (type_to_string ret_type) );
     print_string ("Params: ");
     List.iter declaration_print param_list; print_newline ();
     print_string "Vars: ";
     List.iter declaration_print var_list;
     print_string "\n"
  | _ -> print_string "whatever";;

exception ParserError of string;;
(* Parse the class subroutines *)

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
let parse_sub_params cl =
  (* reads subroutine parameters, including the closing parenthesis *)
  let rec get_params cl param_defs =
    match cl#next with
      Lsymbol ")" -> param_defs (* empty param list *)
    | Lcomment _ -> raise (ParserError "Comments not supported in parameter declaration lists")
    | Lkeyword _ as t ->
        let param_type = get_type t and param_name = get_ident cl#next in
          match cl#next with
            Lsymbol ")" -> List.concat [param_defs; [Dsub_param (param_type, param_name)]]
          | Lsymbol "," -> get_params cl (List.concat [param_defs; [Dsub_param (param_type, param_name)]])
          | _ as t -> lexeme_print t; raise (ParserError "Invalid token in subroutine parameter declaration")
  in
    match cl#next with
      Lsymbol "(" -> get_params cl []
    | _ as t -> lexeme_print t; raise (ParserError "Missing open parenthesis in subroutine parameter declaration");;

(* Parse subroutine variables *)
let rec parse_sub_vars cl var_defs =
  (* parse the list of variable names *)
  let rec get_name cl names =
    match cl#next with
      Lident name -> get_name cl (name::names)
    | Lsymbol "," -> get_name cl names
    | Lsymbol ";" -> names
    | _ as t -> lexeme_print t; raise (ParserError "Syntax error in subroutine variable declaration")
  in
  match cl#peek with
    Lkeyword "var" -> cl#advance; let vtype = get_type cl#next in
              let more_vars = List.map (fun name -> Dsub_var (vtype, name)) (get_name cl [])
    in print_endline "var keyword"; parse_sub_vars cl (List.concat [var_defs; more_vars])
  | _ as l -> lexeme_print l; var_defs
;;

(* Parse subroutine statements *)
let rec parse_if_statement cl statements = statements
and parse_let_statement cl statements =
  match cl#next with
    Lcomment _ -> parse_let_statement cl statements
  | Lsymbol ";" -> statements
  | Lend -> raise (ParserError "Reached end of file in let\n")
  | _ -> parse_let_statement cl statements
and parse_while_statement cl statements =
  match cl#next with
    Lcomment _ -> parse_while_statement cl statements
  | Lsymbol "{" -> print_string "while {"; (match cl#next with
             Lcomment _ -> parse_let_statement cl statements
           | Lsymbol "}" -> statements
           | Lend -> raise (ParserError "Reached end of file in let\n")
           | _ -> parse_sub_statements cl statements)
  | _ -> parse_while_statement cl statements
and  parse_do_statement cl statements = statements
and  parse_return_statement cl statements = statements
and  parse_sub_statements cl statements =
  match cl#next with
    Lkeyword "if" -> print_endline "if";
    parse_sub_statements cl (List.concat [statements; parse_if_statement cl statements])
  | Lkeyword "let" -> print_endline "let";
    parse_sub_statements cl ( List.concat [statements; parse_let_statement cl statements])
  | Lkeyword "while" -> print_endline "while";
    parse_sub_statements cl ( List.concat [statements; parse_while_statement cl statements])
  | Lkeyword "do" -> print_endline "do";
    parse_sub_statements cl ( List.concat [statements; parse_do_statement cl statements])
  | Lkeyword "return" -> print_endline "return";
    parse_sub_statements cl ( List.concat [statements; parse_return_statement cl statements])
  | Lsymbol "}" -> print_endline "}"; statements
  | Lend -> raise (ParserError "Missing } in function\n")
  | _ as t -> print_string "parse_sub_statements: "; lexeme_print t; []
;;

(* Parses subroutine body *)
let rec parse_sub_body cl =
  match cl#next with
    Lcomment _ -> parse_sub_body cl
  | Lsymbol "{" -> let vars = parse_sub_vars cl [] in
           let statements = parse_sub_statements cl [] in
           (vars, statements)
  | _ -> raise (ParserError "Missing { in function body\n")
;;

(* Parse class subroutine definitions *)
let parse_class_subs cl =
  let string_to_funtype s =
    if s = "method" then METHOD
    else if s = "function" then FUNCTION
    else raise (ParserError ("Invalid function type" ^ s))
  in
  match cl#next with
    Lkeyword k when k = "function" || k = "method" ->
      let funtype = string_to_funtype k in
      let ret_type = get_type cl#next in
      let fun_name = get_ident cl#next in
      let fun_params = parse_sub_params cl in
      let (fun_vars, fun_statements) = parse_sub_body cl in
      [Dsub (funtype, ret_type, fun_name, fun_params, fun_vars, fun_statements)]
  | Lkeyword "constructor" -> let ret_type = get_type cl#next in
                  let con_name = get_ident cl#next in
                  let con_params = parse_sub_params cl in
                  let con_vars = parse_sub_vars cl [] in
                  [Dsub (CONSTRUCTOR, ret_type, con_name, con_params, con_vars, [])]
  | _ as l -> print_endline "No match for:"; lexeme_print l; []
;;

(* Parse class variable declarations *)
let rec parse_class_vars cl var_defs =
  (* translate scope keyword into scope type*)
  let get_scope token =
    match token with
      Lkeyword "static" -> Some STATIC
    | Lkeyword "field" -> Some FIELD
    | _ -> None in
  (* parse the list of variable names *)
  let rec get_name cl names =
    match cl#next with
      Lident name -> get_name cl (name::names)
    | Lsymbol "," -> get_name cl names
    | Lsymbol ";" -> names
    | _ -> raise (ParserError "Syntax error in class variable declaration") in
  let t = cl#peek in
  match (get_scope t) with
  (* if the token is a scope definition this is a variable declaration *)
    Some (STATIC | FIELD as vscope) ->
    (
      cl#advance;
      let vtype = get_type cl#next in
      let more_vars = List.map (fun name -> Dclass_var (vscope, vtype, name)) (get_name cl []) in
      parse_class_vars cl (List.concat [var_defs; more_vars]);
    )
  (* otherwise, we are done with class variable declaration block, so rewind the lexer for the token to be read again *)
  | None -> var_defs;;

(* Parse class definition *)
let parse_class cl =
  match cl#next with
    Lident class_name ->
    (
      match cl#next with
        Lsymbol "{" ->
        let vars_defs = parse_class_vars cl [] in
        let subs_defs = parse_class_subs cl in
        List.iter declaration_print vars_defs;
        Dclass (class_name, vars_defs, subs_defs)
      | _ -> raise (ParserError "")
    )
  | _ -> raise (ParserError "Syntax error in class declaration");;

let rec parse cl =
  let token = cl#next in
  match token with
    Lkeyword kwd ->
    if kwd = "class" then parse_class cl
    else raise (ParserError "Unexpected keyword at top level")
  | Lcomment _ -> parse cl
  | _ -> raise (ParserError "Unexpected statement at top level");;

let scanner cl =
  let token = cl#next in
  match token with
    Lkeyword kwd ->
    if kwd = "class" then parse_class cl
    else raise (ParserError "Unexpected keyword at top level")
  | Lcomment _ -> parse cl
  | _ -> raise (ParserError "Unexpected statement at top level");;
