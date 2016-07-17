open Lexer

(*** Parser ***)

type unr_op = UMINUS | NOT  ;;
type bin_op = PLUS | MINUS | MULT | DIV |
              EQUAL | LESS | GREAT | AND | OR  ;;
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

exception ParserError of string;;

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

let kwd_to_string kwd =
  match kwd with
    TRUE -> "true"
  | FALSE -> "false"
  | NULL -> "null"
  | THIS -> "this"
;;

let string_to_kwd_const s =
  match s with
    "true" -> Ekwd_const TRUE
  | "false" -> Ekwd_const FALSE
  | "null" -> Ekwd_const NULL
  | "this" -> Ekwd_const THIS
  | _ -> raise (Invalid_argument (s ^ " is not a valid kwd const"))
;;

let char_to_unr_op = function
    '-' -> UMINUS
  | '~' -> NOT
  | _ as s -> raise (Invalid_argument (Char.escaped s ^ " is not a valid unr op"))
;;

let char_to_bin_op = function
    '+' -> PLUS 
    | '-' -> MINUS 
    | '*' -> MULT 
    | '/' -> DIV 
    | '=' -> EQUAL 
    | '<' -> LESS 
    | '>' -> GREAT 
    | '&' -> AND 
    | '|' -> OR
    | _ as o -> raise (ParserError ("Invalid op" ^ (Char.escaped o)))
;;

let unr_op_to_string unr_op =
  match unr_op with
    UMINUS -> "-"
  | NOT -> "~"
;;

let bin_op_to_string bin_op =
  match bin_op with
    PLUS -> "+"
    | MINUS -> "-"
    | MULT -> "*"
    | DIV -> "/"
    | EQUAL -> "="
    | LESS -> "<"
    | GREAT -> ">"
    | AND -> "&"
    | OR -> "|"
;;

let rec expression_print ?indent:(indent=0) e =
  match e with
    Eint_const i -> print_string (String.make (2*indent) ' '); print_string "<intConst> "; print_int i; print_endline " </intConst>";
  | Estr_const s -> print_string (String.make (2*indent) ' '); print_string "<strConst> "; print_string s; print_endline " </strConst>";
  | Ekwd_const k -> print_string (String.make (2*indent) ' '); print_string "<keyword> "; print_string (kwd_to_string k); print_endline " </keyword>";
  | Eunr_exp (unr_op, exp) ->
    print_string (String.make (2*indent) ' '); print_endline "<term>";
    print_string (String.make (2*indent+2) ' '); print_endline ("<unaryOp> " ^ (unr_op_to_string unr_op) ^ " </unaryOp>");
    print_string (String.make (2*indent+2) ' '); print_endline "<expression>"; 
    expression_print ~indent:(indent+2) exp;
    print_string (String.make (2*indent+2) ' '); print_endline "</expression>"
  | Ebin_exp (exp1, bin_op, exp2) ->
    print_string (String.make (2*indent) ' '); print_endline "<expression>";
    print_string (String.make (2*indent+2) ' '); print_endline ("<binOp> " ^ (bin_op_to_string bin_op) ^ " </binOp>");
    expression_print ~indent:(indent+2) exp1;
    expression_print ~indent:(indent+2) exp2;
    print_string (String.make (2*indent) ' '); print_endline "</expression>";
  | Evar identifier -> print_string (String.make (2*indent) ' '); print_endline ("<ident> " ^ identifier ^ " </ident>");
  | Earray_elem (identifier, i) ->
    print_endline (identifier ^ "[" ^ (string_of_int i) ^ "]")
;;

let rec statement_print ?indent:(indent=0) st =
  match st with
    Slist statements ->
    (
      match statements with
        h::t -> statement_print h; statement_print (Slist t)
      | [] -> ()
    )
  | Slet (identifier, expression) ->
    print_string (String.make (2*indent) ' '); print_endline "<letStmt>";
    print_string (String.make (2*indent+2) ' '); print_endline ("<letLVal> " ^ identifier ^ " </letLVal>");
    print_string (String.make (2*indent+2) ' '); print_endline "<letRVal>";
    expression_print ~indent:(indent+2) expression;
    print_string (String.make (2*indent+2) ' '); print_endline "</letRVal>";
    print_string (String.make (2*indent) ' '); print_endline "</letStmt>"
  | Sif (expression, statements1, statements2) ->
    expression_print expression;
    statement_print (Slist statements1);
    statement_print (Slist statements2)
  | Swhile (expression, statements) ->
    expression_print expression;
    statement_print (Slist statements)
  | Sdo (identifier, expressions) ->
    print_string identifier;
    List.iter expression_print expressions
  | Sreturn ret_val ->
  (
    match ret_val with
      None -> print_endline "None"
    | VAL e -> expression_print e
  )
;;

let rec declaration_print ?indent:(indent=0) decl =
  match decl with
  | Dclass (class_name, class_vars, class_subs) ->
     print_endline "<classDec>";
     print_endline ("  <className> " ^ class_name ^ " </className>");
     List.iter (declaration_print ~indent:2) class_vars;
     List.iter (declaration_print ~indent:2) class_subs;
     print_endline "</classDec>"
  | Dclass_var (var_scope, var_type, var_name) ->
     print_string (String.make (2*indent) ' '); print_endline "<classVar>";
     print_string (String.make (2*indent+2) ' '); print_endline ("<varName> " ^ var_name ^ " </varName>");
     print_string (String.make (2*indent+2) ' '); print_endline ("<varType> " ^ (type_to_string var_type) ^ " </varType>");
     print_string (String.make (2*indent+2) ' '); print_endline ("<varScope> " ^ (scope_to_string var_scope) ^ "</varScope>");
     print_string (String.make (2*indent) ' '); print_endline "</classVar>"
  | Dsub_param (var_type, var_name) ->
     print_string (String.make (2*indent) ' '); print_endline "<subParam>";
     print_string (String.make (2*indent+2) ' '); print_endline ("<paramName> " ^ var_name ^ " </paramName>");
     print_string (String.make (2*indent+2) ' '); print_endline ("<paramType> " ^ (type_to_string var_type) ^ " </paramType>");
     print_string (String.make (2*indent) ' '); print_endline "</subParam>";
| Dsub_var (var_type, var_name) ->
     print_string (String.make (2*indent) ' '); print_endline "<subVar>";
     print_string (String.make (2*indent+2) ' '); print_endline ("<varName> " ^ var_name ^ " </varName>");
     print_string (String.make (2*indent+2) ' '); print_endline ("<varType> " ^ (type_to_string var_type) ^ " </varType>");
     print_string (String.make (2*indent) ' '); print_endline "</subVar>";
  | Dsub (sub_type, ret_type, sub_name, param_list, var_list, body) ->
     print_string (String.make (2*indent) ' '); print_endline "<classSub>";
     print_string (String.make (2*indent+2) ' '); print_endline ("<subName> " ^ sub_name ^ "</subName>");
     print_string (String.make (2*indent+2) ' '); print_endline ("<subType>" ^ (sub_type_to_string sub_type) ^ "</subType>");
     print_string (String.make (2*indent+2) ' '); print_endline ("<subRetType> " ^ (type_to_string ret_type) ^ " </subRetType>");
     print_string (String.make (2*indent+2) ' '); print_endline ("<subParams>");
     List.iter (declaration_print ~indent:(indent+2)) param_list; 
     print_string (String.make (2*indent+2) ' '); print_endline "</subParams>";
     print_string (String.make (2*indent+2) ' '); print_endline "<subVars>";
     List.iter (declaration_print ~indent:(indent+2)) var_list;
     print_string (String.make (2*indent+2) ' '); print_endline "</subVars>";
     print_string (String.make (2*indent+2) ' '); print_endline "<subStmts>";
     List.iter (statement_print ~indent:(indent+2)) body;
     print_string (String.make (2*indent+2) ' '); print_endline "</subStmts>";
     print_string (String.make (2*indent) ' '); print_endline "</classSub>";
  | _ -> print_string "whatever";;

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
    | Lkeyword _ as t ->
      (
        let param_type = get_type t and
        param_name = get_ident cl#next in
        match cl#next with
          Lsymbol ")" -> List.concat [param_defs; [Dsub_param (param_type, param_name)]]
        | Lsymbol "," -> get_params cl (List.concat [param_defs; [Dsub_param (param_type, param_name)]])
        | _ as t -> lexeme_print t; raise (ParserError "Invalid token in subroutine parameter declaration")
      )
    | _ as t -> lexeme_print t; raise (ParserError "Unexpected lexeme")
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
    in parse_sub_vars cl (List.concat [var_defs; more_vars])
  | _ -> var_defs
;;

exception OpError;;

let parse_op cl =
  match cl#next with
    Lop o -> Some (char_to_bin_op o)
  | _ -> cl#rewind; None

let rec parse_term cl =
  match cl#next with
    (* Add the rest of expression types *)
    (* integerConstant *)
    Lint i -> Eint_const i
    (* stringConstant *)
  | Lstring s -> Estr_const s
    (* keywordConstant *)
  | Lkeyword k when k = "true" || k = "false" || k = "this" || k = "null" ->
    string_to_kwd_const k
  | Lop s when s = '-' || s = '~' ->
    Eunr_exp ((char_to_unr_op s), parse_term cl)
    (* varName *)
  | Lident ident when cl#peek != (Lsymbol "(") && cl#peek != (Lsymbol "[")->
    (
      if cl#peek = Lsymbol "["
      (* Add support for varname[idx] *)
      then raise (ParserError "Not yet")
      else Evar ident
    )
    (* ( expression ) *)
  | Lsymbol "(" -> print_endline (": (expressions ) are not supported yet"); Estr_const "DEADBEEF"
    (* varName [ expression ] *)
  | Lident ident when cl#peek = (Lsymbol "[") ->
    print_endline (ident ^ ": arrays are not supported yet"); Estr_const "DEADBEEF"
    (* subroutineCall *)
  | Lident ident when cl#peek = (Lsymbol "(") || cl#peek = (Lsymbol ".") ->
    print_endline (ident ^ ": subroutines are not supported yet"); Estr_const "DEADBEEF"
  | _ as t -> lexeme_print t; print_endline "not implemented yet"; Estr_const "DEADBEEF"
    ;;

let rec parse_expression cl =
  match (parse_term cl) with
  Eint_const _ 
  | Estr_const _ 
  | Ekwd_const _ 
  | Evar _
  | Eunr_exp _ as e -> 
  (
    match parse_op cl with
      Some bin_op ->  Ebin_exp (e, bin_op, (parse_expression cl))
    | None -> e
  )
  | _ -> raise (ParserError "Not supported yet")
  ;;

(*
  let parse_expr_inner cl exp =
    (* will be used to control order *)
    match cl#next with
      Lsymbol ";" -> exp
    | _ -> print_endline "not implemented yet"; exp
  in
  match cl#next with
    (* Add the rest of expression types *)
    Lint i -> parse_expr_inner cl (Eint_const i)
  | Lstring s -> parse_expr_inner cl (Estr_const s)
  | Lkeyword k when k = "true" || k = "false" || k = "this" || k = "null" ->
    parse_expr_inner cl (string_to_kwd_const k)
  | Lident ident ->
    (
      if cl#peek = Lsymbol "["
      (* Add support for varname[idx] *)
      then raise (ParserError "Not yet")
      else parse_expr_inner cl (Evar ident)
    )
  | Lop s when s = '-' || s = '~' ->
    Eunr_exp ((char_to_unr_op s), parse_expression cl)
  | _ as t -> lexeme_print t; print_endline "not implemented yet"; Estr_const "DEADBEEF"
;;
*)

(* Parse subroutine statements *)
let rec parse_if_statement cl statements = statements
and parse_let_statement cl statements =
  match cl#next with
    Lcomment _ -> parse_let_statement cl statements
  | Lident id -> (* add check that var is defined? *)
    (
      match cl#next with
        Lop '=' -> let tmp = [Slet (id, (parse_expression cl))] in cl#advance; tmp
      | Lsymbol "[" -> raise (ParserError "arrays not implemented")
      | _ as t -> lexeme_print t; raise (ParserError "not implemented")
    )
  | Lsymbol ";" -> statements
  | Lend -> raise (ParserError "Reached end of file in let\n")
  | _ as t -> lexeme_print t; raise (ParserError ("Bad"))
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
let rec parse_class_subs cl sub_defs =
  let string_to_funtype s =
    if s = "method" then METHOD
    else if s = "function" then FUNCTION
    else raise (ParserError ("Invalid function type" ^ s))
  in
  match cl#next with
    Lkeyword k when k = "function" || k = "method" ->
    let funtype = string_to_funtype k in
    let ret_type = get_type cl#next;  in
    let fun_name = get_ident cl#next in
    let fun_params = parse_sub_params cl in
    let (fun_vars, fun_statements) = parse_sub_body cl in
    parse_class_subs cl
      (Dsub (funtype, ret_type, fun_name, fun_params, fun_vars, fun_statements) :: sub_defs)
  | Lkeyword "constructor" -> let ret_type = get_type cl#next in
    let con_name = get_ident cl#next in
    let con_params = parse_sub_params cl in
    let con_vars = parse_sub_vars cl [] in
    parse_class_subs cl
      (Dsub (CONSTRUCTOR, ret_type, con_name, con_params, con_vars, []) :: sub_defs)
  | Lsymbol "}" -> sub_defs
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
        let subs_defs = parse_class_subs cl [] in
        Dclass (class_name, vars_defs, subs_defs)
      | _ -> raise (ParserError "")
    )
  | _ -> raise (ParserError "Syntax error in class declaration");;

let rec parse cl classes =
  let token = cl#next in
  match token with
    Lkeyword kwd ->
    if kwd = "class" then parse cl ((parse_class cl) :: classes)
    else raise (ParserError "Unexpected keyword at top level")
  | Lcomment _ -> parse cl classes
  | Lend -> classes
  | _ as t -> lexeme_print t; raise (ParserError "Unexpected statement at top level");;

let scanner cl =
  let classes = parse cl [] in
  classes
  (* return classes for further processing? *)
