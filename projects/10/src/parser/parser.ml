(*open Lexer;;*)

(*let string_of_declaration = Grammar_j.string_of_declaration;;*)

exception ParserError of string;;

let string_to_kwd_const s =
  match s with
    "true" -> `Ekwd_const `TRUE
  | "false" -> `Ekwd_const `FALSE
  | "null" -> `Ekwd_const `NULL
  | "this" -> `Ekwd_const `THIS
  | _ -> raise (Invalid_argument (s ^ " is not a valid kwd const"))
;;

let char_to_unr_op = function
    '-' -> `UMINUS
  | '~' -> `NOT
  | _ as s -> raise (Invalid_argument (Char.escaped s ^ " is not a valid unr op"))
;;

let char_to_bin_op = function
    '+' -> `PLUS
    | '-' -> `MINUS
    | '*' -> `MULT
    | '/' -> `DIV
    | '=' -> `EQUAL
    | '<' -> `LESS
    | '>' -> `GREAT
    | '&' -> `AND
    | '|' -> `OR
    | _ as o -> raise (ParserError ("Invalid op" ^ (Char.escaped o)))
;;

(* Parse the class subroutines *)

(* translate type keyword to type type*)
let get_type token =
  match token with
    Lexer.Lkeyword "int" -> `INT
  | Lexer.Lkeyword "char" -> `CHAR
  | Lexer.Lkeyword "boolean" -> `BOOLEAN
  | Lexer.Lkeyword "void" -> `VOID
  | Lexer.Lident _ -> `CLASS "test"
  | _ -> Lexer.lexeme_print token; raise (ParserError "Invalid type");;

let get_ident token =
  match token with
    Lexer.Lident ident -> ident
  | _ as t -> Lexer.lexeme_print t; raise (ParserError "Invalid identifier");;

(* Parse subroutine params *)
let parse_sub_params cl =
  (* reads subroutine parameters, including the closing parenthesis *)
  let rec get_params cl param_defs =
    match cl#next with
      Lexer.Lsymbol ")" -> param_defs (* empty param list *)
    | Lexer.Lkeyword _ as t ->
      (
        let param_type = get_type t and
        param_name = get_ident cl#next in
        match cl#next with
          Lexer.Lsymbol ")" -> List.concat [param_defs; [`Dsub_param (param_type, param_name)]]
        | Lexer.Lsymbol "," -> get_params cl (List.concat [param_defs; [`Dsub_param (param_type, param_name)]])
        | _ as t -> Lexer.lexeme_print t; raise (ParserError "Invalid token in subroutine parameter declaration")
      )
    | _ as t -> Lexer.lexeme_print t; raise (ParserError "Unexpected lexeme")
  in
  match cl#next with
    Lexer.Lsymbol "(" -> get_params cl []
  | _ as t -> Lexer.lexeme_print t; raise (ParserError "Missing open parenthesis in subroutine parameter declaration");;

(* Parse subroutine variables *)
let rec parse_sub_vars cl var_defs =
  (* parse the list of variable names *)
  let rec get_name cl names =
    match cl#next with
      Lexer.Lident name -> get_name cl (name::names)
    | Lexer.Lsymbol "," -> get_name cl names
    | Lexer.Lsymbol ";" -> names
    | _ as t -> Lexer.lexeme_print t; raise (ParserError "Syntax error in subroutine variable declaration")
  in
  match cl#peek with
    Lexer.Lkeyword "var" -> cl#advance; let vtype = get_type cl#next in
              let more_vars = List.map (fun name -> `Dsub_var (vtype, name)) (get_name cl [])
    in parse_sub_vars cl (List.concat [var_defs; more_vars])
  | _ -> var_defs
;;

exception OpError;;

let parse_op cl =
  match cl#next with
    Lexer.Lop o -> Some (char_to_bin_op o)
  | _ -> cl#rewind; None

let rec parse_term cl =
  match cl#next with
    (* Add the rest of expression types *)
    (* integerConstant *)
    Lexer.Lint i -> `Eint_const i
    (* stringConstant *)
  | Lexer.Lstring s -> `Estr_const s
    (* keywordConstant *)
  | Lexer.Lkeyword k when k = "true" || k = "false" || k = "this" || k = "null" ->
    string_to_kwd_const k
  | Lexer.Lop s when s = '-' || s = '~' ->
    `Eunr_exp ((char_to_unr_op s), parse_term cl)
    (* varName *)
  | Lexer.Lident ident when cl#peek <> (Lexer.Lsymbol "(") && cl#peek <> (Lexer.Lsymbol "[") &&
    cl#peek <> (Lexer.Lsymbol ".") ->
    `Evar ident
    (* ( expression ) *)
  | Lsymbol "(" ->
    (
      let ex = `Eparen_exp (parse_expression cl) and
      close_paren = cl#next in
      match close_paren with
      | Lexer.Lsymbol ")" -> ex
      | _ -> Lexer.lexeme_print close_paren; raise (ParserError " but expecting )")
    )
    (* varName [ expression ] *)
  | Lexer.Lident ident when cl#peek = (Lexer.Lsymbol "[") ->
  (
    cl#advance;
    let ex = `Earray_elem (ident, (parse_expression cl)) and
    close_bracket = cl#next in
    match close_bracket with
    | Lexer.Lsymbol "]" -> ex
    | _ -> Lexer.lexeme_print close_bracket; raise (ParserError " but expecting ]")
  )
    (* subroutineCall *)
  | Lexer.Lident _ when cl#peek = (Lexer.Lsymbol "(") || cl#peek = (Lexer.Lsymbol ".") ->
    cl#rewind; `Esubcall (parse_subroutine_call cl)
  | _ as t -> Lexer.lexeme_print t; raise (ParserError "Syntax error in parse_term")
and

parse_expression cl =
  match (parse_term cl) with
  `Eint_const _
  | `Estr_const _
  | `Ekwd_const _
  | `Evar _
  | `Eunr_exp _
   (* may need to change if we decide to keep parenthesis *)
  | `Eparen_exp _
  | `Earray_elem _
  | `Esubcall _
  | `Ebin_exp _ as e ->
  (
    match parse_op cl with
      Some bin_op ->  `Ebin_exp (e, bin_op, (parse_expression cl))
    | None -> e
  )

(* Parse subroutine expression list *)
and parse_exp_list cl =
  (* reads subroutine expression list, including the closing parenthesis *)
  let rec get_params cl exp_list =
    match cl#peek with
      Lexer.Lsymbol ")" -> cl#advance; exp_list (* empty param list *)
    | _ ->
      (
        let exp = parse_expression cl in
        match cl#next with
          Lexer.Lsymbol ")" -> exp::exp_list
        | Lexer.Lsymbol "," -> get_params cl (exp::exp_list)
        | _ as t -> Lexer.lexeme_print t; raise (ParserError "Invalid token in expression list")
      )
  in
  match cl#next with
    Lexer.Lsymbol "(" -> get_params cl []
  | _ as t -> Lexer.lexeme_print t; raise (ParserError "Missing open parenthesis in subroutine expression list")

 (* Parse subroutine call*)
 and parse_subroutine_call cl =
  match cl#next with
     (* subroutineCall *)
  | Lexer.Lident ident when cl#peek = (Lexer.Lsymbol "(") ->
    `Subcall (ident, (parse_exp_list cl))
    (* methodCall *)
  | Lexer.Lident className when cl#peek = (Lsymbol ".") ->
    cl#advance;
    let subName = get_ident cl#next in
    if cl#peek = (Lexer.Lsymbol "(") then
      `Methcall (className, subName, (parse_exp_list cl))
    else
    begin
      Lexer.lexeme_print cl#peek; raise (ParserError "but expecting (")
    end
  | _ as t -> Lexer.lexeme_print t; raise (ParserError "Invalid subroutine call")
  ;;

let check_terminal t v =
  if t = v
  then ()
  else
  begin
    print_string "Expected: "; Lexer.lexeme_print t;
    print_string "\nbut got: "; Lexer.lexeme_print v; raise (ParserError "")
  end
  ;;

(* Parse subroutine statements *)
let rec parse_if_statement cl =
  check_terminal (Lsymbol "(") cl#next;
  let exp = parse_expression cl in
  check_terminal (Lsymbol ")") cl#next;
  check_terminal (Lsymbol "{") cl#next;
  let if_st = parse_sub_statements cl [] in
  if cl#peek <> (Lkeyword "else") then  [`Sif (exp, if_st, [])]
  else
  begin
    cl#advance;
    check_terminal (Lsymbol "{") cl#next;
    let else_st = parse_sub_statements cl [] in
    [`Sif (exp, if_st, else_st)]
  end

and parse_let_statement cl =
  let parse_let_simple cl =
    match cl#next with
      Lexer.Lop '=' -> let ex = (parse_expression cl) in
        check_terminal (Lexer.Lsymbol ";") cl#next; ex
    | _ as t -> Lexer.lexeme_print t; raise (ParserError " but expected =")
  in
  match cl#next with
    Lexer.Lcomment _ -> parse_let_statement cl
  | Lexer.Lident id when cl#peek <> (Lexer.Lsymbol "[") -> (* add check that var is defined? *)
     [`Slet (id, (parse_let_simple cl))]
  | Lexer.Lident id when cl#peek = (Lexer.Lsymbol "[") ->
    (
      cl#advance;
      let index_ex = (parse_expression cl) in
      check_terminal (Lexer.Lsymbol "]") cl#next;
      [`Slet_array (id, index_ex, (parse_let_simple cl))]
    )
  | Lend -> raise (ParserError "Reached end of file in let\n")
  | _ as t -> Lexer.lexeme_print t; raise (ParserError ("Bad"))

and parse_while_statement cl =
  check_terminal (Lsymbol "(") cl#next;
  let exp = parse_expression cl in
  check_terminal (Lsymbol ")") cl#next;
  check_terminal (Lsymbol "{") cl#next;
  [`Swhile (exp, (parse_sub_statements cl []))]

and  parse_do_statement cl =
  let subcall = parse_subroutine_call cl in
  check_terminal (Lsymbol ";") cl#next; [`Sdo (subcall)]

and  parse_return_statement cl =
  if cl#peek <> (Lexer.Lsymbol ";") then
  begin
    let r = [`Sreturn (Some (parse_expression cl))] in
    check_terminal (Lexer.Lsymbol ";") cl#next;
    r
  end
  else ( cl#advance; [`Sreturn (None)] )

and  parse_sub_statements cl statements =
  match cl#next with
    Lexer.Lkeyword "if" ->
    parse_sub_statements cl (List.concat [statements; parse_if_statement cl])
  | Lexer.Lkeyword "let" ->
    parse_sub_statements cl ( List.concat [statements; parse_let_statement cl])
  | Lexer.Lkeyword "while" ->
    parse_sub_statements cl ( List.concat [statements; parse_while_statement cl])
  | Lexer.Lkeyword "do" ->
    parse_sub_statements cl ( List.concat [statements; parse_do_statement cl])
  | Lexer.Lkeyword "return" ->
    parse_sub_statements cl ( List.concat [statements; parse_return_statement cl])
  | Lsymbol "}" -> statements
  | Lend -> raise (ParserError "Missing } in function\n")
  | _ as t -> print_string "parse_sub_statements: "; Lexer.lexeme_print t; []
;;

(* Parses subroutine body *)
let rec parse_sub_body cl =
  match cl#next with
    Lexer.Lcomment _ -> parse_sub_body cl
  | Lexer.Lsymbol "{" -> let vars = parse_sub_vars cl [] in
           let statements = parse_sub_statements cl [] in
           (vars, statements)
  | _ -> raise (ParserError "Missing { in function body\n")
;;

(* Parse class subroutine definitions *)
let rec parse_class_subs cl sub_defs =
  let string_to_funtype s =
    if s = "method" then `METHOD
    else if s = "function" then `FUNCTION
    else if s = "constructor" then `CONSTRUCTOR
    else raise (ParserError ("Invalid function type" ^ s))
  in
  match cl#next with
  | Lexer.Lkeyword k when k = "function" || k = "method" || k = "constructor" ->
    let funtype = string_to_funtype k in
    let ret_type = get_type cl#next;  in
    let fun_name = get_ident cl#next in
    let fun_params = parse_sub_params cl in
    let (fun_vars, fun_statements) = parse_sub_body cl in
    parse_class_subs cl
      (`Dsub (funtype, ret_type, fun_name, fun_params, fun_vars, fun_statements) :: sub_defs)
  | Lexer.Lsymbol "}" -> sub_defs
  | _ as l -> print_endline "No match for:"; Lexer.lexeme_print l; []
;;

(* Parse class variable declarations *)
let rec parse_class_vars cl var_defs =
  (* translate scope keyword into scope type*)
  let get_scope token =
    match token with
      Lexer.Lkeyword "static" -> Some `STATIC
    | Lexer.Lkeyword "field" -> Some `FIELD
    | _ -> None in
  (* parse the list of variable names *)
  let rec get_name cl names =
    match cl#next with
      Lexer.Lident name -> get_name cl (name::names)
    | Lexer.Lsymbol "," -> get_name cl names
    | Lexer.Lsymbol ";" -> names
    | _ -> raise (ParserError "Syntax error in class variable declaration") in
  let t = cl#peek in
  match (get_scope t) with
  (* if the token is a scope definition this is a variable declaration *)
    Some (`STATIC | `FIELD as vscope) ->
    (
      cl#advance;
      let vtype = get_type cl#next in
      let more_vars = List.map (fun name -> `Dclass_var (vscope, vtype, name)) (get_name cl []) in
      parse_class_vars cl (List.concat [var_defs; more_vars]);
    )
  (* otherwise, we are done with class variable declaration block, so rewind the lexer for the token to be read again *)
  | None -> var_defs;;

(* Parse class definition *)
let parse_class cl =
  match cl#next with
    Lexer.Lident class_name ->
    (
      match cl#next with
        Lexer.Lsymbol "{" ->
        let vars_defs = parse_class_vars cl [] in
        let subs_defs = parse_class_subs cl [] in
        `Dclass (class_name, vars_defs, subs_defs)
      | _ -> raise (ParserError "")
    )
  | _ -> raise (ParserError "Syntax error in class declaration");;

let rec parse cl classes =
  let token = cl#next in
  match token with
    Lexer.Lkeyword kwd ->
    if kwd = "class" then parse cl ((parse_class cl) :: classes)
    else raise (ParserError "Unexpected keyword at top level")
  | Lexer.Lcomment _ -> parse cl classes
  | Lexer.Lend -> classes
  | _ as t -> Lexer.lexeme_print t; raise (ParserError "Unexpected statement at top level");;

let scanner cl =
  let classes = parse cl [] in
  classes
  (* return classes for further processing? *)
