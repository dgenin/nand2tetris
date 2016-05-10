open Lexer


let global_sm =
      (*0*)
      let class_sm = [(*0*) [((Lkeyword("class"), Lany), (0,1))];
                      (*1*) [((Lident("*"), Lany), (0,2))];
                      (*2*) [((Lsymbol("{"), Lany), (0,3))];
                      (*3*) [((Lkeyword("static"), Lany), (1,0)); ((Lkeyword("field"), Lany), (1,0)); ((Lsymbol("}"), Lany), (0,4));
                             ((Lkeyword("method"), Lany), (-1,-1)); ((Lkeyword("function"), Lany), (2,0));
                            ((Lkeyword("constructor"), Lany), (-1,-1));]
                      (* (*2*) [(L0(Lkeyword("char"))), (0,3)); (L0(Lkeyword("int")), (0,3)); (L0(Lkeyword("boolean")), (0,3)); (L0(Lkeyword("void")), (0,3))];
                      (*3*) [(L0(Lident("*")), (0,4))];
                      (*4*) [(L0(Lsymbol(",")),(0,3)); (L0(Lsymbol(";")), (0,1))] *)
                      (*5*) (*Done*) ] in
      (*1*)
      let var_sm = [  (*0*) [((Lkeyword("char"), Lany), (1,1)); ((Lkeyword("int"), Lany), (1,1)); ((Lkeyword("boolean"), Lany), (1,1))];
                      (*1*) [((Lident("*"), Lany), (1,2))];
                      (*2*) [((Lsymbol(","), Lany), (1,1)); ((Lsymbol(";"), Lany), (1,3))]
                      (*3*) (*Done*) ] in
      (*2*)
      let func_sm = [ (*0*) [((Lkeyword("char"), Lany), (2,1)); ((Lkeyword("int"), Lany), (2,1)); ((Lkeyword("boolean"), Lany), (2,1));
                             ((Lkeyword("void"), Lany), (2,1))];
                      (*1*) [((Lident("*"), Lany), (2,2))];
                      (*2*) [((Lsymbol("("), Lany), (2,3))];
                      (*3*) [((Lkeyword("char"), Lany), (2,4)); ((Lkeyword("int"), Lany), (2,4)); ((Lkeyword("boolean"), Lany), (2,4)); ((Lsymbol(")"), Lany), (2, 6))];
                      (*4*) [((Lident("*"), Lany), (2,5))];
                      (*5*) [((Lsymbol(","), Lany), (2,3)); ((Lsymbol(")"), Lany), (2,6))];
                      (*6*) [((Lsymbol("{"), Lany), (2,7));];
                      (*7*) [((Lsymbol("}"), Lany), (2,8)); ((Lkeyword("var"), Lany), (1,0)); ((Lkeyword("let"), Lany), (3,1));];
                      (*8*) (*Done*) ] in
      (*3*)
      let statement_sm = [
                      (*0*) [((Lkeyword("let"), Lany), (3,1))];
                            (* varName [ expression ] = *)         (* varName = *)
                      (*1*) [((Lident("*"), Lsymbol("[")), (3,3)); ((Lident("*"), Lop '='), (3,5))];
                      (*2*) [((Lsymbol(";"), Lany)), (3,7)];

                             (* varName [ expression ] = *)
                      (*3*) [((Lsymbol("["), Lany), (4,0))];
                      (*4*) [((Lsymbol("]"), Lop '='), (3,5))];

                      (*5*) [((Lop '=', Lany), (4,0))];
                      (*6*) [((Lsymbol(";"), Lany), (3,7))];
                      (*7*)
                    ] in
      (*4*)                   (* unary op *)
      let expr_sm = [ (*0*) [((Lop('-'), Lany), (5,0)); ((Lsymbol("~"), Lany), (5,0));
                             (* term followed by an op*)
                             ((Lint(0), Lop('$')), (6,0)); ((Lstring("str"), Lop('$')), (6,0)); ((Lident("*"), Lop('$')), (6,0));
                             (* expression list *)
                             ((Lident("*"), Lsymbol("(")), (5,4));
                             (* varName [ expression ] *)
                             ((Lident("*"), Lsymbol("[")), (5,2));
                             (* (expression) *)
                             ((Lsymbol("("), Lany), (4,0));
                             (* term not followed by an op*)
                             ((Lint(0), Lany), (4,2));     ((Lstring("str"), Lany), (4,2)); ((Lident("*"), Lany), (4,2))];

                             (* handles: (expression) *)
                            (* term followed by an op*)        (* term not followed by an op*)
                      (*1*) [((Lsymbol(")"), Lop('$')), (6,0)); ((Lsymbol(")"), Lany), (4,2))];
                      (*2*) (*Done*) ] in

      (*5*)                  (* unary op*)
      let term_sm = [ (*0*) [((Lop('-'), Lany), (5,0)); ((Lsymbol("~"), Lany), (5,0));
                             (* term followed by an op*)
                             ((Lint(0), Lop('$')), (6,0)); ((Lstring("str"), Lop('$')), (6,0)); ((Lident("*"), Lop('$')), (6,0)); ((Lident("*"), Lsymbol("(")), (5,4)); ((Lident("*"), Lsymbol("[")), (5,2));
                             (* term not followed by an op*)
                             ((Lint(0), Lany), (5,6));     ((Lstring("str"), Lany), (5,6));     ((Lident("*"), Lany), (5,6)); ((Lsymbol("("), Lany), (4,0))];

                            (* handles: (expression) *)
                            (* term followed by an op*)        (* term not followed by an op*)
                      (*1*) [((Lsymbol(")"), Lop('$')), (6,0)); ((Lsymbol(")"), Lany), (6,0))];

                            (* handles varName [ expression ] *)
                      (*2*) [((Lsymbol("["), Lany), (4,0))];
                            (* term followed by an op*)        (* term not followed by an op*)
                      (*3*) [((Lsymbol("]"), Lop('$')), (6,0)); ((Lsymbol("]"), Lany), (5,6))];

                            (* handles subroutine call *)
                      (*4*) [((Lsymbol("("), Lany), (-1,-1))]; (*TODO: expression list*)
                            (* term followed by an op*)        (* term not followed by an op*)
                      (*5*) [((Lsymbol(")"), Lop('$')), (6,0)); ((Lsymbol(")"), Lany), (5,6))];
                      (*6*) (*Done*) ] in
      (*6*)
      let op_sm = [   (*0*) [((Lop('*'), Lsymbol("(")), (-1,-1)); ((Lident("*"), Lsymbol("[")), (-1,-1)); ((Lident("*"), Lany), (6,5)); ((Lsymbol("("), Lany), (4,0)); ((Lsymbol("-"), Lany), (5,0)); ((Lsymbol("~"), Lany), (5,0))];
                      (*1*) [((Lsymbol(")"), Lany), (6,5))];
                      (*2*) [((Lany, Lany), (-1,-1))];
                      (*3*) [((Lany, Lany), (-1,-1))];
                      (*4*) [((Lany, Lany), (-1,-1))];
                      (*5*) (*Done*) ] in


     [class_sm; var_sm; func_sm; statement_sm; expr_sm; term_sm; op_sm];;

let rec print_state si ii s  =
  match s with
    (l,i)::res -> let (a,b) = i in print_int si; print_char '.'; print_int ii; print_string "->"; print_int a; print_char '.'; print_int b;
                  let (l0, l1) = l in print_string "[label=\""; lexeme_print l0; print_string ","; lexeme_print l1; print_string "\"];\n";
                  print_state si ii res
  | [] -> print_string "\n\n";;

(* let print_transitions t =
  match t with
    ((l0, l1), (sm, sn)) -> lexeme_print l0; lexeme_print l1; print_int sm; print_int sn; print_endline;;

let print_transitions_line sm =
  List.iter print_state sm;; *)

List.iteri (fun i sm -> print_string "digraph test"; print_int i; print_endline "{"; List.iteri (print_state i) sm; print_endline "}") global_sm;;