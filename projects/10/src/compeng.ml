open Lexer;;
(*open Grammar_t;;*)

(*let string_of_declaration = Grammar_j.string_of_declaration;;*)

exception ParserError of string;;

class compeng (fname:string) =
    object (s)
      val in_ch = open_in fname
      val symtab = new Symtab.symbol_table ()
      val vmw = new Vmwriter.vmwriter ()

      method test () =
        vmw#init "/tmp/test.s";
        vmw#write_label "good";
        vmw#close
      
      method parse (cl:Lexer.lexeme_list) : unit =
        let token = cl#next in
        match token with
          Lkeyword kwd ->
          if kwd = "class" then s#parse_class cl
          else raise (ParserError "Unexpected keyword at top level")
        | Lcomment _ -> s#parse cl
        | Lend -> vmw#close ()
        | _ as t -> lexeme_print t; raise (ParserError "Unexpected statement at top level")

      (* Parse class definition *)
      method parse_class (cl:Lexer.lexeme_list) : unit =
        match cl#next with
          Lident class_name ->
          (
            vmw#init (class_name ^ ".vm");
            match cl#next with
              Lsymbol "{" -> ()
              (*let vars_defs = parse_class_vars cl [] in
              let subs_defs = parse_class_subs cl [] in
              `Dclass (class_name, vars_defs, subs_defs)*)
            | _ -> raise (ParserError "")
          )
        | _ -> raise (ParserError "Syntax error in class declaration")

      (* Parse class variable declarations *)
      method parse_class_vars (cl:Lexer.lexeme_list): unit =
        (* translate scope keyword into scope type*)
        let get_scope token =
          match token with
            Lkeyword "static" -> Some `STATIC
          | Lkeyword "field" -> Some `FIELD
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
          Some (`STATIC | `FIELD as vscope) ->
          (
            cl#advance;
            (*let vscope = symtab.scope2sym vscope in*)
            let vtype = Parser.get_type cl#next in
            List.iter (fun name -> symtab#define (name) vtype vscope) (get_name cl []);
            (*symtab#define (get_name cl) vtype vscope;*)
            let more_vars = List.map (fun name -> `Dclass_var (vscope, vtype, name)) (get_name cl []) in
            s#parse_class_vars cl
          )
        (* otherwise, we are done with class variable declaration block, so rewind the lexer for the token to be read again *)
        | None -> ()

end;;

let test () =
  let ce = new compeng "test.jack" in
  ce#test ();;
        (*let vmw = new Vmwriter.vmwriter (class_name ^ ".vm") in*)