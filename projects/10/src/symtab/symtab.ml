open Batteries;;

(*let string_of_declaration = Grammar_j.string_of_declaration;;*)

exception ParserError of string;;

type sym =
    Svar
  | Sfield
  | Sarg
  | Sstat
  ;;

let scope2sym s = 
    match s with
    | `FIELD -> Sfield
    | `STATIC -> Sstat
    | _ -> raise (Failure "bad scope")

class symbol_table () =
    object (s)
        val classt = BatHashtbl.create 50
        val mutable subt = BatHashtbl.create 0

        method start_subroutine () =
            subt <- BatHashtbl.create 50;

        method define (name:Grammar.identifier) (typ:Grammar.var_type) (kind:Grammar.var_scope) =
            match kind with
            | `FIELD | `STATIC -> BatHashtbl.add classt name (typ, kind, (BatHashtbl.length classt) + 1)
            | `ARG | `VAR -> BatHashtbl.add subt name (typ, kind, (BatHashtbl.length subt) + 1)
        method var_count (kind:Grammar.var_scope) : int =
            let ht_enum =
                if kind = `FIELD || kind = `STATIC then
                    BatHashtbl.enum classt
                else
                    BatHashtbl.enum subt
            in
            let count_types (c, kind) ((_: Grammar.var_type), (v: (Grammar.identifier * Grammar.var_scope * int))) =
                match v with
                    | (_, tkind, _) when kind = tkind -> (c+1, kind)
                    | _ -> (c, kind)
            in
            let (count, _) = (*BatEnum.fold*) List.fold_left count_types (0, kind) [(`INT, ("foo", `STATIC, 5)) ](*ht_enum*) in
            count
        method kind_of name =
            match (BatHashtbl.find_option subt name) with
            | Some (_, kind, _) -> Some(kind)
            | None -> match (BatHashtbl.find_option classt name) with
                | Some(_, kind, _) -> Some(kind)
                | None -> None
        method type_of name =
            match (BatHashtbl.find_option subt name) with
            | Some (typ, _, _) -> Some(typ)
            | None -> match (BatHashtbl.find_option classt name) with
                | Some(typ, _, _) -> Some(typ)
                | None -> None
        method index_of name =
            match (BatHashtbl.find_option subt name) with
            | Some (_, _, i) -> Some(i)
            | None -> match (BatHashtbl.find_option classt name) with
                | Some(_, _, i) -> Some(i)
                | None -> None
end;;

let test () =
    let symtab = new symbol_table () in
    symtab#define "name" `CHAR `STATIC;
    symtab#define "name2" `INT `STATIC;
    symtab#start_subroutine ();
    match (symtab#index_of "name") with
        | Some(i) -> print_int i; print_newline ();
        | None -> print_endline "none?"
        ;
    print_endline (string_of_int (symtab#var_count `STATIC));;
