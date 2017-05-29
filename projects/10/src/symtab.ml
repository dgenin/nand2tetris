open Batteries;;

module G = Grammar
module BH = BatHashtbl

type sym =
    Svar
  | Sfield
  | Sarg
  | Sstat
  ;;

class symbol_table () =
    object (s)
        val classt = BH.create 50
        val mutable subt = BH.create 0

        method start_subroutine () =
            subt <- BH.create 50;

        method define (name:G.identifier) (typ:G.var_type) (kind:G.var_scope) =
            match kind with
            | `FIELD | `STATIC -> BH.add classt name (typ, kind, (BH.length classt) + 1)
            | `ARG | `VAR -> BH.add subt name (typ, kind, (BH.length subt) + 1)
        method var_count (kind:G.var_scope) : int =
            let ht_enum =
                if kind = `FIELD || kind = `STATIC then
                    BH.enum classt
                else
                    BH.enum subt
            in
            let count_types (c, kind) ((_: G.identifier), (v: (G.var_type * G.var_scope * int))) =
                match v with
                    | (_, tkind, _) when kind = tkind -> (c+1, kind)
                    | _ -> (c, kind)
            in
            (*let (count, _) = List.fold_left count_types (0, kind) [(`INT, ("foo", `STATIC, 5)) ] in*)
            let (count, _) = BatEnum.fold count_types (0, kind) ht_enum in count
        method kind_of name =
            match (BH.find_option subt name) with
            | Some (_, kind, _) -> Some(kind)
            | None -> match (BH.find_option classt name) with
                | Some(_, kind, _) -> Some(kind)
                | None -> None
        method type_of name =
            match (BH.find_option subt name) with
            | Some (typ, _, _) -> Some(typ)
            | None -> match (BH.find_option classt name) with
                | Some(typ, _, _) -> Some(typ)
                | None -> None
        method index_of name =
            match (BH.find_option subt name) with
            | Some (_, _, i) -> Some(i)
            | None -> match (BH.find_option classt name) with
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
