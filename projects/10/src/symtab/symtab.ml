type sym =
    Svar
  | Sfield
  | Sarg
  | Sstat
  ;;

open Batteries

class symbol_table () =
    let ct = BatHashtbl.create 50 in
    let st = BatHashtbl.create 0 in
    object (s)
        val classt = ct
        val mutable subt = st

        method start_subroutine =
            subt <- BatHashtbl.create 50;
        method define (name:string) (typ:string) (kind:sym) =
            match kind with
            | Sfield | Sstat -> BatHashtbl.add classt name (typ, kind, (BatHashtbl.length classt) + 1)
            | Sarg | Svar -> BatHashtbl.add subt name (typ, kind, (BatHashtbl.length subt) + 1)
        method var_count (kind:sym) : int =
            let ht_enum =
                if kind = Sfield || kind = Sstat then
                    BatHashtbl.enum ct
                else
                    BatHashtbl.enum st
            in
            let count_types (c, kind) ((_: string), (v: (string * sym * int))) =
                match v with
                    | (_, tkind, _) when kind = tkind -> (c+1, kind)
                    | _ -> (c, kind)
            in
            let (count, _) = BatEnum.fold count_types (0, kind) ht_enum in
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
        (* TODO: Replace string/int with valid types*)
end;;

let test () =
    let symtab = new symbol_table () in
    symtab#define "name" "type" Svar;
    symtab#define "name2" "int" Svar;
    symtab#start_subroutine;
    match (symtab#index_of "name") with
        | Some(i) -> print_int i
        | None -> print_endline "none?"
        ;
    print_endline (string_of_int (symtab#var_count Svar));;
