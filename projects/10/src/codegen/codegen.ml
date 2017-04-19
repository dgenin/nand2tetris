type sym = 
    Svar
  | Sfield
  | Sarg  
  | Sstat
  ;;

open BatHashtbl

class symbol_table () =
    let ct = create 50 in
    let st = create 0 in
    object (s)
        val classt = ct
        val mutable subt = st

        method start_subroutine = 
            subt <- create 50;
        method define (name:string) (typ:string) (kind:sym) = 
            match kind with
            | Sfield | Sstat -> add classt name (typ, kind, (length classt) + 1)
            | Sarg | Svar -> add subt name (typ, kind, (length subt) + 1)
        method kind_of name =
            match (find_option subt name) with
            | Some (_, kind, _) -> Some(kind)
            | None -> match (find_option classt name) with
                | Some(_, kind, _) -> Some(kind)
                | None -> None
        method type_of name =
            match (find_option subt name) with
            | Some (typ, _, _) -> Some(typ)
            | None -> match (find_option classt name) with
                | Some(typ, _, _) -> Some(typ)
                | None -> None
        method index_of name =
            match (find_option subt name) with
            | Some (_, _, i) -> Some(i)
            | None -> match (find_option classt name) with
                | Some(_, _, i) -> Some(i)
                | None -> None    
        (* TODO: Replace string/int with valid types*)
end;;

let test () =
    let symtab = new symbol_table () in
    symtab#define "name" "type" Svar;
    symtab#start_subroutine;
    match (symtab#index_of "name") with
        | Some(i) -> print_int i
        | None -> print_endline "none?"
