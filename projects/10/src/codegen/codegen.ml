type sym = 
    Svar
  | Sfield
  | Ssub
  | Smethod
  ;;

class scope ast  par =
    let symt = BatHashtbl.create 100 in
    object (s)
        val symtab = symt
        val mutable children = []
        val parent = par (* need to use sub-ast*)
        (* TODO: Replace string/int with valid types*)
        method has_sym (sym: string) : bool =
            try
                let _ = BatHashtbl.find symtab sym in true
            with Not_found -> false
        method get_sym (sym: string) : sym option =
            try
                BatHashtbl.find symtab sym
            with Not_found -> None
end;;