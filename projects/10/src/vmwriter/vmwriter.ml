type segment =
    | CONST
    | ARG
    | LOCAL
    | STATIC
    | THIS
    | THAT
    | POINTER
    | TEMP
    ;;

let seg2str = function 
    | CONST -> "const"
    | ARG -> "arg"
    | LOCAL -> "local"
    | STATIC -> "static"
    | THIS -> "this"
    | THAT -> "that"
    | POINTER -> "ponter"
    | TEMP -> "temp"
;;

class vmwriter (fname:string) =
    let out_ch = open_out fname in
    object (s)
        val out = out_ch
        method write_push (seg:segment) (index:int) : unit = 
           output_string out ("push " ^ (seg2str seg) ^ " " ^ (string_of_int index) ^ "\n")
        method write_pop (seg:segment) (index:int) : unit = 
           output_string out ("pop " ^ (seg2str seg) ^ " " ^ (string_of_int index) ^ "\n")
end;;