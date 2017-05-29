open Printf

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

type command =
    | ADD
    | SUB
    | NEG
    | EQ
    | GT
    | LT
    | AND
    | OR
    | NOT
    ;;

let seg2str = function 
    | CONST -> "constant"
    | ARG -> "argument"
    | LOCAL -> "local"
    | STATIC -> "static"
    | THIS -> "this"
    | THAT -> "that"
    | POINTER -> "ponter"
    | TEMP -> "temp"
;;

let cmd2str = function
    | ADD -> "add"
    | SUB -> "sub"
    | NEG -> "neg"
    | EQ -> "eq"
    | GT -> "gt"
    | LT -> "lt"
    | AND -> "and"
    | OR -> "or"
    | NOT -> "not"
    ;;

class vmwriter () =
    object
        val mutable out: out_channel = open_out "/dev/null" 
        method init (fname:string) : unit  =
            out <- (open_out fname)
        method write_push (seg:segment) (index:int) : unit = 
            output_string out ("\tpush " ^ (seg2str seg) ^ " " ^ (string_of_int index) ^ "\n")
        method write_pop (seg:segment) (index:int) : unit = 
            output_string out ("\tpop " ^ (seg2str seg) ^ " " ^ (string_of_int index) ^ "\n")
        method write_arithmetic (cmd:command) : unit = 
            output_string out ("\t" ^ (cmd2str cmd) ^ "\n")
        method write_label (label:string) : unit =
            output_string out ("label " ^ (label) ^ "\n")
        method write_goto (label:string) : unit =
            output_string out ("\tgoto " ^ (label) ^ "\n")
        method write_if (label:string) : unit =
            output_string out ("\tif-goto " ^ (label) ^ "\n")
        method write_call (func:string) (num_args:int) : unit =
            output_string out (sprintf "\tcall %s %d\n" func num_args)
        method write_function (func:string) (num_locs:int) : unit =
            output_string out (sprintf "function %s %d\n" func num_locs)
        method write_return : unit =
            output_string out "\treturn\n"
        method close (): unit = close_out out
end;;

let test () = 
    let vmw = new vmwriter () in
    vmw#init "test.s"; 
    vmw#write_push THIS 2;
    vmw#write_push ARG 1;
    vmw#write_arithmetic ADD;
    vmw#write_push ARG 0;
    vmw#write_push ARG 1;
    vmw#write_push CONST 5;
    vmw#write_call "Math.multiply" 2;
    vmw#write_call "BankAccount.commission" 2;
    vmw#write_arithmetic SUB;
    vmw#write_pop THIS 2;
    vmw#write_return;
    vmw#close ();;
    
    