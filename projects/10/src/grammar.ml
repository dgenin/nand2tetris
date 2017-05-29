(* Auto-generated from "grammar.atd" *)


type bin_op = [
    `PLUS | `MINUS | `MULT | `DIV | `EQUAL | `LESS | `GREAT | `AND | `OR
]

type const = [ `TRUE | `FALSE | `NULL | `THIS ]

type identifier = string

type sub_type = [ `CONSTRUCTOR | `FUNCTION | `METHOD ]

type unr_op = [ `UMINUS | `NOT ]

type var_scope = [ `STATIC | `FIELD | `ARG | `VAR ]

type var_type = [ `INT | `CHAR | `BOOLEAN | `VOID | `CLASS of identifier ]

type expression = [
    `Eint_const of int
  | `Estr_const of string
  | `Ekwd_const of const
  | `Eunr_exp of (unr_op * expression)
  | `Ebin_exp of (expression * bin_op * expression)
  | `Evar of identifier
  | `Earray_elem of (identifier * expression)
  | `Eparen_exp of expression
  | `Esubcall of subroutine_call
]

and subroutine_call = [
    `Subcall of (identifier * expression list)
  | `Methcall of (identifier * identifier * expression list)
]

type statement = [
    `Slist of statement list
  | `Slet of (identifier * expression)
  | `Slet_array of (identifier * expression * expression)
  | `Sif of (expression * statement list * statement list)
  | `Swhile of (expression * statement list)
  | `Sdo of subroutine_call
  | `Sreturn of expression option
]

type declaration = [
    `Dclass of (identifier * declaration list * declaration list)
  | `Dclass_var of (var_scope * var_type * identifier)
  | `Dsub
      of (
          sub_type
        * var_type
        * identifier
        * declaration list
        * declaration list
        * statement list
      )
  | `Dsub_var of (var_type * identifier)
  | `Dsub_param of (var_type * identifier)
  | `Dempty
]

type paren = [ `LPAREN | `RPAREN ]

type brack = [ `LBRACK | `RBRACK ]

type brace = [ `LBRACE | `RBRACE ]
