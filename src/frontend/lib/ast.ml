(* Syntaxe abstraite issue de l'analyse syntaxique *)

type abstract_set =
  AbstractSet of string * string

type init =
  Init of abstract_set * abstract_set

type stmt =
  | Translation of string * string
  | Rotation of string * string * string
  | Iteration of stmt list
  | Or of stmt list * stmt list

type program =
  Program of init * stmt list

(* pretty printing for AST *)

open Format

let comma fmt () = fprintf fmt ",@ "
let newline fmt () = fprintf fmt "@."

let rec print_list p sep fmt = function
  | [] -> ()
  | [e] -> p fmt e
  | e :: el -> fprintf fmt "%a%a%a" p e sep () (print_list p sep) el

let rec print_stmt fmt = function
  | Translation (u, v) ->
    fprintf fmt "translation(%s, %s);" u v
  | Rotation (u, v, theta) ->
    fprintf fmt "rotation(%s, %s, %s);" u v theta
  | Iteration l ->
    fprintf fmt "iter {@\n  @[<v>%a@]@\n}" (print_list print_stmt newline) l
  | Or (l1, l2) ->
    fprintf fmt "{@\n  @[<v>%a@]@\n} or {@\n  @[<v>%a@]@\n}"
      (print_list print_stmt newline) l1 (print_list print_stmt newline) l2

let print_set fmt (AbstractSet (i1, i2)) =
  fprintf fmt "@[<hov 2>[%s,@ %s]@]" i1 i2

let print_init fmt (Init (s1, s2)) =
  fprintf fmt "@[<hov 2>init(%a,@ %a);@]" print_set s1 print_set s2

let print fmt (Program (init, stmts)) =
  fprintf fmt "%a@\n@[<v>%a@]@\n" print_init init (print_list print_stmt newline) stmts