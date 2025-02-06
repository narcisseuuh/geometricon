{
(* Definitions and helper functions *)
open Parser

exception Lexing_error of string
}

rule token = parse
    | [' ' '\t' '\r' '\n'] { token lexbuf }  (* Skip whitespace *)
    | ['0'-'9']+ ('.' ['0'-'9']+)? as lxm { INT lxm } (* representing mathematical integers *)
    | "/*" { comment lexbuf ; token lexbuf }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '[' { LBRACK }
    | ']' { RBRACK }
    | '{' { LCURL }
    | '}' { RCURL }
    | "init" { INIT_KW }
    | "translation" { TRANSLAT_KW }
    | "rotation" { ROTAT_KW }
    | "or" { OR_KW }
    | "iter" { ITER_KW }
    | 'x' { TIMES }
    | ',' { COMMA }
    | ';' { SEMICOL }
    | eof { EOF }
    | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) }

and comment = parse
    | "/*" { comment lexbuf; comment lexbuf }
    | "*/" { () }
    | eof { failwith "commentaire non termine" }
    | _  { comment lexbuf }

{
(* Entry point for the lexer *)
let lexer = token
}