%{
open Ast
%}

%token <string> INT
%token INIT_KW TRANSLAT_KW ROTAT_KW OR_KW ITER_KW
%token LPAREN RPAREN LBRACK RBRACK LCURL RCURL
%token TIMES COMMA SEMICOL
%token EOF

%start main

%type <Ast.program> main
%type <Ast.init> init
%type <Ast.abstract_set> set
%type <Ast.stmt list> stmt_list
%type <Ast.stmt> stmt
%type <Ast.stmt> operation
%%

main:
| i=init SEMICOL l=stmt_list EOF { Program (i, l) }
;

init:
| INIT_KW LPAREN s1=set TIMES s2=set RPAREN { Init (s1, s2) }
;

set:
| LBRACK i1=INT COMMA i2=INT RBRACK { AbstractSet (i1, i2) }
;

stmt_list:
| l=separated_list(SEMICOL, stmt) { l }
;

stmt:
| operation { $1 }
| LCURL s1=stmt_list RCURL OR_KW LCURL s2=stmt_list RCURL { Or (s1, s2) }
| ITER_KW LCURL s=stmt_list RCURL { Iteration s }
;

operation:
| TRANSLAT_KW LPAREN u=INT COMMA v=INT RPAREN
    { Translation (u, v) }
| ROTAT_KW LPAREN u=INT COMMA v=INT COMMA theta=INT RPAREN
    { Rotation (u, v, theta) }
;