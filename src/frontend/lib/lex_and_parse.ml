include Ast
include Parser
include Lexer

let get_ast filename = 
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast = Parser.main Lexer.lexer lexbuf in
  ast