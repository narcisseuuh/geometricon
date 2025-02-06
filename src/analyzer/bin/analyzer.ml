include Frontend.Lex_and_parse

let () =
  if Array.length Sys.argv <> 2 then
    failwith "you should precise a file to parse";
  let ast = get_ast Sys.argv.(1) in
  print Format.std_formatter ast