include Frontend.Lex_and_parse
include Interpreter.Lib_interpreter
open Oplot.Plt

let () =
begin
  if Array.length Sys.argv <> 2 then
    failwith "usage : interpreter [file.geo]";
  let ast = get_ast Sys.argv.(1) in
  print Format.std_formatter ast;
  let point = interpret ast in
  plot_point point;
  if !to_plot <> [] then
    display ~dev:img !to_plot
end