include Frontend.Ast
include Frontend.Lex_and_parse
include Analyzer.Analysis
include Analyzer.Interval_domain

module AbsInterpreter = Interprete(Interval)

(* precising arguments the following way :
([filename], [loop_unroll || -1], [widening || -1]) *)
let parameters = [
  ("tests/basic1.geo", -1);
  ("tests/basic2.geo", -1);
  ("tests/basic3.geo", -1);
  ("tests/basic4.geo", -1);
  ("tests/basic5.geo", -1);
  ("tests/widening.geo", 5);
]

let rec analyze_tests p =
  match p with
  | (filename, unroll) :: rest ->
    begin
      let ast = get_ast filename in
      if unroll > 0 then
        loop_unroll := unroll;
      AbsInterpreter.eval_prog ast;
      analyze_tests rest
    end
  | [] -> ()

let () = analyze_tests parameters