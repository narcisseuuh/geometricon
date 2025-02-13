include Frontend.Ast
include Frontend.Lex_and_parse
include Analyzer.Analysis

(* precising arguments the following way :
([filename], [loop_unroll || -1], [widening || -1]) *)
let parameters = [
  ("basic1.geo", -1, -1);
  ("basic2.geo", -1, -1);
  ("basic3.geo", -1, -1);
  ("basic4.geo", -1, -1);
  ("basic5.geo", -1, -1);
  ("widening.geo", 5, 5);
]

let rec analyze_tests p =
  match p with
  | (filename, unroll, widening) :: rest ->
    begin
      let _ast = get_ast filename in
      if unroll > 0 then
        loop_unroll := unroll;
      if widening > 0 then
        widening_delay := widening;
      ();
      analyze_tests rest
    end
  | [] -> ()

let () = analyze_tests parameters