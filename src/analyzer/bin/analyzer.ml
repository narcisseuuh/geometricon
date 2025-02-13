include Frontend.Lex_and_parse
include Analyzer.Analysis
include Analyzer.Results

exception ArgsError of string

let help fmt =
  Format.fprintf fmt "usage : analyzer [ARGS]* file.geo\n\
  arguments can be :\n\
  -unroll n : loop unrolling for the iter operator."

let rec parse_args args =
  match args with
  | [] -> ()
  | "-unroll" :: n_str :: rest ->
    begin
      try 
        let n = int_of_string n_str in
        loop_unroll := n;
        parse_args rest
      with
      | _ -> raise (ArgsError "loop unrolling should be followed\
              by an integer.")
    end
  | filename :: rest ->
    if String.ends_with ~suffix:".geo" filename then
      begin
        file := filename;
        parse_args rest
      end
    else
      raise (ArgsError (String.cat "unknown argument " filename))

let () =
  begin
    parse_args @@ List.tl (Array.to_list Sys.argv);
    let filename = !file in
    if filename = "" then
      help Format.err_formatter
    else
      let ast = get_ast !file in
      let () = print Format.std_formatter ast in
      ()
  end