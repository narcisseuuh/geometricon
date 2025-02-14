include Frontend.Lex_and_parse
include Analyzer.Analysis
include Analyzer.Interval_domain

module AbsInterpreter = Interprete(Interval)

exception ArgsError of string

let parse_set s =
  try
    if String.contains s 'x' then
      let parts = String.split_on_char 'x' s in
      match parts with
      | [x_part; y_part] ->
        let parse_interval part =
          if String.length part >= 2 && part.[0] = '[' && part.[String.length part - 1] = ']' then
            let interval = String.sub part 1 (String.length part - 2) in
            match String.split_on_char ',' interval with
            | [x1; x2] -> (x1, x2)
            | _ -> raise (ArgsError "Invalid interval format")
          else
            raise (ArgsError "Invalid interval format")
        in
        let x_interval = parse_interval x_part in
        let y_interval = parse_interval y_part in
        (AbstractSet x_interval, AbstractSet y_interval)
      | _ -> raise (ArgsError "Invalid target format")
    else
      raise (ArgsError "Invalid target format")
  with
  | Failure _ -> raise (ArgsError "Invalid number format")

let help fmt =
  Format.fprintf fmt "usage : analyzer [ARGS]* file.geo\n\
  arguments can be :\n\
  -unroll n : loop unrolling for the iter operator.\n\
  -target [interval]x[interval] : zone we want to ensure we don't hit. Default is [-1,0]x[0,1]."

let rec parse_args args =
  match args with
  | [] -> ()
  | "-target" :: target_sets :: rest ->
    begin
    target_set := parse_set target_sets;
    parse_args rest
    end
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
      AbsInterpreter.eval_prog ast 
  end