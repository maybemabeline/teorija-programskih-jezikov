let read_source filename =
  let channel = open_in filename in
  let source = really_input_string channel (in_channel_length channel) in
  close_in channel;
  source

let parse_source filename =
  let source = read_source filename in
  Parser.parse source

let check_and_run c =
  if Checker.check c then Interpreter.run c
  else failwith "Not all locations are set!"

let () =
  match Array.to_list Sys.argv with
  | [ _; filename ] -> filename |> parse_source |> check_and_run
  | [ _; "--ast"; filename ] ->
      filename |> parse_source |> Graphviz.ast_string |> print_endline
  | _ ->
      failwith
        ("Run IMP as '" ^ Sys.argv.(0) ^ " <filename>.imp' or '" ^ Sys.argv.(0)
       ^ " --ast <filename>.imp'")
