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
  | [ _imp; filename ] -> filename |> parse_source |> check_and_run
  | [ _imp; "--ast"; filename ] ->
      filename |> parse_source |> Graphviz.ast_string |> print_endline
  | _ ->
      let imp = Sys.executable_name in
      failwith
        (Printf.sprintf
           "Run IMP as '%s <filename>.imp' or '%s --ast <filename>.imp'" imp imp)
