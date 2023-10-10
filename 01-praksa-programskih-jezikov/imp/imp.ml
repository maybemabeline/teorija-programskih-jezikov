let read_source filename =
  let channel = open_in filename in
  let source = really_input_string channel (in_channel_length channel) in
  close_in channel;
  source

let () =
  match Array.to_list Sys.argv with
  | [ _; filename ] ->
      let source = read_source filename in
      let c = Parser.parse source in
      if Checker.check c then Interpreter.run c
      else failwith "Not all locations are set!"
  | [ _; "--ast"; filename ] ->
      filename |> read_source |> Parser.parse |> Graphviz.output
      |> print_endline
  | _ ->
      failwith
        ("Run IMP as '" ^ Sys.argv.(0) ^ " <filename>.imp' or '" ^ Sys.argv.(0)
       ^ " <filename>.imp tag'")
