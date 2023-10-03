let read_source filename =
  let channel = open_in filename in
  let source = really_input_string channel (in_channel_length channel) in
  close_in channel;
  source

let () =
  if Array.length Sys.argv <> 2 then
    failwith ("Run IMP as '" ^ Sys.argv.(0) ^ " <filename>.imp'")
  else
    let filename = Sys.argv.(1) in
    let source = read_source filename in
    let c = Parser.parse source in
    if Checker.check c then Interpreter.run c
    else failwith "Not all locations are set!"
