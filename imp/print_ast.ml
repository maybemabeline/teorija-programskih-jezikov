let output_graphviz tag cmd =
  let string_of_location (Syntax.Location l) = Printf.sprintf "#%s" l in
  let exp_label = function
    | Syntax.Lookup l -> string_of_location l
    | Int i -> string_of_int i
    | Plus _ -> "+"
    | Minus _ -> "-"
    | Times _ -> "*"
  and bexp_label = function
    | Syntax.Bool b -> string_of_bool b
    | Equal _ -> "="
    | Less _ -> "<"
    | Greater _ -> ">"
  and cmd_label = function
    | Syntax.Assign _ -> ":="
    | IfThenElse _ -> "if _ then _ else"
    | Seq _ -> ";"
    | Skip -> "Skip"
    | WhileDo _ -> "while"
  in
  let node_name ind = Printf.sprintf "%s%d" tag ind in
  let node_str name label = Printf.sprintf "%s[label=%S];" name label in
  let connect name child = Printf.sprintf "%s -> %s;" name child in
  let rec tree_exp ind exp =
    let name = node_name ind in
    let ind = ind + 1 in
    let ind, direct, all =
      match exp with
      | Syntax.Lookup (Location l) -> (ind, [], [])
      | Int i -> (ind, [], [])
      | Times (l, r) | Minus (l, r) | Plus (l, r) ->
          let ind, l, la = tree_exp ind l in
          let ind, r, ra = tree_exp ind r in
          (ind, [ l; r ], la @ ra)
    in
    let all = List.map (connect name) direct @ all in
    let all = node_str name (exp_label exp) :: all in
    (ind, name, all)
  and tree_bexp ind bexp =
    let name = node_name ind in
    let ind = ind + 1 in
    let ind, direct, all =
      match bexp with
      | Syntax.Bool b -> (ind, [], [])
      | Equal (l, r) | Less (l, r) | Greater (l, r) ->
          let ind, l, la = tree_exp ind l in
          let ind, r, ra = tree_exp ind r in
          (ind, [ l; r ], la @ ra)
    in
    let all = List.map (connect name) direct @ all in
    let all = node_str name (bexp_label bexp) :: all in
    (ind, name, all)
  and tree_cmd ind cmd =
    let name = node_name ind in
    let ind = ind + 1 in
    let ind, direct, all =
      match cmd with
      | Syntax.Assign (l, exp) ->
          let ind, l, la = tree_exp ind (Lookup l) in
          let ind, r, ra = tree_exp ind exp in
          (ind, [ l; r ], la @ ra)
      | IfThenElse (bexp, c1, c2) ->
          let ind, l, la = tree_bexp ind bexp in
          let ind, t, ta = tree_cmd ind c1 in
          let ind, f, fa = tree_cmd ind c2 in
          (ind, [ l; t; f ], la @ ta @ fa)
      | WhileDo (bexp, c) ->
          let ind, l, la = tree_bexp ind bexp in
          let ind, t, ta = tree_cmd ind c in
          (ind, [ l; t ], la @ ta)
      | Seq (c1, c2) ->
          let ind, t, ta = tree_cmd ind c1 in
          let ind, f, fa = tree_cmd ind c2 in
          (ind, [ t; f ], ta @ fa)
      | Skip -> (ind, [], [])
    in
    let all = List.map (connect name) direct @ all in
    let all = node_str name (cmd_label cmd) :: all in
    (ind, name, all)
  in
  let _, _, all = tree_cmd 0 cmd in
  Printf.sprintf "digraph G {\n  subgraph ast_%s {\n    %s\n  }\n}\n" tag
    (String.concat "\n    " all)

let read_source filename =
  let channel = open_in filename in
  let source = really_input_string channel (in_channel_length channel) in
  close_in channel;
  source

let main () =
  if Array.length Sys.argv <> 3 then
    failwith ("Run as '" ^ Sys.argv.(0) ^ " <filename>.imp' tag ")
  else
    Sys.argv.(1) |> read_source |> Parser.parse
    |> output_graphviz Sys.argv.(2)
    |> print_endline

let _ = main ()
