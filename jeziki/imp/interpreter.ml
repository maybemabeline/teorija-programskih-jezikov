let rec eval_exp st = function
  | Syntax.Int n -> n
  | Syntax.Lookup l -> List.assoc l st
  | Syntax.Plus (e1, e2) -> eval_exp st e1 + eval_exp st e2
  | Syntax.Minus (e1, e2) -> eval_exp st e1 - eval_exp st e2
  | Syntax.Times (e1, e2) -> eval_exp st e1 * eval_exp st e2

let eval_bexp st = function
  | Syntax.Bool b -> b
  | Syntax.Equal (e1, e2) -> eval_exp st e1 = eval_exp st e2
  | Syntax.Greater (e1, e2) -> eval_exp st e1 > eval_exp st e2
  | Syntax.Less (e1, e2) -> eval_exp st e1 < eval_exp st e2

let rec eval_cmd st = function
  | Syntax.Assign (l, e) ->
      let n = eval_exp st e in
      let st' = (l, n) :: List.remove_assoc l st in
      st'
  | Syntax.IfThenElse (b, c1, c2) ->
      if eval_bexp st b then eval_cmd st c1 else eval_cmd st c2
  | Syntax.Seq (c1, c2) ->
      let st' = eval_cmd st c1 in
      let st'' = eval_cmd st' c2 in
      st''
  | Syntax.Skip -> st
  | Syntax.WhileDo (b, c) ->
      if eval_bexp st b then
        let st' = eval_cmd st c in
        eval_cmd st' (Syntax.WhileDo (b, c))
      else st
  | Syntax.PrintInt e ->
      let n = eval_exp st e in
      let s = string_of_int n in
      print_endline s;
      st

let run c =
  let st' = eval_cmd [] c in
  print_endline "[";
  List.iter
    (fun (Syntax.Location l, n) ->
      print_endline ("  #" ^ l ^ " := " ^ string_of_int n))
    st';
  print_endline "]"
