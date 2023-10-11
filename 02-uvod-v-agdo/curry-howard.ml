(* A /\ B => B /\ A *)
(* 'a * 'b -> 'b * 'a *)
let konjunkcija_komutira (dokaz_a, dokaz_b) = (dokaz_b, dokaz_a)

type ('a, 'b) sum =
  | Left of 'a
  | Right of 'b

(* A /\ (B \/ C) => (A /\ B) \/ (A /\ C) *)
(* 'a * ('b, 'c) sum -> (('a * 'b), ('a * 'c)) sum *)

let distribucija_konjunkcije_cez_disjunkcijo (dokaz_a, dokaz_b_ali_c) =
  match dokaz_b_ali_c with
  | Left dokaz_b -> Left (dokaz_a, dokaz_b)
  | Right dokaz_c -> Right (dokaz_a, dokaz_c)

type empty

(* ~A \/ ~B => ~(A /\ B) *)

type 'a not = 'a -> empty

let negacija_konjunkcije (a_ni_res_ali_b_ni_res : ('a not, 'b not) sum) : ('a * 'b) not =
  match a_ni_res_ali_b_ni_res with
  | Left a_ni_res -> fun (x, y) -> a_ni_res x
  | Right b_ni_res -> fun (x, y) -> b_ni_res y

let hd = function
  | x :: _ -> Some x
  | [] -> None

let x : 'a =
  let rec f m = f m in
  f 0
