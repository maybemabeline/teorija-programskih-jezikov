---
jupytext:
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.12
    jupytext_version: 1.8.0
kernelspec:
  display_name: OCaml 4.11
  language: OCaml
  name: ocaml-jupyter
---

# Razčlenjevanje

Razčlenjevalnikov običajno ne pišemo direktno, temveč se poslužimo posebnih orodij, ki sprejmejo definicijo jezika v zapisu podobnem BNF, ter iz nje samodejno ustvarijo program. V primeru OCamla sta taka programa [`ocamlyacc`](https://v2.ocaml.org/manual/lexyacc.html) in njegova naprednejša različica [Menhir](http://gallium.inria.fr/~fpottier/menhir/). Prav tako je sestavni del razčlenjevalnika _lekser_, ki zaporedje znakov najprej pretvori v zaporedje _leksemov_, kjer so zaporedne števke že združene v števila, ključne besede nadomeščene s posebnimi simboli, presledki in komentarji pa odstranjeni.

Mi pa bomo zato, da bomo ostali znotraj enega orodja in zraven ponovili še malo funkcijskega programiranja, ubrali bolj neposredno pot in uporabili [kombinatorje razčlenjevalnikov](https://en.wikipedia.org/wiki/Parser_combinator). Vsak razčlenjevalnik bo sestavljen iz manjših razčlenjevalnikov, od katerih bo vsak predelal del besedila. Poglejmo najprej, kakšne oblike bodo.

## Tip razčlenjevalnikov `'a parser`

Recimo, da želimo razčlenjevalnik za cela števila. Začnimo z željo po funkciji `string -> int`, ki vzame vhodni niz ter vrne prebrano število, na primer iz niza `"42"` dobimo število `42`.

Ker vsak niz ne bo predstavljal števila, moramo najprej popraviti ti rezultata, da dobimo `string -> int option`. Tako bomo iz niza `"42"` dobili `Some 42`, iz niza `"abc"` pa `None`.

Razčlenjevalnike bomo združevali in vsak ne bo prebral celotnega niza, temveč samo njegov začetek, preostanek pa bo predal nadaljnjim razčlenjevalnikom. Zato ne vrnemo samo rezultata, temveč tudi preostali niz, zato je še boljši tip `string -> (int * string) option`. Tako bi razčlenjevalnik pri vhodu `"42abc"` vrnil `Some (42, "abc")`, pri vhodu `"42"` rezultat `Some (42, "")`, pri vhodu `"abc42"` pa `None`, saj začetek ne predstavlja števila.

Niz je veliko enostavneje brati postopoma, če ga poprej pretvorimo v zaporedje znakov. Za pretvorbo uporabljamo funkciji:

```{code-cell}
let explode str = List.init (String.length str) (String.get str)
let implode chrs = String.init (List.length chrs) (List.nth chrs)
```

```{code-cell}
explode "abc"
```

```{code-cell}
implode ['T'; 'P'; 'J']
```

Tudi tip razčlenjevalnikov spremenimo v `char list -> (int * char list) option`. Ker seveda ne bomo brali samo celih števil, celoten tip razčlenjevalnikov parametriziramo glede na tip prebranih vrednosti:

```{code-cell}

type 'a parser = char list -> ('a * char list) option
```

Zgoraj opisani razčlenjevalnik bi tako lahko definirali kot:

```{code-cell}
let parse_int : int parser =
  fun chrs ->
  let rec take_initial_digits =
    function
    | chr :: chrs when String.contains "0123456789" chr ->
        let digits, rest = take_initial_digits chrs in
        (chr :: digits, rest)
    | chrs -> ([], chrs)
  in
  match take_initial_digits chrs with
  | ([], chrs) -> None
  | (digits, chrs) ->
      Some (int_of_string (implode digits), chrs)
```

```{code-cell}
parse_int (explode "42abc")
```

```{code-cell}
parse_int (explode "42")
```

```{code-cell}
parse_int (explode "abc42")
```

Pri definiciji smo ročno dopisali tip `int parser`, saj bi nam OCaml sicer prikazoval ekvivalenten, vendar manj pregleden tip `char list -> (int * char list) option`. Za preglednejše testiranje si napišimo pomožno funkcijo `($$$) : 'a parser -> string -> ('a * string) option`, ki samodejno poskrbi za pretvorbo med nizi in zaporedji znakov.

```{code-cell}
let ( $$$ ) (parser : 'a parser) str =
  match parser (explode str) with
  | None -> None
  | Some (x, rest) -> Some (x, implode rest)
```

Funkciji damo simbolno ime `( $$$ )`, da jo lahko kličemo infiksno kot `parser $$$ niz`.

```{code-cell}
parse_int $$$ "42abc"
```

## Osnovni razčlenjevalniki

Vse razčlenjevalnike pisati na tak način bi bilo precej nerodno. Raje si bomo definirali nekaj osnovnih, ki jih bomo združevali v večje. Prvi je razčlenjevalnik `fail`, ki vedno zavrne vhod:

```{code-cell}
let fail : 'a parser = fun _chrs -> None
```

```{code-cell}
fail $$$ "42abc"
```

Nato imamo funkcijo `return`, ki sprejme vrednost `v` in vrne razčlenjevalnik, ki ne glede na vhod vrne uspešno prebrano vrednost `v`, celoten vhod pa posreduje naprej.

```{code-cell}
let return v : 'a parser = fun chrs -> Some (v, chrs)
```

```{code-cell}
return 10 $$$ "42abc"
```

Prvi razčlenjevalnik, ki bo dejansko upošteval vhod, je `character`, ki prebere prvi znak, kadar obstaja, ostale pa pošlje naprej:

```{code-cell}
let character : char parser = function
  | [] -> None
  | chr :: chrs -> Some (chr, chrs)
```

```{code-cell}
character $$$ "42abc"
```

```{code-cell}
character $$$ ""
```

Nato imamo izbiro, ki najprej poskusi en razčlenjevalnik, če pa temu ne uspe, pa še drugega. Oba razčlenjevalnika morata seveda vračati vrednost enakega tipa.

```{code-cell}
let ( || ) (parser1 : 'a parser) (parser2 : 'a parser) : 'a parser =
 fun chrs ->
  match parser1 chrs with
  | None -> parser2 chrs
  | Some (v, chrs') -> Some (v, chrs')
```

```{code-cell}
(fail || character) $$$ "42abc"
```

```{code-cell}
(return 'X' || character) $$$ "42abc"
```

Nazadnje moramo znati razčlenjevalnike še združevati, kar storimo s funkcijo `>>=`, ki dva razčlenjevalnika veriži enega za drugim. V večini primerov bo drugi razčlenjevalnik odvisen od vrednosti, ki jo je prebral prvi. Na primer, če bomo najprej prebrali znak za začetek komentarja, bomo preskočili vse do konca vrstice, če pa bomo našli kaj drugega, pa bomo pričakovali veljaven ukaz. Tako je `parser1` razčlenjevalnik, ki vrne vrednost tipa `'a`, med tem ko `parser2` ni razčlenjevalnik, temveč funkcija `'a -> 'b parser`, ki vrne razčlenjevalnik odvisen od prebranega rezultata.

```{code-cell}
let ( >>= ) (parser1 : 'a parser) (parser2 : 'a -> 'b parser) : 'b parser =
 fun chrs ->
  match parser1 chrs with
  | None -> None
  | Some (v, chrs') -> parser2 v chrs'
```

Veriženje deluje tako, da najprej uporabimo `parser1`. Če že ta zavrne vhod, takoj končamo in vrnemo `None`. V primeru, pa da dobimo neko vrednost `v`, jo skupaj s preostankom znakov podamo funkciji `parser2`, ki potem uspešno ali neuspešno vrne končni rezultat.

Na primer, spodnja kombinacija najprej prebere katerikoli znak, nato pa ta znak poda funkciji, ki ne glede na preostanek, ki sledi, vedno vrne niz, sestavljen iz 10 kopij tega znaka.

```{code-cell}
let vedno_preberi_10_kopij_znaka c = return (String.make 10 c)
```

```{code-cell}
character >>= vedno_preberi_10_kopij_znaka $$$ "42abc"
```

Isto seveda lahko dosežemo tudi z anonimno funkcijo:

```{code-cell}
(character >>= fun c -> return (String.make 10 c)) $$$ "42abc"
```

## Osnovni kombinatorji

Izkaže se, da je zgoraj naštetih pet razčlenjevalnikov, torej `fail`, `return`, `character`, `||` in `>>=` dovolj, da izrazimo vse razčlenjevalnike, ki jih potrebujemo. Na primer, če želimo vrniti par zaporedoma prebranih vrednosti, lahko to zapišemo kot:

```{code-cell}
let pair parser1 parser2 = parser1 >>= fun v1 -> parser2 >>= fun v2 -> return (v1, v2)
```

```{code-cell}
pair character character $$$ "42abc"
```

Če želimo, da uspešno prebrana vrednost zadošča še kakšnemu predikatu, uporabimo funkcijo `satisfy`, kjer v drugem delu odvisno od pogoja vrednost vrnemo ali zavrnemo:

```{code-cell}
let satisfy cond parser =
  parser >>= fun v -> if cond v then return v else fail
```

Na primer, spodnji razčlenjevalnik sprejema samo znake `'a'`, `'b'` in `'c'`.

```{code-cell}
satisfy (String.contains "abc") character $$$ "123"
```

```{code-cell}
satisfy (String.contains "abc") character $$$ "a23"
```

Pri zapisu pogojnih razčlenjevalnikov uporabljamo operacijo `x |> f`, ki funkcijo `f` uporabi na vrednosti `x`. Isti primer kot zgoraj bi napisali na sledeč način, kjer je bolj jasno, da najprej preberemo znak, nato pa preverimo, da je eden od znakov niza `"abc"`.

```{code-cell}
character |> satisfy (String.contains "abc") $$$ "a23"
```

Konkretna uporaba kombinatorja `satisfy` je razčlenjevalnik `exactly`, ki sprejme natanko znak `chr`:

```{code-cell}
let exactly chr = character |> satisfy (( = ) chr)
```

```{code-cell}
(exactly 'a' || exactly 'b') $$$ "abc"
```

Enako si lahko definiramo razčlenjevalnike, ki sprejemajo samo števke, črke ali presledke:

```{code-cell}

let digit =
  let is_digit = String.contains "0123456789" in
  character |> satisfy is_digit

let alpha =
  let is_alpha = String.contains "abcdefghijklmnopqrstvwuxyz" in
  character |> satisfy is_alpha

let space =
  let is_space = String.contains " \n\t\r" in
  character |> satisfy is_space
```

Podoben kombinator je `map`, ki prebere rezultat `v` in ga pretvori s pomočjo funkcije `f`.

```{code-cell}
let map f parser = parser >>= fun v -> return (f v)
```

```{code-cell}
(character |> map Char.uppercase_ascii) $$$ "abc"
```

Poseben primer operacije `>>=` je `>>`, kjer ne upoštevamo vrednosti, ki jo je prebral prvi razčlenjevalnik. Na primer, vemo, da se zanka vedno začne s ključno besedo `while`, ampak sama vrednost `"while"` za pomen programa ni pomembna.

```{code-cell}
let ( >> ) parser1 parser2 = parser1 >>= fun _ -> parser2
```

```{code-cell}
(exactly 'a' >> exactly 'b' >> character) $$$ "abcde"
```

```{code-cell}
(exactly 'a' >> exactly 'b' >> character) $$$ "bacde"
```

Postopek lahko posplošimo na poljuben niz znakov. Razčlenjevalnik napišemo tako, da besedo `str` razbijemo na seznam znakov `chrs`, nato iz tega naredimo seznam razčlenjevalnikov, ki zaporedoma sprejemajo natanko te znake, nato pa jih vse skupaj verižimo s pomočjo `>>`. Ta razčlenjevalnik bomo uporabljali za prebiranje ključnih besed kot so `while`, `if` in podobno, zato na koncu vrnemo vrednost `()`.

```{code-cell}
let word str =
  let chrs = explode str in
  let chr_parsers = List.map exactly chrs in
  List.fold_right ( >> ) chr_parsers (return ())
```

```{code-cell}
word "WHILE" $$$ "WHILE TRUE DO SKIP"
```

```{code-cell}
word "WHILE" $$$ "IF TRUE THEN SKIP ELSE SKIP"
```

Podobno lahko s pomočjo `fold_right` zaporedoma z `||` poskusimo vse razčlenjevalnike iz danega seznama:

```{code-cell}
let one_of parsers = List.fold_right ( || ) parsers fail
```

```{code-cell}
one_of [word "WHILE"; word "IF"] $$$ "WHILE TRUE DO SKIP"
```

```{code-cell}
one_of [word "WHILE"; word "IF"] $$$ "IF TRUE THEN SKIP ELSE SKIP"
```

```{code-cell}
one_of [word "WHILE"; word "IF"] $$$ "SKIP"
```

Rekurzivno si lahko definiramo tudi razčlenjevalnika `many` in `many1`, ki dani razčlenjevalnik uporabita poljubno mnogokrat oziroma vsaj enkrat, vrneta pa seznam uspešno prebranih vrednosti:

```{code-cell}
let rec many parser = many1 parser || return []

and many1 parser =
  parser >>= fun v ->
  many parser >>= fun vs -> return (v :: vs)
```

```{code-cell}
many digit $$$ "1234abc567"
```

S pomočjo vseh zgoraj naštetih kombinatorjev zdaj veliko lepše napišemo razčlenjevalnik za branje celih števil. Najprej moramo prebrati vsaj eno števko, nato seznam števk združimo v en sam niz, na koncu pa pogoljufamo in s pomočjo vgrajene funkcije to pretvorimo v število.

```{code-cell}
let integer = many1 digit |> map implode |> map int_of_string
```

```{code-cell}
integer $$$ "123abc"
```

## Razčlenjevalniki za IMP

Razvili smo vsa orodja, ki jih potrebujemo, da preberemo celotno sintakso jezika IMP. Za ločevanje med posameznimi deli bomo uporabili presledke. Pri ključnih besedah bomo zahtevali vsaj enega, okoli operatorjev pa ne nujno:

```{code-cell}
let spaces = many space >> return ()
let spaces1 = many1 space >> return ()
```

Zaradi enostavnosti bomo za večje podizraze zahtevali, da se pojavijo v oklepajih:

```{code-cell}
let parens parser =
  exactly '(' >> spaces >> parser >>= fun v -> spaces >> exactly ')' >> return v
```

```{code-cell}
parens integer $$$ "(1234)"
```

Lokacije se bodo začele z znakom `#`, ki jim bo sledilo ime. To bo sestavljeno iz alfanumeričnih znakov, od katerih mora biti prvi črka.

```{code-cell}
let ident =
  alpha >>= fun chr ->
  many (alpha || digit) >>= fun chrs -> return (implode (chr :: chrs))

let location = word "#" >> ident >>= fun ident -> return (Location ident)
```

```{code-cell}
location $$$ "#fact"
```

Dvojiška operacija je podana s simbolom `op`, njegovim pomenom `f` ter z razčlenjevalnikom argumentov `parser`. Med operacijo in argumentoma so morebitni presledki

```{code-cell}
let binop parser op f =
  parser >>= fun v1 ->
  spaces >> word op >> spaces >>
  parser >>= fun v2 ->
  return (f v1 v2)
```

Pri razčlenjevalniku za aritmetične izraze sledimo sintaksi. Izrazi so lahko sestavljeni iz aritmetičnih operacij ali atomarni, pri čemer so atomarni izrazi lokacije, konstante ali običajni izrazi v oklepajih.

```{code-cell}
let rec exp chrs =
  one_of
    [
      binop atomic_exp "+" (fun e1 e2 -> Plus (e1, e2));
      binop atomic_exp "-" (fun e1 e2 -> Minus (e1, e2));
      binop atomic_exp "*" (fun e1 e2 -> Times (e1, e2));
      atomic_exp;
    ]
    chrs

and atomic_exp chrs =
  one_of
    [
      (location >>= fun l -> return (Lookup l));
      (integer >>= fun n -> return (Int n));
      parens exp;
    ]
    chrs
```

```{code-cell}
exp $$$ "1 + 3"
```

Podobno definiramo razčlenjevalnik za Booleove izraze:

```{code-cell}
let bexp =
  one_of
    [
      word "TRUE" >> return (Bool true);
      word "FALSE" >> return (Bool false);
      binop exp "=" (fun e1 e2 -> Equal (e1, e2));
      binop exp "<" (fun e1 e2 -> Less (e1, e2));
      binop exp ">" (fun e1 e2 -> Greater (e1, e2));
    ]
```

```{code-cell}
bexp $$$ "1 + 3 < 2 * 4"
```

Podobno definiramo tudi razčlenjevalnik za ukaze:

```{code-cell}
let rec cmd chrs =
  let if_then_else =
    word "IF" >> spaces1 >> bexp >>= fun b ->
    spaces1 >> word "THEN" >> spaces1 >> cmd >>= fun c1 ->
    spaces1 >> word "ELSE" >> spaces1 >> atomic_cmd >>= fun c2 ->
    return (IfThenElse (b, c1, c2))
  and while_do =
    word "WHILE" >> spaces1 >> bexp >>= fun b ->
    spaces1 >> word "DO" >> spaces1 >> atomic_cmd >>= fun c ->
    return (WhileDo (b, c))
  and seq =
    atomic_cmd >>= fun c1 ->
    spaces >> word ";" >> spaces >> cmd >>= fun c2 ->
    return (Seq (c1, c2))
  in
  one_of [ if_then_else; while_do; seq; atomic_cmd ] chrs

and atomic_cmd chrs =
  let assign =
    location >>= fun l ->
    spaces >> word ":=" >> spaces >> exp >>= fun e ->
    return (Assign (l, e))
  and skip = word "SKIP" >> return Skip
  in
  one_of [ assign; skip; parens cmd ] chrs
```

```{code-cell}
cmd $$$ "IF 3 < 4 THEN SKIP ELSE SKIP"
```

Vsaka IMP datoteka je sestavljena iz enega samega ukaza (ki je seveda lahko sestavljen iz več ukazov, ločenih s podpičjem). Da preberemo celotno datoteko, torej le pokličemo razčlenjevalnik `cmd`. Če nam ni ostalo nič neprebranih znakov, poženemo `eval_cmd` na praznem seznamu, sicer pa program zavrnemo:

```{code-cell}
let run str =
  match str |> String.trim |> explode |> cmd with
  | Some (c, []) -> eval_cmd [] c
  | Some (_, _ :: _) | None -> failwith "Parsing error"
```

```{code-cell}
run "#n := 10; #fact := 1; WHILE #n > 0 DO ( #fact := #fact * #n; #n := #n - 1 )"
```

## Vaje

### Naloga 1

Izboljšajte razčlenjevalnik, da bo dopolnil nepopolne pogojne stavke. Ukaz `IF b THEN c` naj se prevede v enako sintaktično drevo kot `IF b THEN c ELSE SKIP`.
