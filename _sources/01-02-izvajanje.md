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

# Izvajanje

Običajno bi po sintaksi jezika formalno podali še njegovo semantiko, torej pomen posameznih delov. Ker za to še nimamo ustreznih matematičnih orodij, bomo ravnali podobno kot pri večini programskih jezikov: napisali bomo implementacijo, torej program, ki prebere ukaze, zapisane v konkretni sintaksi, ter jih na nek (po vsej sreči smiselen) način izvede. Nato pa bomo proglasili, da je pomen programa v IMPu tisto, kar implementacija z njim naredi. Implementacijo bomo napisali v programskem jeziku [OCaml](https://ocaml.org), ki je eden najprikladnejših jezikov za implementacije programskih jezikov. Končnica `ML` namreč pomeni _meta-language_ oziroma metajezik, torej jezik za opis jezikov.

Spomnimo se abstraktne sintakse jezika IMP:

```{code-cell}
:tags: [remove-output]

type location = Location of string

type exp =
  | Lookup of location
  | Int of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp

type bexp =
  | Bool of bool
  | Equal of exp * exp
  | Less of exp * exp
  | Greater of exp * exp

type cmd =
  | IfThenElse of bexp * cmd * cmd
  | WhileDo of bexp * cmd
  | Seq of cmd * cmd
  | Assign of location * exp
  | Skip
```

in primerov

```{code-cell}
let exp1 = Times (Int 6, Int 7)
let exp2 = Minus (Lookup (Location "m"), Int 1)
let bexp = Greater (Lookup (Location "m"), Int 0)
let cmd1 = Assign (Location "m", exp2)
let cmd2 = WhileDo (bexp, cmd1)
```

## Aritmetični in logični izrazi

Pomen bomo najprej določili aritmetičnim izrazom, ki naj bi predstavljali cela števila. Ker izrazi lahko vsebujejo pomnilniške lokacije, mora funkcija za evalvacijo poleg izraza sprejeti tudi trenutno stanje pomnilnika, ki ga bomo predstavili kar s seznamom parov, ki danim lokacijam pripiše cela števila, na primer

```{code-cell}
let st1 = [(Location "m", 10); (Location "n", 0)]
let st2 = [(Location "m", 5)]
```

```{code-cell}
let rec eval_exp st = function
  | Lookup l -> List.assoc l st
  | Int n -> n
  | Plus (e1, e2) -> eval_exp st e1 + eval_exp st e2
  | Minus (e1, e2) -> eval_exp st e1 - eval_exp st e2
  | Times (e1, e2) -> eval_exp st e1 * eval_exp st e2
```

Tako na primer velja

```{code-cell}
eval_exp [] exp1
```

```{code-cell}
eval_exp st1 exp2
```

```{code-cell}
eval_exp st2 exp2
```

Podobno lahko definiramo funkcijo za evalvacijo logičnih izrazov, ki sprejme stanje in logični izraz ter vrne logično vrednost:

```{code-cell}
let eval_bexp st = function
  | Bool b -> b
  | Equal (e1, e2) -> eval_exp st e1 = eval_exp st e2
  | Less (e1, e2) -> eval_exp st e1 < eval_exp st e2
  | Greater (e1, e2) -> eval_exp st e1 > eval_exp st e2
```

```{code-cell}
eval_bexp st1 bexp
```

```{code-cell}
eval_bexp st2 bexp
```

## Ukazi

Nazadnje definirajmo še funkcijo za evalvacijo ukazov. Funkcija sprejme stanje in ukaz, vrne pa končno vrednost stanja po izvršenem ukazu.

```{code-cell}
let rec eval_cmd st = function
  | IfThenElse (b, c1, c2) ->
      if eval_bexp st b then eval_cmd st c1 else eval_cmd st c2
  | WhileDo (b, c) ->
      (* eval_cmd st (IfThenElse (b, Seq (c, WhileDo (b, c)), Skip)) *)
      if eval_bexp st b then
        let st' = eval_cmd st c in
        eval_cmd st' (WhileDo (b, c))
      else st
  | Seq (c1, c2) ->
      let st' = eval_cmd st c1 in
      eval_cmd st' c2
  | Assign (l, e) -> (l, eval_exp st e) :: List.remove_assoc l st
  | Skip -> st
```

```{code-cell}
eval_cmd st1 cmd1
```

```{code-cell}
eval_cmd st2 cmd1
```

```{code-cell}
eval_cmd st1 cmd2
```

```{code-cell}
eval_cmd st2 cmd2
```

## Vaje

### Naloga 1

Razmislite, kako bi dopolnili evalvator jezika IMP za:

1. logična veznika `&&` in `||`,

2. ukaz `SWITCH`, ki zamenja vrednosti dveh lokacij,

3. ukaz `FAIL`, ki prekine izvajanje programa.
