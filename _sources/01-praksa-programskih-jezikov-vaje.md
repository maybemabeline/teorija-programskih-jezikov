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

# Vaje

```{code-cell}
:tags: [remove-cell, remove-stdout]

(* Ko se v Jupytru prvič požene OCaml, program Findlib izpiše neko sporočilo.
   Da se to sporočilo ne bi videlo v zapiskih, je tu ta celica, ki sproži izpis,
   vendar ima nastavljeno, da je v zapiskih v celoti skrita. *)
```

## Naloga 1

Napišite sintaktično drevo, ki ustreza programom:

```text
#a := 2 + #b
```

```text
IF #x = 2 THEN 
  #x := 3
ELSE
  SKIP
```

```text
WHILE #z > 0 DO 
  #z := #z - 1;
  #w := #z + #w
```

```text
(WHILE #z > 0 DO #z := #z - 1);
#w := #z + #w
```

## Naloga 2

Programe najprej napišite v OCamlu, nato pa jih prevedite v programski jezik IMP s predavanj.

1. Napišite program, ki sešteje vsa naravna števila manjša od `n`.

2. Napišite program, ki preveri ali je podano število praštevilo.

## Naloga 3

Razmislite, kako bi dopolnili sintakso in evaluator jezika IMP za:

1. logična veznika `&&` in `||`,

2. ukaz `SWITCH`, ki zamenja vrednosti dveh lokacij,

3. ukaz `FAIL`, ki prekine izvajanje programa.

## Naloga 4

Izboljšajte razčlenjevalnik, da bo dopolnil nepopolne pogojne stavke. Ukaz `IF b THEN c` naj se prevede v enako sintaktično drevo kot `IF b THEN c ELSE SKIP`.

## Naloga 5

Dopolnite vse dele IMPa s podporo za zanke `FOR` oblike:

```text
FOR #x := 0 TO 100 DO
  cmd
```

Pri tem sta `0` in `100` seveda zgolj zgled poljubnih aritmetičnih izrazov.
