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

S semantiko malih korakov izračunajte zaporedje korakov, ki izračunajo vrednost izrazov:

1. `(2 * 2) + (3 * 3)`
2. `(fun x -> if x + 1 > 0 then x else 0) 12`
3. `(rec f x. if x = 0 then 1 else x * f (x - 1)) 3``

## Naloga 2

Zgornja operacijska semantika sledi neučakanemu izvajanju, saj pri uporabi funkcije najprej izračunamo vrednost argumenta in dobljeno vrednost substituiramo v telo funkcije. Pri lenem izvajanju pa pri uporabi funkcije argument še neizračunan substituiramo v telo funkcije. Popravite semantiko na leno izvajanje (potrebno je spremeniti zgolj pravila za aplikacijo funkcij) ter primerjajte izvajanje na primerih:

1. `(fun x -> (fun y -> if x > 0 then y else 0)) 1 (4*((4+2)*3))`
2. `(fun x -> (x * x) + x) (3*(2+(4*0)))`

## Naloga 3

Preverite tipe izrazov. Izrazi morda nimajo primernega tipa. V tem primeru poiščite mesto, kjer se postopek zatakne.

  1. `b:bool, x:int |- 1 + (if b then 2 else x) : int`
  2. `|- fun x -> (fun y -> x > y) : int -> int -> bool`
  3. `|- (rec f x -> if x then f x else 0) true : int`
  4. `f : int -> int |- (f 3) > (f (f 0)) 2 : bool`

## Naloga 4

Napišite nekaj izrazov, katerim ni možno dodeliti tipa, vendar se izračunajo v vrednost.

## Naloga 5

Razširite jezik in sistem tipov s pari, seznami in vsotami. Za pare dodajte projekciji na posamezno komponento, za sezname in vsote pa dodajte razgrajevalnik `match`.

Kot dodaten izziv premislite, kako razširitve narediti v lenem izvajanju in napišite funkcijo, ki zgradi neskončen seznam ničel.

## Naloga 6

V jeziku iz naloge 3 poiščite primeren tip za spodnji izraz in ustreznost preverite z izpeljavo, v primeru ko sta s `fst` in `snd` označeni projekciji na prvo in drugo komponento para.

``` (fun p -> (match fst p with [] -> true | x :: xs -> snd p)) (1::2::[], false) ```

## Naloga 7

Pokažite, kako lahko v jezik dodamo medsebojno rekurzivne funkcije.
