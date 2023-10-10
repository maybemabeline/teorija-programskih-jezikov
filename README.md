# Teorija programskih jezikov

V tem repozitoriju se zbirajo gradiva za predmet Teorija programskih jezikov na magistrskem študijuna [Fakulteti za matematiko in fiziko](https://www.fmf.uni-lj.si/).

## Zapiski

Viri zapiskov se nahajajo v mapi `zapiski`. Za izdelavo HTML datotek si morate namestiti paket [`jupyter-book`](https://jupyterbook.org/). Nato pa pokličete

```bash
jupyter-book build zapiski
```

Če imate ustrezne pravice, lahko HTML najenostavneje objavite kar prek [GitHub pages](https://pages.github.com) tako, da si namestite še paket [`ghp-import`](https://github.com/c-w/ghp-import) in poženete

```bash
ghp-import --no-jekyll --no-history --force --push zapiski/_build/html
```
