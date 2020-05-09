_This is a school project, hence the README in French._

# Projet OCaml - Rummikub

Implémentation d'une version simplifiée du Rummikub en OCaml. Spécification [ici](http://www-verimag.imag.fr/~bassetni/INF201/inf201_projet_Rummikub_etd-2x1.pdf) ([archive](https://web.archive.org/web/20200509195316/http://www-verimag.imag.fr/~bassetni/INF201/inf201_projet_Rummikub_etd-2x1.pdf)).

## Lancement

Comme nous utilisons des modules OCaml (instruction `open`), il est nécéssaire de compiler chaque fichier individuellement après modification. Pour faciliter le travail, un petit script de build a été écrit. Il essaye de trouver les instructions `open` de chaque fichier inclus à partir du point d'entrée et calcule l'ordre dans lequel les fichiers doivent être chargés (impossible d'avoir des dépendances récursives). Enfin, il lance le point d'entrée en passant tous les objets compilés en paramètres.

 - `ocaml build.ml` - lance le projet en entier (point d'entrée: main.ml)
 - `ocaml build.ml [fichier_en_particulier]` - lance un fichier en particulier

## Débogage avec le Toplevel

Le plus simple est de lancer le projet avec la commande ci-dessous. On verra alors dans le terminal une ligne de la forme `$ ocaml multiensemble.cmo tuiles.cmo combinatoire.cmo main.ml`. Il suffit de relancer cette commande _sans le dernier argument_ (le point d'entrée), ce qui ouvre le toplevel interactif. Ensuite, on inclus le fichier *.ml qu'on vient d'enlever avec `#use "[le_fichier_ml_qu_on_vient_d_enlever]";;`.
