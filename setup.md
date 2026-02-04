# Installation

Pour faire marcher ce squelette, il faut installer

* swipl (normalement déjà installé à la PPTI), environnement pour le langage
    Prolog.
    cf https://www.swi-prolog.org/Download.html

* `opam`, le gestionnaire de paquets d'OCaml,
    cf https://opam.ocaml.org/doc/Install.html. Toutes les dépendances
    du projet seront à installer via opam

* `dune`, un *build system* pour OCaml

```$ opam install dune```

* `menhir`, un générateur de *parsers* pour Ocaml

```$ opam install menhir```

* `bos`, une bibliothèque qui permet d'inter-agir avec le système (utilisé
    pour faire les tests et les appels à prolog)

```$ opam install bos```

Une fois toutes ces installations effectuées, vous devriez pouvoir *build* le
squelette du projet avec la commande

```$ dune build```

sans recevoir de messages d'erreur.


# Lancer les tests

Il y a dans le projet deux exécutables :

* Un qui permet de lancer une série de tests donc le résultat est connu à
    l'avance. Son code est dans `bin/test_aps.ml`. Il se lance avec
    la commande suivante :

```$ dune exec tests```

* Un qui permet de tester un seul fichier à la fois (le chemin doit être donné 
    en argument de l'exécutable). Son code est dans `bin/main.ml`. 
    Il se lance avec la commande suivante :

```$ dune exec aps "examples/APSX/progX.aps"``` 


# Coloration syntaxique pour APS dans VSCode.

Si vous avez besoin de coloration syntaxique pour écrire plus facilement
vos exemples, 

https://gitlab.com/DanaelCarbonneau/extension-vscode-aps