

# Calendrier

**28/01/2026** Fin de la première séance, rendre le lien du dépot git du projet

**15/04/2026** Dernière séance, présentation **obligatoire** de l'avancement
du projet

**22/04/2026** Date limite pour rendre le projet

Il est fortement recommandé de faire des petits *commits* réguliers,
qui **expliquent** ce qui a été fait. Toute trace de travail sérieux et régulier
sera valorisée.

# Modalités de rendu

Le rendu se fera via un dépot git.
En plus du code du projet, vous devrez remplir

- le fichier `who.md`
- un `Readme.md` qui présentera l'état de votre travail / les différentes
    commandes pour l'utiliser / les difficultés rencontrées

# Dépôts

Vous pouvez utiliser pour cela le dépot gitlab STL :

https://stl.algo-prog.info/

(**attention** il faudra sauvegarder votre projet ailleurs après la fin
du semestre si vous souhaitez en conserver une trace -le git est mis à 0 tous
les ans-)

Sinon, vous pouvez faire un dépot **privé** sur votre hébergeur de git préféré
Ensuite, il faudra m'ajouter en *maintainer* sur le projet

- sur le gitlab STL : carbonneaud
- sur gitlab (compte public) : DanaelCarbonneau
- sur github : DanaelCarbonneau
- sur codeberg : danael_carbonneau

# Squelette de code / langages de programmation utilisables

Un squelette de code complet vous est proposé en OCaml sur moodle. Vous êtes
libres de ne pas vous en servir.

L'utilisation de Prolog est obligatoire pour l'écriture du typeur, pour le reste
du projet, le choix du langage est libre, tant que votre base de code est 
claire et facilement utilisable (y compris pour quelqu'un qui ne connaît 
potentiellement pas le langage utilisé).

# Ce qu'il faut faire

Pour chaque version d'APS, il faudra implémenter

- L'analyse de la syntaxe
- Le typage, en encodant les règles de typage vues en cours en prolog
- La sémantique, en écrivant un interprète
- Écrire des programmes en APS afin d'en tester le typage et la sémantique


# Passage d'une version d'APS à une autre

Il n'y a pas besoin de rendre toutes les versions d'APS que vous avez faites,
seulement la plus récente dans votre avancement comptera.
Cependant, en passant d'un APS à un autre, il est recommandé de garder une
version **facilement** accessible pour vous d'où vous en étiez. Vous pourrez
par exemple utiliser le système de *release* de gitlab/github, ou bien de
faire des branches.

# Évaluation

Vous serez noté.e.s sur l'avancement de votre projet, les potentielles
extensions, la qualité et le contenu de votre rapport, votre correction par
rapport à la spécification, et, à moindre échelle, sur le professionnalisme
de l'implémentation (automatisation des tests, qualité et granularité des
commits, facilité d'appel du typeur/interpréteur).

Vous serez notés par groupe (pas d'individualisation des notes par membre
des binômes).


# Triche

## Utilisation de LLMs conversationnels

Les LLMs conversationnels sont une **source** qui n'est ni vous, ni le polycopié
de cours. Tout travail emprunté à une **source** sans la citer est considéré
comme du **plagiat**.

Nous ne pouvons pas vous forcer à ne pas les utiliser, cependant, utiliser une
source extérieure, quelle qu'elle soit, vous oblige à la citer, précisément.
Qu'il s'agisse d'extraits de code récupérés de *stack overflow* ou bien générés
avec un LLM, il faut les citer précisément (lien en commentaire, *logs* de vos
prompts).

Cependant, peu importe son utilisation, le code rendu est **le vôtre**. Vous
devez pouvoir en expliquer **chaque** ligne, et les comprendre.

## Copie de projets

Le projet est à réaliser strictement en binome. Si vous avez recopié le projet
d'un autre groupe, la note finale sera divisée de manière à refléter le travail
réellement effectué.