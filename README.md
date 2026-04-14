# Projet d'APS

#### Etudiante

Laura LY (M1 STL - 21500152)

## Etat du travail

Ce projet recouvre toutes les fonctionnalités de APS0 jusqu'à APS2, ie ce projet implémente l'intégralité d'APS2.

## Commandes

### Pour lancer l'intégralité des tests

> ```dune exec tests```

### Pour lancer les tests sur un fichier particulier

> ```dune exec aps "examples/APSX/progY.aps"```  

avec `X` la version d'APS (du test) et `Y` le numéro de fichier correspondants (voir le dossier `examples/`)

### Pour lancer plusieurs tests spécifiques

> ```dune exec -- tests --options1 --options2 ... --optionsX```

Il est possible d'ajouter les options suivantes:

- ```--apsX``` avec `X` pouvant être 0, 1, 1a ou 2, correspondants aux dossiers dans `examples/`. Cette option permet de lancer tous les tests du dossier choisi
- ```--semantic``` permet d'afficher uniquement les tests d'évaluation
- ```--typer``` permet d'afficher uniquement les tests de typage
- ```--prologTerm``` permet d'afficher uniquement les tests de pretty printer

Il est possible de combiner plusieurs options. Ainsi:  
> ```dune exec -- tests --semantic --prologTerm --aps0 --aps2```  

affiche les tests d'evaluation et de pretty printer sur les fichiers tests de aps0 et aps2.

## Difficultés

Les difficultées rencontrées lors du travail sur ce projet ont tout d'abord été liées au manque de familiarité avec le langage Prolog, notamment le système d'abstraction des variables et d'unification ont ralenti le début du projet. D'autres difficultés ont émané du manque de clarté des erreurs de Menhir lors du parsing, alors que celles-ci étaient souvent liée à une faute de frappe dans le programme (un `;` à la place de `,` par exemple) plutôt qu'une faute d'implémentation de la grammaire. Enfin, bien que le cours sur APS2 ait beaucoup aidé à la compréhension de l'extension, de légères confusions personnelles sur l'utilisation de certains opérateurs ont pu également faire obstacle à une avancée fluide. La lecture de certains exemples dans les annales mises à disposition a pu grandement aider à ce sujet.
