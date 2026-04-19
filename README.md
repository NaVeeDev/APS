# Projet d'APS

#### Etudiante

Laura LY (M1 STL - 21500152)

## Etat du travail

Ce projet recouvre toutes les fonctionnalités de APS0 jusqu'à APS3, ie ce projet implémente l'intégralité d'APS3.

Il y a séparation entre procédures (peuvent prendre des arguments par référence, le corps doit entièrement être fait d'instructions, le type de retour est void et doit être appelé précédé par le token `CALL` ), fonctions (ne peuvent pas prendre des arguments par reference, le corps doit être fait d'expressions, et le type de retour n'est pas void) et fonctions procédurales (ne peuvent pas prendre des arguments par référence, le corps doit être fait d'instructions, et le type de retour n'est pas void).

Il y détection de code mort. Cette dernière est faite lors du parsing, puisque que les `RETURN` ne peuvent se trouver qu'en fin de bloc. Ainsi, tout code comportant du code mort est rejeté par le parser (voir `examples/APS3/prog3.aps`).

## Commandes

### Pour lancer l'intégralité des tests

> ```dune exec tests```

### Pour lancer les tests sur un fichier particulier

> ```dune exec aps "examples/APSX/progY.aps"```  

avec `X` la version d'APS (du test) et `Y` le numéro de fichier correspondants (voir le dossier `examples/`)

### Pour lancer plusieurs tests spécifiques

> ```dune exec -- tests --options1 --options2 ... --optionsX```

Il est possible d'ajouter les options suivantes:

- ```--apsX``` avec `X` pouvant être 0, 1, 1a, 2 ou 3 correspondants aux dossiers dans `examples/`. Cette option permet de lancer tous les tests du dossier choisi
- ```--semantic``` permet d'afficher uniquement les tests d'évaluation
- ```--typer``` permet d'afficher uniquement les tests de typage
- ```--prologTerm``` permet d'afficher uniquement les tests de pretty printer

Il est possible de combiner plusieurs options. Ainsi:  
> ```dune exec -- tests --semantic --prologTerm --aps0 --aps2```  

affiche les tests d'evaluation et de pretty printer sur les fichiers tests de aps0 et aps2.

Note: Pour les tests du typer, pour faciliter la visualisation, le nom du programme est affiché sur fond vert si le résultat correspond à celui attendu, en rouge sinon, et sur fond noir si il y a erreur à une étape antérieure au typage (lexer ou parser).

## Difficultés

Les difficultés rencontrées lors du travail sur ce projet ont tout d'abord été liées au manque de familiarité avec le langage Prolog, notamment le système d'abstraction des variables et d'unification ont ralenti le début du projet. D'autres difficultés ont émané du manque de clarté des erreurs de Menhir lors du parsing, alors que celles-ci étaient souvent liée à une faute de frappe dans le programme (un `;` à la place de `,` par exemple) plutôt qu'une faute d'implémentation de la grammaire. Enfin, bien que le cours sur APS2 ait beaucoup aidé à la compréhension de l'extension, de légères confusions personnelles sur l'utilisation de certains opérateurs ont pu également faire obstacle à une avancée fluide. La lecture de certains exemples dans les annales mises à disposition a pu grandement aider à ce sujet.
