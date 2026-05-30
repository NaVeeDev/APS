# Projet d'APS

### Note/Grade: 100/100

<sub> English version [here](#aps-project---implementing-a-full-interpreter)!</sub>

Ce projet a été conçu dans le cadre de ma première année de master STL à Sorbonne Université (2025-2026).

#### Etudiante

Laura LY (M1 STL - 21500152)

## Etat du travail

Ce projet recouvre toutes les fonctionnalités de APS0 jusqu'à APS3, ie ce projet implémente l'intégralité d'APS3. (La description complète de chaque version d'APS est disponible dans le dossier `notes/`)

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

<br> <br>


# APS Project - Implementing a full interpreter
This project was implemented during my first year's second semester of Master STL at Sorbonne Unniversité (2025-2026)

## Current state
This project covers all functionalities from APS0 to APS3, which means all of APS3 have been successfully implemented. Details of all APS's versions are available in the `notes/` folder.

There is a distinction between procedures (can take arguments by reference, the body must consist entirely of statements, the return type is void, and they must be called preceded by the `CALL` token), functions (cannot take arguments by reference, the body must consist of expressions, and the return type is not void), and procedural functions (cannot take arguments by reference, the body must consist of statements, and the return type is not void).

There is dead code detection as well. This is done during parsing, since `RETURN` statements can only appear at the end of a block. Therefore, any code containing dead code is rejected by the parser (see `examples/APS3/prog3.aps`).


## Commands

### Launching all tests
> ```dune exec tests```

### Launching tests on a specific file
> ```dune exec aps "examples/APSX/progY.aps"```  

with `X` being APS's version (for the test) and `Y` the file number (see the `examples/` folder)

### Launching specific tests
> ```dune exec -- tests --options1 --options2 ... --optionsX```

Below are the available options:

- ```--apsX``` with `X` being 0, 1, 1a, 2 or 3, which corresponds to the folders in `examples/`. This option allows the user to launch all the tests in the chosen folder
- ```--semantic``` displays the evaluation test results
- ```--typer``` displays the typing test results
- ```--prologTerm``` displays pretty printer tests

One can use multiple options at once. For instance:  
> ```dune exec -- tests --semantic --prologTerm --aps0 --aps2```

displays evaluation and pretty printer test results for APS0 and APS2 test files.

Note: To make typer tests more understandable, the program's name is displayed on a green background if its test result is the expected one, red if not, black if there is an error at an earlier styep (lexer or parser).

## Difficulties
The difficulties encountered while working on this project were firstly related to a lack of familiarity with the Prolog language, particularly the variable abstraction system and unification, which slowed down the start of the project. Other difficulties arose from the lack of clarity in Menhir's error messages during parsing, as these were often linked to a typo in the program (a ; instead of a , for example) rather than a mistake in the grammar implementation. Finally, although the APS2 lecture greatly helped in understanding the extension, some minor personal confusion about the use of certain operators also hindered smooth progress. Reading some examples from the past exams that were provided helped greatly in this regard.
