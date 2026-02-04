# Prise en main

*Si vous pensez savoir quoi faire, vous pouvez directement ajouter toute la*
*syntaxe de APS0, écrire les règles de typage et l'interprète.*

Après avoir mis en place correctement le projet (cf `setup_projet.md), et pris
connaissance des différents fichiers

* Écrire les règles de typage pour le sous-ensemble d'APS0(dont la syntaxe
    est traitée par le squelette du projet) dans `lib/typing_rules.pl`.

* Typer à la main `examples/APS0/prog1.aps` et l'ajouter à la liste de tests

* Vérifiez que vos deux premiers exemples sont typés correctement
    * En cas de difficultés à cette étape, vous pouvez lancer prolog pour
    tester vos règles une par une avec la commande suivante (Pour plus 
    d'informations, cf. `memento_prolog.md`.) :

```$swipl lib/typing_rules.pl```


* Écrire un nouvel interprète, sur le même modèle que `prologTerm`, mais qui
    traduit cette fois les règles de **sémantique** (*ie.* comment 
    s'exécutent ces programmes)
* Adaptez les codes des deux modes de test afin d'interpréter les programmes
    APS **dont le typage est correct**.

* Étendre la syntaxe à tout APS0, ajoutez les règles de typage, l'affichage
    approprié dans `lib/prologTerm.ml` ainsi que leur interprétation. Ajoutez
    les tests appropriés (y compris en écrivant de **nouveaux** programmes APS).




