# ProgFonc-Projet

##### Table of Contents
* [Français](#fr)
  * [Présentation](#fr_pr)
  * [Utilisation](#fr_ut)
  * [Compétences acquises](#fr_cp)
  * [Résultat](#fr_rs)
* [English](#en)
  * [Presentation](#en_pr)
  * [Use](#en_u)
  * [Skills acquired](#en_sk)
  * [Result](#en_rs)

<a name="fr"/>

## Français

<a name="fr_pr"/>

### Présentation

Ce projet a été effectué en troisième année du [CMI Informatique](http://formations.univ-poitiers.fr/fr/index/autre-diplome-niveau-master-AM/autre-diplome-niveau-master-AM/cmi-informatique-JD2XQGVY.html) à l'[UFR SFA Université de Poitiers](https://sfa.univ-poitiers.fr/) dans le cadre de l'enseignement [Programmation fonctionnelle](http://formations.univ-poitiers.fr/fr/index/autre-diplome-niveau-master-AM/autre-diplome-niveau-master-AM/cmi-informatique-JD2XQGVY/specialite-s6-K5C7D86V/programmation-fonctionnelle-et-theorie-des-langages-JB1YIY7Z/programmation-fonctionnelle-JBID7FWA.html).

Ce projet a été développé en binôme sous Ubuntu avec [GNU Emacs](https://www.gnu.org/software/emacs/) et le mode [Tuareg](https://github.com/ocaml/tuareg).

<a name="fr_ut"/>

### Utilisation

Sous linux lancer le programme 'exec_project'.

Ensuite, le programme attend sur l'entrée standard une suite d'expressiond arithmétiques délimitées par des point-virgules sous la forme [notation polonaise inverse](https://fr.wikipedia.org/wiki/Notation_polonaise_inverse).

Un exemple d'utilisation :

```shell
$ ./exec_project
34 56 2 + x * -;                                      
x 3 + 5 7 + + 3 4 * 1 3 + / /;
4 ~ 3 *;
^D
```

Donne le résultat :

```
Expression numéro 1 avant simplification :
34 - (56 + 2) * x
Expression numéro 1 après simplification :
34 - 58 * x

Expression numéro 2 avant simplification :
(x + 3 + 5 + 7) / ((3 * 4) / (1 + 3))
Expression numéro 2 après simplification :
(x + 3 + 12) / 3

Expression numéro 3 avant simplification :
(-4) * 3
Expression numéro 3 après simplification :
(-4) * 3
```

<a name="fr_cp"/>

### Compétences acquises

* Notion de types
  * Typage des expressions
  * Inférence de type
  * Curryfication
* Fonctionnelles
  * Filtrage
  * Récursivité
  * Lambda-calcul
* Stratégies d'évaluations
* Analyse syntaxique

<a name="fr_rs"/>

### Résultat

Nous avons obtenu la note de ?/20. (Résultat en juin)

<a name="en"/>

## English

<a name="en_pr"/>

### Presentation

This project was carried out in the third year of the [CMI Informatique](http://formations.univ-poitiers.fr/fr/index/autre-diplome-niveau-master-AM/autre-diplome-niveau-master-AM/cmi-informatique-JD2XQGVY.html) at the [University of Poitiers](https://www.univ-poitiers.fr/en/) as part of the [Functional programming](http://formations.univ-poitiers.fr/fr/index/autre-diplome-niveau-master-AM/autre-diplome-niveau-master-AM/cmi-informatique-JD2XQGVY/specialite-s6-K5C7D86V/programmation-fonctionnelle-et-theorie-des-langages-JB1YIY7Z/programmation-fonctionnelle-JBID7FWA.html) teaching programme.

This project was developed in pairs under Ubuntu with [GNU Emacs](https://www.gnu.org/software/emacs/) and [Tuareg](https://github.com/ocaml/tuareg) mode.

<a name="en_u"/>

### Use

Under linux run the program 'exec_project'.

Then the program expects a sequence of semicolon-delimited arithmetic expressionsd in the form [reverse Polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation) on the standard input.

An example of usage:

```shell
$ ./exec_project
34 56 2 + x * -;                                      
x 3 + 5 7 + + 3 4 * 1 3 + / /;
4 ~ 3 *;
^D
```

Gives the result:

```
Expression numéro 1 avant simplification :
34 - (56 + 2) * x
Expression numéro 1 après simplification :
34 - 58 * x

Expression numéro 2 avant simplification :
(x + 3 + 5 + 7) / ((3 * 4) / (1 + 3))
Expression numéro 2 après simplification :
(x + 3 + 12) / 3

Expression numéro 3 avant simplification :
(-4) * 3
Expression numéro 3 après simplification :
(-4) * 3
```

<a name="en_sk"/>

### Skills acquired

* Notion of types
  * Typing of expressions
  * Type inference
  * Curryfication
* Functional
  * Filtering
  * Recursion
  * Lambda calculus
* Evaluation strategies
* Syntactic analysis
  
<a name="en_rs"/>

### Result

We obtained a score of ?/20. (Result in June)
