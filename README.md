# [QCBS R Workshop Series](https://r.qcbs.ca/)

This series of 10 workshops serves as a guide to use R for a wide array of statistical analyses relevant to research in biology and ecology. These open-access workshops were created by members of the QCBS both for members of the QCBS and the larger community.

The content of this workshop has been peer-reviewed by several QCBS members. If you would like to suggest modifications, please contact the current coordination team, listed [here](https://r.qcbs.ca/about/) or open a pull request (see contributing guidelines at <https://r.qcbs.ca/presenter-developer-protocol/developing-en.html>).

# [Série d'ateliers R du CSBQ](https://r.qcbs.ca/fr/)

Cette série de 10 ateliers servent de guide afin de maîtriser le logiciel R pour une grande variété d'analyses statistiques pertinentes en recherche en biologie et en écologie. Ces ateliers en libre accès ont été créés par des membres du CSBQ à la fois pour les membres du CSBQ et pour la grande communauté d'utilisateurs de `R`.

Le contenu de cet atelier a été révisé par plusieurs membres du CSBQ. Si vous souhaitez y apporter des modifications, veuillez SVP contacter l'équipe de coordination de la série, listé [ici](https://r.qcbs.ca/fr/about/) ou ouvrez un pull request (voir les instructions <https://r.qcbs.ca/presenter-developer-protocol/developper-fr.html>).

# Workshop 8: Generalized additive models

The goal of this workshop will be to first examine what is a non-linear model,
and how Generalized Additive Models (GAM) allow us to handle non-linear
relationships. We will also go over how to plot and interpret these non-linear
relationships, how to include interaction terms, autocorrelated errors and
expand on previous workshop by briefly examining a mixed modelling framework.
Lastly, we will briefly touch upon what GAM are doing behind the scenes.


# Atelier 8 : Modèles additifs généralisés

L'objectif de cet atelier sera d'examiner ce qu'est un modèle non-linéaire et
comment les modèles additifs généralisés (GAM) nous permettent de modéliser les
relations non-linéaires. Nous examinerons également comment visualiser et
interpréter ces relations non-linéaires, comment ajouter des interactions,
comment prendre en compte la non-indépendance des données (e.g. erreurs
auto-corrélées) et comment inclure des effets aléatoires en se basant sur les
ateliers précédents. Enfin, nous allons brièvement aborder la mécanique derrière
le fonctionnement des GAM.

# Workshop materials

Language | Slides | Bookdown | Script | GitHub 
:--------|:-------|:-----|:------ |:-------
EN | [![badge](https://img.shields.io/static/v1?style=flat-square&label=Slides&message=08&color=red&logo=html5)](https://r.qcbs.ca/workshop08/pres-en/workshop08-pres-en.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=book&message=08&logo=github)](https://r.qcbs.ca/workshop08/book-en/index.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=08&color=2a50b8&logo=r)](https://r.qcbs.ca/workshop08/pres-en/workshop08-script-en.R) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop08) 
FR | [![badge](https://img.shields.io/static/v1?style=flat-square&label=Diapos&message=08&color=red&logo=html5)](https://r.qcbs.ca/workshop08/pres-fr/workshop08-pres-fr.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=livre&message=08&logo=github)](https://r.qcbs.ca/workshop08/book-fr/index.html) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=script&message=08&color=2a50b8&logo=r)](https://r.qcbs.ca/workshop08/pres-fr/workshop08-script-fr.R) | [![badge](https://img.shields.io/static/v1?style=flat-square&label=repo&message=dev&color=6f42c1&logo=github)](https://github.com/QCBSRworkshops/workshop08) 

> *Note: The wiki for this workshop was converted to Bookdown in February 2021. <br> The wiki pages for this workshop will no longer be updated (Archive: [EN](https://wiki.qcbs.ca/r_workshop8), [FR](https://wiki.qcbs.ca/r_atelier8)).* 

# License

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/).

[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)

# Contributions

Since 2014, several QCBS members contributed to consistently and collaboratively develop and update this workshop, as part of the *Learning and Development Award* from the Québec Centre for Biodiversity Science. They were:

|      2022 - 2021 - 2020      |      2019 - 2018 - 2017     |      2016 - 2015 - 2014      |
|:----------------------------:|:---------------------------:|:----------------------------:|
| Daniel Schoenig         |  (more to come) | Eric Pedersen  |
|  Laurie Maynard         |   |  Zofia Taranu |
|  Marie-Hélène Brice     |   |  Cédric Frenette Dussault |
|  Kevin Cazelles         |   | Emmanuelle Chrétien  |
|  Pedro Henrique P. Braga|   |  Vincent Fugère |
|  Esteban Gongora        |   |  |
|  Linley Sherin        |   |   |


# Development status

**Template** 

[![receive-from-template-and-dispatch-to-workflows](https://github.com/QCBSRworkshops/workshop08/workflows/receive-from-template-and-dispatch-to-workflows/badge.svg)](https://github.com/QCBSRworkshops/workshop08/actions?query=workflow%3Areceive-from-template-and-dispatch-to-workflows) 

**Building workshop materials**

Language | Slides | Book
:------- | :----- | :-----
EN  | [![render-presentation-en](https://github.com/QCBSRworkshops/workshop08/workflows/render-presentation-en/badge.svg)](https://github.com/QCBSRworkshops/workshop08/actions?query=workflow%3Arender-presentation-en) | [![render-book-en](https://github.com/QCBSRworkshops/workshop08/workflows/render-book-en/badge.svg)](https://github.com/QCBSRworkshops/workshop08/actions?query=workflow%3Arender-book-en)
FR   | [![render-presentation-fr](https://github.com/QCBSRworkshops/workshop08/workflows/render-presentation-fr/badge.svg)](https://github.com/QCBSRworkshops/workshop08/actions?query=workflow%3Arender-presentation-fr) | [![render-book-fr](https://github.com/QCBSRworkshops/workshop08/workflows/render-book-fr/badge.svg)](https://github.com/QCBSRworkshops/workshop08/actions?query=workflow%3Arender-book-fr)
