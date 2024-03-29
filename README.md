# CABIN_vv_RCBA

![](Configuration/gc_fr.png) &nbsp;&nbsp; ![](Configuration/rcba_logo.png)

# VÉRIFICATION ET VALIDATION DE DONNÉES RCBA

*[Martin Jean](mailto:martin.jean@canada.ca), Évelyne Paquette-Boisclair et Eva Charbonneau-Bérubé*

*Environnement et Changement climatique Canada*

## Pourquoi cet outil?

Cet outil vous propose une démarche reproductible dans l'évaluation de la qualité de vos données RCBA. Il permet d’importer vos données RCBA (provenant de la base de données du RCBA ou de vos propres fichiers) dans le format *R*. Par la suite, il vous aide à vérifier et valider successivement vos données afin de détecter des problèmes, des irrégularités ou des aberrations potentielles. Il s'applique à l'ensemble de vos données, qu'elles soient générales, biologiques et d'habitat (environnementales). 

Enfin, une analyse sommaire de certaines caractéristiques statistiques de vos données vous est présentée, vous permettant ainsi de bien connaître vos données et mieux profiter du protocole d’analyse statistique prévu dans le RCBA.

## Structure de l’outil

L’outil est composé de cinq carnets R correspondant à l'importation de vos données, puis de l’examen des données générales, biologiques, environnementales (habitat) et des coordonnées géographiques.

Chaque carnet comprend du texte décrivant les étapes et des suggestions d’action sur les résultats obtenus. S’y trouve également le code permettant d’exécuter les commandes *R* ainsi que le résultat de ce code. Toutes ces informations peuvent être enregistrées et visualiser en format HTML interactif ou être sauvegardées en format *Microsoft Word*.

## Exigences préalables

Pour utiliser ces outils, vous devez disposer des éléments suivants :

- un ordinateur fonctionnant sous *Windows*, *macOS*, ou *Linux*;
- le langage *R* version 2.11.1 ou plus récente, disponible [ici](http://cran.r-project.org);
- l’application *RStudio* version 1.0 ou plus récente, disponible [ici](http://www.rstudio.com);
- un éditeur de texte avancé est recommandé (par exemple, *[Notepad++](http://notepad-plus-plus.org)* sous *Windows*, *[BBEdit](http://www.barebones.com/products/bbedit/)* sous *macOS*, ou *[Geany](http://www.geany.org)* sous *Linux*);
- une connexion internet;
- un accès au dossier *CABIN_vv_RCBA*. Ce dossier est diponible sur l'espace de travail partagé sur le réseau interne d'ECCC (accessible à \\\\ec_ontario\\ontfs\\GroupShares\\Bur\\GS2\\CABIN\\CABIN QA-QC\\R Tool\\CABIN_vv_RCBA). Il sera également disponible sur (GitHub)[https://github.com/martinjeanphd/CABIN_vv_RCBA.git].

## Instructions pour démarrer une analyse

Premièrement, nous devons installer les modules externes nécessaires pour *R*

Pour réaliser une analyse de la qualité de vos données RCBA, vous devez procéder comme suit :

- Exporter vos données de la base de données du RCBA ou préparer vos fichiers de données en utilisant les modèles fournis;
- Lancer *RStudio* ;
- Ouvrir le document *LISEZMOI.Rmd*, lisez le contenu, nommer votre projet et exécuter le code permettant de créer une structure de dossiers pour votre projet (voir plus bas);
- Placer vos fichiers de données dans le dossier *Data_Donnees* situé dans votre dossier projet nouvellement créé;
- Créer un projet dans *RStudio* et assignez-lui votre dossier de projet nouvellement créé;
- Procéder à l’importation de vos données au moyen du carnet *RCBA_vv_importation.Rmd*;
- Procéder à l’analyse de vos données générales en utilisant le carnet *RCBA_vv_général.Rmd*;
- Procéder à l’analyse de vos données géographiques en utilisant le carnet *RCBA_vv_spatial.Rmd*;
- Procéder à l’analyse de vos données biologiques en utilisant le carnet *RCBA_vv_biologique.Rmd*;
- Enfin, procéder à l’analyse de vos données environnementales en utilisant le carnet *RCBA_vv_habitat.Rmd*.

## Structure et description des du dossier CABIN_vv_RCBA

###Structure générale du dossier

Le diagramme suivant illustre la structure générale des fichiers contenus dans le dossier *CABIN_vv_RCBA* :

```
CABIN_vv_RCBA
|_
   Configuration
      
|_
   Functions_Fonctions
    
|_
   LISEZMOI.Rmd
    
|_
   Projects_Projets
    
|_
   README.Rmd
    
|_
   Requirements.R
    
|_
   Templates_Modèles
```

Tous les dossiers et fichiers contenus dans cette structure sont essentiels. Vous aurez à modifier le contenu du dossier *Projects_Projets* selon vos besoins.

### Description des dossiers et fichiers

Voici une description sommaire des différents dossiers et documents contenus dans le dossier *CABIN_vv_RCBA*.

**Configuration** : dossier contenant des paramètres généraux sur l'environnement *R*, le fichier contenant la liste des variables, et les images utilisées dans les différents carnets.

**Functions_Fonctions** : dossier contenant un ensemble de fichiers abritant des procédures et fonctions personnalisées. 

**LISEZMOI.Rmd** : le fichier que vous lisez en ce moment.

**Projects_Projets** : dossier contenant les différents projets. Un projet peut correspondre à une analyse particulière de vos données.

**README.Rmd** : la version anglaise du présent fichier.

**Requirements.R** : fichier contenant des commandes relatives à l'installation de modules externes *R* ainsi que les appels à des fichiers de fonctions personnalisées.

**Templates_Modèles** : dossier contenant les modèles de projet en français et en anglais.

## Fonctionnement général

Ouvrez le document **LISEZMOI.Rmd**, un carnet [R Markdown](http://rmarkdown.rstudio.com). Lorsque vous exécuter le code présent dans un tel carnet, les résultats s'affichent sous le code correspondant. 

Pour débuter, remplacer le mot *Test* dans la ligne plus bas par le nom que vous souhaitez donner à votre projet, puis appuyez sur le bouton *Run Current Chunk* (triangle vert à droite) situé à l'intérieur du bloc de code (*chunk*), ou en placant le curseur à l'intérieur du bloc de code puis en tapant *Cmd+Shift+Enter*. 


En enregistrant un carnet R Markdown, un fichier HTML contenant le code et les résultats sera créé dans le même dossier que celui-ci (appuyez sur le bouton *Preview* ou sur les touches *Cmd+Shift+K* pour prévualiser le fichier HTML).

## Références

En plus des nombreuses ressources disponibles sur le Web, les documents suivants ont inspiré le présent travail :

- Borcard, D., Gillet, F. et Legendre, P. (2011). Numerical Ecology with R. New York: Springer. Repéré à http://www.springer.com/us/book/9781441979759

- Roberts, D. R Labs for Community Ecologists. Repéré le 2016-07-14, à http://ecology.msu.montana.edu/labdsv/R/

- Zuur, A. F., Ieno, E. N. et Elphick, C. S. (2010 Mar 01). A protocol for data exploration to avoid common statistical problems. Methods in Ecology and Evolution, 1(1), 3-14. http://dx.doi.org/10.1111/j.2041-210X.2009.00001.x
