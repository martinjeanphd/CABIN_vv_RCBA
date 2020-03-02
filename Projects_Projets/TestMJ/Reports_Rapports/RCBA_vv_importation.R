# IMPORTATION DE DONNÉES RCBA
# Version 2.0
# Environnement et changement climatique Canada
 
## Modules et fonctions personnalisées requis
source("../../Required_packages.R")
source("../../Required_functions.R")


## Configuration du projet
source("../../Configuration/project_settings.R")

##Définition des paramètres de base

###Titre de l'analyse

# Exécuter les lignes suivantes sans modification pour définir un titre à votre analyse, de même que la langue et la plateforme.

analysisTitle <- svDialogs::dlgInput("Saisissez un titre pour votre analyse:", default = "Test", Sys.info()["analysisTitle"])$res
if(is.null(analysisTitle) | length(analysisTitle) == 0) {analysisTitle = "Test"} else {if(analysisTitle == "NA") {analysisTitle <- "Test"}}

Language <- "Francais"

Platform <- svDialogs::dlgInput("Choisissez la plate-forme que vous utilisez:", default = "Windows", Sys.info()["Platform"])$res
if (is.null(Platform) | length(Platform) == 0) {
  Platform <- "Windows"
} else {
  if (Platform == "NA") {
    Platform <- "Windows"
  }
}




# Les fichiers CSV sont souvent utilisés autour de logiciels de gestion de bases de donnée ou de tableurs comme *Microsoft Excel*. Un problème potentiel est causé par le fait que les séparateurs ne soient pas standardisés (virgules, points-virgules sous certaines localisations dont la française, etc.). Il faut donc préciser ici comment sont construits vos fichiers CSV. Précisez quel est le caractère utilisé pour séparer les valeurs sur chaque ligne du fichier (SEP) ainsi que le caractère qui définit la marque de décimale (DEC).

# - Les valeurs par défaut en anglais sont généralement SEP="," et DEC="."
# - Les valeurs par défaut en français sont généralement SEP=";" et DEC=","

 # **ATTENTION**: il est important de vérifier si vos fichiers de données CSV contiennent des données qui incluent des sauts de ligne. Si c'est le cas, cela pourrait poser des problèmes de lecture de vos données. Corrigez la situation au besoin avant de continuer.


SEP <- ","
DEC <- "."



###Format de certaines données

# Pour importer les données correctement, le format de certaines données doit être indiqué.

# La base de données RCBA exporte les données en utilisant certains formats.
# C'est le cas pour les données manquantes ainsi que la date.

# Veuillez indiquer la valeur indiquant dans les fichiers une valeur manquante
na.vals <- -99

# Veuillez indiquer le format de la date utilisé dans les fichiers (excluant l'heure).
# 
# Les codes sont:
# %Y: Année avec quatre chiffres (2017)
# %y: Année avec deux chiffres (00-99)
# %m: Mois avec deux chiffres (01-12)
# %B: Mois en caractères (selon la langue de votre système)
# %b: Mois abbrévié en caractères (selon la langue de votre système)
# %d: Jours du mois avec deux chiffres (01-31)
# %A: Nom complet du jour en caractères (selon la langue de votre système)
# %a: Nom abbrévié du jour en caractères (selon la langue de votre système)
dateformat <- "%d/%m/%Y"



###Liste des variables

# Une liste complète de toutes les paramètres généraux et physico-chimiques (à l'exception des variables biologiques) est compilée dans le fichier "VariableList.csv". Ce fichier contient le nom des paramètres en format abbrévié (utilisé dans la base de données du RCBA), le nom complet en français et en anglais, le groupe de paramètres, la forme du paramètre (par exemple, total, dissous, extractible), et l'unité de mesure.


# Exécuter les lignes suivantes sans modification
# 
# Pour ajouter des paramètres au fichier VariableList.csv, veuillez communiquer avec Martin Jean (martin.jean@canada.ca)
param.filename <- "VariableList.csv"
param.master <- read.csv(paste("../../Configuration/", param.filename, sep=""), as.is=TRUE, header=TRUE, sep=",", dec=".")



###Données censurées

# Exécutez le code sans modification
NDmethod <- "ReplaceValue"
replace.value <- 0.5



##Importation des fichiers de données

# Note: ces fichiers doivent se trouver dans votre dossier de projet
envFilename <- "Habitat.csv"
bioFilename <- "Benthic.csv"

    ###Importation des fichiers CSV
    
    
    ####Fichier de données environnementales
    

# Exécutez le code sans modification
all.dataset.ENV <- read.csv(paste("Data_Donnees/", envFilename, sep=""), as.is=TRUE, header=TRUE,
                            na.string=na.vals, sep=SEP, dec=DEC, row.names = 1, encoding = "latin1")
dataset.ENV <- rm.NAcol(all.dataset.ENV)
dataset.ENV <- rm.NArows(dataset.ENV)


# Supprime les premières colonnes de la table Environnement pour ne conserver
# que les données environnementales
dataset.ENV[1:(match("GPSDatum", names(dataset.ENV)))] <- list(NULL)

datatable(dataset.ENV, extensions = c('FixedColumns', 'Scroller'), options = list(dom = 't', scrollX = TRUE, fixedColumns = TRUE, deferRender = TRUE, scroller = TRUE))




####Fichier de données biologiques

all.dataset.BIO <- read.csv(paste("../Data_Donnees/", bioFilename, sep = ""), as.is=TRUE, header=TRUE,
	 			   na.string=na.vals, sep=SEP, dec=DEC, row.names=1, encoding = "latin1")
dataset.BIO <- rm.NAcol(all.dataset.BIO)
dataset.BIO <- rm.NArows(dataset.BIO)


# Supprime les premières colonnes de la table Benthic pour ne conserver
# que les données biologiques
dataset.BIO[1:(match("TotalSample", names(dataset.BIO)))] <- list(NULL)

# Remplace les valeurs NA par 0 dans la table Benthic
dataset.BIO[is.na(dataset.BIO)] <- 0

#Afficher le tableau de données importé
datatable(dataset.BIO, extensions = c('FixedColumns', 'Scroller'), options = list(dom = 't', scrollX = TRUE, fixedColumns = TRUE, deferRender = TRUE, scroller = TRUE))






####Ficher de la liste des visites

# En utilisant les fichiers exportées de la base de données
#   RCBA, créer une liste unifiée des noms d'échantillons (Site, date, et numéro d'échantillon)
    names.BIO <- all.dataset.BIO[, c("Site", "SampleDate", "SampleNumber")]
    names.HAB <- all.dataset.ENV[, c("Site", "SampleDate", "SampleNumber")]
    dataset.NAM <- unique(rbind(names.BIO, names.HAB))
    dataset.NAM <- rm.Time(dataset.NAM)
	dataset.NAM2 <- rownames_to_column(dataset.NAM, var = "rownames")
	dataset.NAM2 <- arrange(dataset.NAM2, Site, SampleDate, SampleNumber)


#Afficher le tableau de données créé
datatable(dataset.NAM)

####Fichier de données générales

Un fichier comprenant les données générales a été créée à partir des fichiers importés plus haut.

```{r, echo=FALSE}
# Créer un fichier dataset.GEN pour les données générales
# Les lignes contenant des valeurs manquantes seront supprimées
dataset.GEN <- all.dataset.BIO[, 1:(match("TotalSample", names(all.dataset.BIO)))]
paste("Fichier de données générales créé à partir d'un extrait du fichier", bioFilename)
genFilename <- paste("Extrait de ", bioFilename, sep="")
dataset.GEN <- rm.NAcol(dataset.GEN)

# Supprime l'heure dans la colonne SampleDate
dataset.GEN <- rm.Time(dataset.GEN)
			cat("Heures supprimées du fichier dataset.GEN", "\n")

```


> ![ACTION:](../../../Configuration/action.png)
> *Vérifier les données et assurez-vous que cela réflète bien la réalité. En particulier, répondez aux questions suivantes :*
>
> - *L'ensemble des colonnes (de « Taxonomist » à « TotalSample ») est-il présent?*
> - *Le nombre de visites de ce fichier (`r nrow(dataset.GEN)`) correspond-il à celui de la liste des visites (`r nrow(dataset.NAM)`), du fichier de données biologiques (`r nrow(dataset.BIO)`) et du fichier de données environnementales (`r nrow(dataset.ENV)`)?*
> - *Les valeurs de la colonne « Date » ont-elles été importées correctement? Correspondent-elles bien aux dates d'échantillonnage?*


```{r, echo=FALSE, warning=FALSE}
#Afficher le tableau de données créé
datatable(dataset.GEN, extensions = c('FixedColumns', 'Scroller'), options = list(dom = 't', scrollX = TRUE, fixedColumns = TRUE, deferRender = TRUE, scroller = TRUE))
```

####Fichier de coordonnées spatiales

Un fichier comprenant les coordonnées géographiques a été créée à partir des fichiers importés plus haut.

```{r, echo=FALSE}
# Exécuter ces lignes tel quel
# Créer un fichier dataset.SPA pour les coordonnées géographiques
# Les lignes contenant des valeurs manquantes seront supprimées
# Une liste des coordonnées est également créée
dataset.SPAb <- all.dataset.BIO[, c("Longitude", "Latitude", "GPSDatum")]
dataset.SPAe <- all.dataset.ENV[, c("Longitude", "Latitude", "GPSDatum")]
dataset.SPAb <- rownames_to_column(dataset.SPAb, var = "rownames")
dataset.SPAe <- rownames_to_column(dataset.SPAe, var = "rownames")
dataset.SPA <- unique(rbind(dataset.SPAb, dataset.SPAe))
dataset.SPA <- arrange(dataset.SPA, rownames)
rownames(dataset.SPA) <- dataset.SPA[, "rownames"]
dataset.SPA2 <- dataset.SPA
dataset.SPA["rownames"] <- NULL
NA.SPA <- get.NArows(dataset.SPA)
NA.SPA <- rownames_to_column(NA.SPA, var = "rownames")
NA.SPA <- inner_join(NA.SPA, dataset.NAM2, by = "rownames")
rm(dataset.SPAb)
rm(dataset.SPAe)
paste("Fichier de coordonnées géographique créé à partir d'un extrait des fichiers", bioFilename, "et", envFilename)
spaFilename <- paste("Extrait de ", bioFilename, "et", envFilename, sep="")
dataset.SPA <- rm.NArows(dataset.SPA)
```

> ![ACTION:](../../../Configuration/action.png)
> *Vérifier les données et assurez-vous que cela réflète bien la réalité. En particulier, répondez aux questions suivantes :*
>
> - *Les valeurs de la colonne « GPSDattum » doivent idéalement être toutes identiques. Est-ce le cas?*
> - *Le nombre de visites de ce fichier (`r nrow(dataset.SPA)`) correspond-il à celui de la liste des visites (`r nrow(dataset.NAM)`), du fichier de données générales (`r nrow(dataset.GEN)`), du fichier de données biologiques (`r nrow(dataset.BIO)`) et du fichier de données environnementales (`r nrow(dataset.ENV)`)?*
> - *Les valeurs des colonnes « Latitude » et « Longitude » doivent être non-nulles. Est-ce bien le cas?*


```{r, echo=FALSE, warning=FALSE}
#Afficher le tableau de données créé
datatable(dataset.SPA)
#rm(all.dataset.BIO)
#rm(all.dataset.ENV)
```


###Enregistrement des fichiers R

# Les cinq fichiers de données (visites, données générales, données environnementales, données biologiques, et coordonnées spatiales) sont sauvegardés en format *RData* dans le sous-dossier *Data_Donnees*.


#Enregistrer le jeu de données créé dans des fichiers .RData pour utilisation ultérieure
save(dataset.ENV, file="../Data_Donnees/dataset.Env.RData")
save(dataset.BIO, dataset.BIO2, file="../Data_Donnees/dataset.Bio.RData")
save(dataset.SPA, file="../Data_Donnees/dataset.Spa.RData")
save(dataset.GEN, file="../Data_Donnees/dataset.Gen.RData")
save(dataset.NAM, file="../Data_Donnees/dataset.Nam.RData")


#Prochaines étapes
# Vos données sont maintenant prêtes à être vérifiées et validées.

# Reportez-vous au document *LISEZMOI.md* pour plus de détails sur la procédure de vérification et de validation de vos donneés.


# Développé par [Martin Jean](mailto:martin.jean@canada.ca) et Evelyne Paquette-Boisclair