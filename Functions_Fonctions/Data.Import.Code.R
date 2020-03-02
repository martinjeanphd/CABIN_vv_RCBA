# Data.Import.Code.R

###################################################################
# FUNCTION TO IMPORTing and Manipulating datasets
#
# Author: Jennifer MacDonald Date:  15 March 2013 Version: 1.0
# Modified by: Martin Jean Date: July 11 2016 Version 1.1
#
#------------------------------------------------------------------
# FONCTION POUR L'IMPORTATION ET LA MANIPULATION DE JEUX DE DONNÉES
#
# Auteur: Jennifer MacDonald Date:  15 mars 2013 Version: 1.0
# Modified by: Martin Jean Date: 1 juillet 2015 Version 1.1
#
##################################################################


Data.Import <- function(homedir, datadir, resultsdir, dateformat, filename,
                        param.master, na.vals, replace.value, NDmethod, NSims,
                        SEP, DEC) {

    #===============================================================================
    # STEP 1: SOURCE SUPPORTING CODE
    #
    #-------------------------------------------------------------------------------
    #
    # ÉTAPE 1: RÉFÉRENCER LE CODE REQUIS
    #===============================================================================

    # MOVED THIS TO REQUIRED.PACKAGES.R code to define prior to running any of the code and functions...
    #
    # DÉPLACER ceci vers le code REQUIRED.PACKAGES.R pour définir avant l'analyse les fonctions...
    #setwd(functiondir)
    #source(read.data._wDates.R)
    #source(add.rm.columns.R)
    #source(SplitDL.R)
    #source(N.per.seas.R)
    #source(ReplaceND.R)
    #setwd(homedir)


    #===============================================================================
    # STEP 2: READ IN THE .csv DATASET
    #
    #-------------------------------------------------------------------------------
    #
    # ÉTAPE 2: LIRE LE FICHIER .csv
    #===============================================================================

      #----------------------------------------------------------------------------
      # NON_FONCTIONAL
      # CALLS THE READ.DATA_wDates to read in .csv file
      #   - read in csv file (filename)
      #   - formats Date as dateformat
      #   - replaces missing values with NAs
      #
      # RETURNS: DF with parameters by column and Date, StationID
      #
      # Note: parameters with <MDL will be of class character
      #
      #-------------------------------------------------------------------------------
      #
      # NON-FONCTIONNEL
      # UTILISER READ.DATA_wDates pour lire le fichier .csv
      #   - lire le fichier csv
      #   - formater la date comme défini par dateformat
      #   - remplacer valeurs manquantes par NA
      #
      # RETOURNE: DF avec les paramètres par colonne et Date, StationID
      #
      # Note: les paramètres avec <LDM seront de la classe caractère
      #----------------------------------------------------------------------------

    setwd(datadir)

    all.dataset <- read.csv(filename, as.is=TRUE, header=TRUE,
                   na.string=na.vals, sep=SEP, dec=DEC, row.names=1)



    #all.datasets <- read.data_wDates(datadir, filename, dateformat, na.vals, SEP, DEC)
    #head(all.datasets)


    #===============================================================================
    # STEP 3: FORMAT THE DATASET
    #
    #-------------------------------------------------------------------------------
    #
    # ÉTAPE 3: FORMATER LE JEU DE DONNÉES
    #===============================================================================

    #TO SAVE OUTPUT TO A LOG FILE (split=TRUE means output also shows on the screen)
    #
    #POUR ENREGISTRER LA SORTIE DANS UN FICHIER LOB (split=TRUE signifie que la sortie est aussi affichée à l'écran)
    setwd(dataSUBdir)
    sink("LOG.DataFormatting.txt", append=TRUE, split=TRUE)

    cat("===========================================", "\n")
    if (Language == "Francais") {
        cat("Fichier", filename, "importé", "\n")
    } else {
        cat("File", filename, "imported", "\n")
    }

    cat("===========================================", "\n"); cat("\n")
if (Language == "Francais") {
        cat("INFORMATION TOUCHANT LES ETAPES DE FORMATAGE DES DONNEES", "\n")
    } else {
        cat("INFORMATION FROM DATA FORMATTING STEPS", "\n")
    }

      #----------------------------------------------------------------------------
      # Apply rm.NAcol function to remove columns containing ONLY NA data
      #
      #-------------------------------------------------------------------------------
      #
      # Utiliser la fonction rm.NAcol pour supprimer les colonnes ne contenant que des données NA
      #----------------------------------------------------------------------------
      all.dataset <- rm.NAcol(all.dataset)


      #----------------------------------------------------------------------------
      # SPLIT OUT "<" VALUES using SplitDL
      # Returns a list of DF with obs, cen, Date  where obs = value, cen=TRUE or FALSE
      #
      # DIVISER LES VALEURS "<" à l'aide de SplitDL
      # Retourne une liste de dataframe avec obs, cen, Date où obs=valeur, cen=VRAI ou FAUX
      #----------------------------------------------------------------------------

      #dataset.cen.df <- SplitDL(all.dataset)
      dataset.cen.df <- all.dataset

      #----------------------------------------------------------------------------
      # IDENTIFY additional parameters: paramlist, stationID
      # and assign to the .GlobalEnv
      #
      # IDENTIFIER les paramètres additionnels: paramlist, stationID
      # et assigner à .GlobalEnv
      #----------------------------------------------------------------------------

      #paramlist = vector of all parameter names
      #
      #paramlist = vecteur contenant tous les noms de paramètres

      #   paramlist <- names(dataset.cen.df)
      #
      #stationID <- unique(all.dataset$StationID)


      #assign("paramlist", paramlist, envir = .GlobalEnv)
      #assign("stationID", stationID, envir = .GlobalEnv)

      #----------------------------------------------------------------------------
      # Add additional columns into each df in list
      # (functions stored in add.rm.columns.R)
      #   - flow column
      #   - Year, Month, MonthName
      #   - Parameter and StationID
      #   - Variable Group
      #   - Units
      #
      # Ajouter des colonnes additionnelles pour chaque dataframe dans la liste
      # (fonctions enregistrées dans add.rm.columns.R)
      #   - colonne débit
      #   - Année, Mois, NomMois
      #   - Paramètre et StationID
      #   - Groupe de variable
      #   - Unités
      #----------------------------------------------------------------------------

       #---------------------------------------------------------------------------
      # assign dataset.cen.df to .GlobalEnv
      #
      # assigner dataset.cen.df à .GlobalEnv
      #----------------------------------------------------------------------------

      #assign("dataset.cen.df", dataset.cen.df, envir = .GlobalEnv)


    #===============================================================================
    # Step 4:  REMOVE ROWS WITH NO DATA (obs = NA)
    #   NOTE: AFTER this step each df may be different lengths
    #
    # ÉTAPE 4: SUPPRIMER LES LIGNES NE CONTENANT PAS DE DONNÉES (obs = NA)
    #   NOTE: APRÈS cette étape chaque dataframe aura une longueur différente
    #===============================================================================

    #removed rows where all obs=NA; supprime les colonnes où toutes les obs=NA
    #dataset.cen.NA <- llply(all.dataset,rm.NArows)

    #removed rows where all obs=NA; supprime les colonnes où toutes les obs=NA
    dataset.cen.NA <- rm.NArows(all.dataset)
    #str(dataset.cen.NDNA)

    #assign("dataset.cen.NA", dataset.cen.NA, envir = .GlobalEnv)




#===============================================================================
    # STEP 5: REPLACE Non-detects WITH A VALUE
    #
    #   OPTIONS:
    #   1) Replace all values with %of value (in decimal).
    #      - Use replace.value=0.5 for half the DL
    #      - Use replace.value=0 to replace with zero.
    #   2) Multiple imputation with random value between 0 and DL
    #
    #  RETURNS a list of df with Data, obs, cen, Date, Flow, ... but where
    #      Data_MDL? = obs where been replaced with selected value IF cen=TRUE
    #
    #-------------------------------------------------------------------------------
    #
    # ÉTAPE 5: REMPLACER Non détecté PAR UNE VALEUR
    #
    #   OPTIONS :
    #   1) Remplacer toutes les valeurs par un % d'une valeur (décimale)
    #      - Utiliser replace.value=0.5 pour la moitié de la limite de détection
    #      - Utiliser replace.value=0 pour remplacer la valeur par 0
    #   2) Imputation multiple avec des valeurs aléatoires entre 0 et la limite
    #      de détection
    #
    #   RETOURNE une liste de dataframe avec Données, obs, cen, Date, Débit, ... mais
    #      par Data_MDL? = obs lorsque remplacé par la valeur sélectionnée si cen=TRUE
    #===============================================================================


    # NON FUNTIONNAL #####################################
    # if (NDmethod == "ReplaceValue") {
    #   #dataset.cen.NDNA <- llply(dataset.cen.NA, replaceND, replace.value)
    #   dataset.cen.NDNA <- replaceND(dataset.cen.NA, replace.value)
    # } else if (NDmethod == "MultipleImputation") {
    #   dataset.cen.NDNA <- llply(dataset.cen.NA, Impute.ND, NSims)
    # }
    #######################################################

    #output will be different depending on method:
    #for REplaceValue = list of df (1 per variable)
    #for MultipleImputation = list of lists (1 list per variables with a NSims
    #   list of dataframes)
    #
    #la sortie sera différente en fonction de la méthode utilisée :
    #pour ReplaceValue = liste des dataframes (1 par variable)
    #pour MultipleImputation = liste de listes (1 liste par variable avec une
    #   NSims listes de dataframes)

    #===============================================================================
    # STEP 7: Log-transform the dataset (natural log)
    #         (adds a Data.log column to each dataframe)
    #
    # Could add other transformation options later.
    #
    # ÉTAPE 7 : transformation log du jeu de données (log naturel)
    #           (ajoute une colone Data.log dans chaque dataframe)
    #
    # D'autres transformations pourront être ajoutées plus tard.
    #===============================================================================
    #WARNING:
    #log(-x) = NA  #NAs will be produced if negative numbers exist
    #log(0) = -Inf
    #
    #ATTENTION :
    #log(-x) = NA  #Les valeurs NA seront produites si un nombre négatif existe
    #log(0) = -Inf



    # cat("\n")
    # if (Language == "Francais"){
    #   cat("VALEURS DU PARAMÈTRE PLUS PETITES OU ÉGALES À ZÉRO", "\n")
    # } else {
    #   cat("PARAMETER VALUES LESS THAN OR EQUAL TO ZERO:", "\n")
    # }
    # cat("-------------------------------------------------", "\n")
    #
    # if (NDmethod=="ReplaceValue"){
    #
    #   ds.zero <- ldply(dataset.cen.NDNA, function(x) {
    #                       if(nrow(subset(x, x$Data<=0))>0) {tmp <- subset(x, x$Data<=0); tmp}
    #                     })
    #   nm.zero <- unique(ds.zero$Parameter)
    #   if (Language == "Francais"){
    #     cat(length(nm.zero), " paramètre(s) contient(contiennent) des valeurs plus petites ou égales à zéro : ","\n"); for (k in 1: length(nm.zero)){cat(nm.zero[k], "\n")}
    #   } else {
    #     cat(length(nm.zero), " parameter(s) contain values less than or equal to zero: ","\n"); for (k in 1: length(nm.zero)){cat(nm.zero[k], "\n")}
    #   }
    #
    #   dataset.cen.NDNA.log <- llply(dataset.cen.NDNA, function(df) {
    #     df$Data.log <- log(df$Data)
    #     col_idx <- grep("Data.log", names(df))
    #     df <- df[, c(col_idx, (1:ncol(df))[-col_idx])]   #Moves Data.log column to 1st column
    #     df})
    #
    # } else if (NDmethod=="MultipleImputation"){   ## need to verify Multiple Imputation code past here...
    #
    #   ds.zero <- ldply(llply(dataset.cen.NDNA, function(y) {ldply(y, function(x){
    #     if(nrow(subset(x, x$Data<=0))>0){tmp <- subset(x, x$Data<=0); tmp}})}))
    #
    #   nm.zero <- unique(ds.zero$Parameter)
    #
    #   if (Language == "Francais"){
    #     cat(length(nm.zero), " paramètre(s) contient(contiennent) des valeurs plus petites ou égales à zéro : ","\n"); for (k in 1: length(nm.zero)){cat(nm.zero[k], "\n")}
    #
    #   } else {
    #       cat(length(nm.zero), " parameter(s) contain values less than or equal to zero: ","\n"); for (k in 1: length(nm.zero)){cat(nm.zero[k], "\n")}
    #   }
    #
    #   dataset.cen.NDNA.log <- llply(dataset.cen.NDNA, function(x){
    #     llply(x,function(df) {
    #       df$Data.log <- log(df$Data)
    #       col_idx <- grep("Data.log", names(df))
    #       df <- df[, c(col_idx, (1:ncol(df))[-col_idx])]
    #       df})})
    #
    # }#end else if

    #---------------------------------------------------------------------------
    # assign dataset.cen.NDNA.log to .GlobalEnv
    #----------------------------------------------------------------------------
    sink()

    # assign("dataset.cen.NDNA.log", dataset.cen.NDNA.log, envir = .GlobalEnv)
    #return(dataset.cen.NDNA.log)
    setwd(datadir)
    return(dataset.cen.NA)

    #--------------------------------------------------
    # dataset.cen.NDNA.log:
    # FINISH WITH A LIST of DF 'dataset.cen.NA' in format of
    #   - Data = obs column with ND replaced
    #   - Data.log = log of Data column
    #   - obs = originals data (<values reported with MDL value)
    #   - cen = T-F  = indicates whether data was censored or not
    #   - Date = in dateformate
    #   - Year = numeric Year
    #   - Month = 1,2, 3...12 (as numeric)
    #   - MonthName = Jan, Feb as ordered factor
    #   - Yr.Index
    #   - StationID
    #   - Parameter
    #   - Group
    #   - Units
    #   - dates.dec
    #--------------------------------------------------



}#end function / fin de fonction

# variables created but not exported to global environment
  # all.datasets = df of parameter, Date, Hour, StationID
  # dataset.cen.df = list of df with obs, cen, Date, Flow
  # dataset.cen.NA = list of df with NA rows removed

