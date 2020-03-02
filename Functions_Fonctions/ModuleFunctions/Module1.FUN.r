#MODULE1_FUN.R

#################################################################################
# MODULE 1: STEPS FOR IMPORTING AND FORMATTING A DATASET THAT CAN BE USED 
#           IN THE EXPLORATORY DATA ANALYSIS
#
# MODULE 1: ÉTAPES POUR L'IMPORTATION ET LE FORMATAGE D'UN JEU DE DONNÉES
#           QUI POURRA ÊTRE UTIILISÉ POUR L'ANALYSE EXPLORATOIRE DE DONNÉES
#################################################################################

Module1.FUN <- function(homedir, datadir,  filename, na.vals=999999, dateformat, 
                         param.filename="VariableList.csv", SEP=",", DEC=".", NDmethod="ReplaceValue", 
                         replace.value=0.5, NSims=1000, savename)  {  


#===============================================================================
#  STEP 1: ENTER USER DEFINED PARAMETERS FOR:
#          - Importing a .csv data file
#          - Dealing with Non-detects (replace value, or Multiple Imputation)
#          - Reducing the dataset to a consistent frequency
#
#  ÉTAPE 1: SAISIR LES PARAMÈTRES DÉFINIS PAR L'UTILISATEUR POUR:
#          - Importer un fichier .csv
#          - Gérer les valeurs non détectées (remplacer les valeurs ou imputation multiple)
#          - Réduire le jeu de données à une fréquence déterminée
#===============================================================================

#===============================================================================
# IMPORT THE CSV FILE DEFINING VARIABLE UNITS, GROUP AND NAMES
#
# IMPORTER LE FICHIER CSV DÉFINISSANT LES NOMS, UNITÉS ET GROUPE DE VARIABLES
#===============================================================================
if (Platform == "Windows") setwd(paste(homedir, "\\Data", sep="")) else
setwd(paste(homedir, "/Data", sep=""))
param.master <- read.csv(param.filename, as.is=TRUE, header=TRUE, sep=SEP, dec=DEC)  
setwd(homedir)


#===============================================================================
#  IMPORT AND FORMAT THE DATASET using the Data.Import Function
#
# IMPORTER ET FORMATER LE JEU DE DONNÉES en utilisant la fonction Data.Import
#===============================================================================

# Notes: 
#  1) All function arguments have previously been defined 
#  2) because unnamed variables are used in the function call, the variables must be entered in 
#      the same order as is found in the function definition.
#
# Notes:
#  1) Tous les paramètres de la fonction ont péalablement été définis
#  2) Puisque les variables sans nom sont utilisées dans l'appel de la fonction, les variables
#     être saisies dans le même ordre que celui utilisé dans la définition de la fonction

dataset.NDNA <<- Data.Import(homedir, datadir, resultsdir, dateformat, filename,  season.input, param.master, 
                            na.vals, replace.value, NDmethod, NSims, SEP=SEP, DEC=DEC)



#===============================================================================
# STEP 2 (optional): Save a LOG file with the user defined parameters used to 
#                    import and format the dataset
#
# ÉTAPE 2 (facultative): Enregistrer un fichier LOG avec les paramètres spécifiés
#                        par l'utilisateur et utilisés pour l'importation et le
#                        formatage du jeu de données
#===============================================================================

#Does the same as Log.DataImport(savedir=paste(datadir,"\\CreatedDataFiles", sep="")) 
#this will save the log file to the 'CreatedDataFiles' subfolder in the datadir
#
#Réalise la même chose que Log.DataImport(savedir=paste(datadir,"\\CreatedDataFiles", sep=""))
#Le fihcier LOG sera enregistré dans dossier 'CreatedDataFiles' situé dans datadir

if (Platform == "Windows") setwd(paste(datadir,"\\CreatedDataFiles", sep="")) else
setwd(paste(datadir,"/CreatedDataFiles", sep=""))

sink("LOG.DataInputVariables.txt", append=FALSE, split=TRUE)

if (Language == "Francais"){
   cat("Paramètres de l'utilisateur pour le formatage du fichier .csv de données",  "\n")
} else {
  cat("User Defined Parameters for Formatting a .csv dataset",  "\n")
}
cat("================================",  "\n")

cat("\n")
if (Language == "Francais"){
  cat("Jeu de données importé (nom du fichier): ",filename,  "\n")
  cat("Valeurs manquantes recodées en NA: ", na.vals, "\n")
} else {
  cat("Imported dataset (filename): ",filename,  "\n")
  cat("Missing values recoded to NA: ", na.vals, "\n")
}
cat("\n")

if (Language == "Francais"){
  cat("Méthodes sur données censurées:", "\n")
} else {
  cat("Censored Data methods:", "\n")
}
cat("----------------------------", "\n")

cat("NDmethod: ",NDmethod,  "\n")
cat("replace.value: ",replace.value,  "\n")
cat("\n")

cat("\n")

sink()


#===============================================================================
# STEP 3 (optional): Save the created datasets to .RData file for later use
#
# dataset.NDNA = a list of formatted dataframes (1 per parameter) 
#
# ÉTAPE 3 (facultative): Enregistrer les jeux de données dans un fichier .RData
#
# dataset.NDNA = une liste des dataframes formatés (1 par paramètre)
#===============================================================================

if (Platform == "Windows") setwd(paste(datadir,"\\CreatedDataFiles", sep="")) else
setwd(paste(datadir,"/CreatedDataFiles", sep=""))

save(dataset.NDNA, file=paste(savename, ".RData", sep=""))

setwd(homedir)

}#end function