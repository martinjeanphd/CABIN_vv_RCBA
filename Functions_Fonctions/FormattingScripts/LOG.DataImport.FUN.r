#Create.LogFiles.FUN

##########################################################################
# Functions to create log files for storing info on steps
#
# Fonction pour créer un fichier log enregistrant les étapes
#########################################################################

#===============================================================================
# Function to PRINT Information on User Defined Variables Used to input and format
# a .csv dataset
#
# Fonction permettant d'IMPRIMER l'information touchant les
#===============================================================================

Log.DataImport <- function(savedir, filename, na.vals, NDmethod, replace.value=NULL, NSims=NULL)
{

  setwd(savedir)

  sink("LOG.DataInputVariables.txt", append=FALSE, split=TRUE)

  if (Language == "Francais"){
   cat(catDiacritical("Paramètres définis par l'utilisateur pour le formatage du fichier .csv"))
} else {
  cat("User Defined Parameters for Formatting a .csv dataset",  "\n")
}
  cat("================================",  "\n")

  cat("\n")
if (Language == "Francais"){
     cat(catDiacritical(paste("Fichier importé : ",filename,  sep="")))
     cat(catDiacritical(paste("Valeurs manquantes recodées en NA : ", na.vals, sep="")))
} else {
    cat("Imported dataset (filename): ",filename,  "\n")
    cat("Missing values recoded to NA: ", na.vals, "\n")
}
  cat("\n")

if (Language == "Francais"){
     cat(catDiacritical("Méthodes sur les valeurs censurées :"))
} else {
     cat("Censored Data methods:", "\n")

}

cat("----------------------------", "\n")

if (Language == "Francais") {
    cat(catDiacritical(paste("Méthode : ",NDmethod,  "\n")))
    if(NDmethod == "ReplaceValue"){
        cat(catDiacritical(paste("remplacé par : ",replace.value,  "\n", sep="")))
    }
    if(NDmethod == "MultipleImputation"){
        cat(catDiacritical(paste("simulée : ",NSims,  "\n", sep="")))
    }
} else {
    cat("NDmethod: ",NDmethod,  "\n")
    if(NDmethod == "ReplaceValue"){cat("replace.value: ",replace.value,  "\n")}
    if(NDmethod == "MultipleImputation"){cat("NSims: ",NSims,  "\n")}
}
  cat("\n")

  cat("----------------------------", "\n")



  sink()

}