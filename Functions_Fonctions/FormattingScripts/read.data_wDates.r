#read.data_wDates.R

################################################################################
#
# READ.DATA_wDates FUNCTION: Reads in data (as .csv) and formats the date
#   - read in csv file (filename)
#   - formats Date as dateformat
#   - replaces missing value with NAs
#
# RETURNS a dataframe with 1 parameter per column, plus column for Date, Hour, Station
# formats the date as 'dateformat'
#
#-------------------------------------------------------------------------------
#
# FONCTION READ.DATA_wDates: Lit les données (en .csv) et formatte la date
#   - lecture du fichier csv (filename)
#   - formatter la date selon la variable dateformat
#   - remplacer les valeurs manquantes par NA
#
# RETOURNE un dataframe avec 1 paramètre par colonne, plus colonnes pour Date, station
# formattage de la date selon 'dateformat'
################################################################################

read.data_wDates <- function(datadir, filename, dateformat, na.vals, SEP, DEC){

  setwd(datadir)

  dataset <- read.csv(filename, as.is=TRUE, header=TRUE, na.string=na.vals, sep=SEP, dec=DEC)  # read dataset into R

  stationID <-  unique(dataset$ID)[1]                     # extract station ID

  L <- length(dataset$ID[dataset$ID==ID])   # determine how many valid records are
                                                                 # available in the current dataset
                                                                 # (i.e., eliminate any artificial
                                                                 # records introduced when importing the data
                                                                 # into R)

  dataset <- dataset[1:L, ]  # retain all valid records in the dataset


  dataset$SampleDate <- as_Date(dataset$SampleDate, format=dateformat)  # format the dates in the dataset


  return(dataset)

}#end function


