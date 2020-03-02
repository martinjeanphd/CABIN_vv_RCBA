#Requirements

#Required.Packages.FUN <- function {

#================================================================================
# Step 1: Define directories pathways
# Note: all these folders MUST already exist under the homedir folder
#
# Étape 1: définir les chemins d'accès des différents dossiers
# Note: tous ces dossiers DOIVENT déjà exister dans le dossier homedir
#================================================================================

#homedir <- DEFINED IN User.Defined.Parameters.R
#datadir <- User.Defined.Parameters.R
#resultsdir <- User.Defined.Parameters.R
#maindatadir <- User.Defined.Parameters.R

#Main Code Folder directories
maincodedir <<- "./"

#Data Folder directories
maindatadir <<- "./Data_Donnees"


#Supporting Function Folder directories
functiondir <<- "Functions_Fonctions"

plotfundir <<- paste(functiondir, "/PlotScripts", sep="")

cabinFUNdir <<- paste(functiondir, "/CABINScripts", sep="")


#================================================================================
# 2) Load necessary R packages
#
#  Packages must have been installed on your current version of R previously
#  # if package unavailable, install it via the R command: install.packages("packageNAME")
#================================================================================

#install.packages("survival")  #REQUIRED FOR NADA, will not load automatically when NADA is loaded from .gz
if (!require(survival)) install.packages("survival", repos = "http://cran.rstudio.com/")

if (!require(NADA)) install.packages("NADA", repos = "http://cran.rstudio.com/") #NOTE: The following object(s) are masked from ‘package:stats’: cor

#scales goes with ggplot and adds the needed scale* functions
if (!require(scales)) install.packages("scales", repos = "http://cran.rstudio.com/")

if(!require(ade4)) install.packages("ade4", repos = "http://cran.rstudio.com/")
if(!require(DT)) install.packages("DT", repos = "http://cran.rstudio.com/")
if(!require(Kendall)) install.packages("Kendall", repos = "http://cran.rstudio.com/")
if(!require(pracma)) install.packages("pracma", repos = "http://cran.rstudio.com/")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.rstudio.com/")
if(!require(ape)) install.packages("ape", repos = "http://cran.rstudio.com/")
if(!require(car)) install.packages("car", repos = "http://cran.rstudio.com/")
if(!require(corrgram)) install.packages("corrgram", repos = "http://cran.rstudio.com/")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.rstudio.com/")
if(!require(Deducer)) install.packages("Deducer", repos = "http://cran.rstudio.com/")
if(!require(DescTools)) install.packages("DescTools", repos = "http://cran.rstudio.com/")
if(!require(Devices)) install.packages("Devices", repos = "http://cran.rstudio.com/")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.rstudio.com/")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.rstudio.com/")
if(!require(formattable)) install.packages("formattable", repos = "http://cran.rstudio.com/")
if(!require(geosphere)) install.packages("geosphere", repos = "http://cran.rstudio.com/")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.rstudio.com/")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.rstudio.com/")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.rstudio.com/")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.rstudio.com/")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.rstudio.com/")
if(!require(labdsv)) install.packages("labdsv", repos = "http://cran.rstudio.com/")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.rstudio.com/")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.rstudio.com/")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.rstudio.com/")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.rstudio.com/")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.rstudio.com/")
if(!require(sp)) install.packages("sp", repos = "http://cran.rstudio.com/")
if(!require(spdep)) install.packages("spdep", repos = "http://cran.rstudio.com/")
if(!require(TeachingDemos)) install.packages("TeachingDemos", repos = "http://cran.rstudio.com/")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.rstudio.com/")
if(!require(vegan)) install.packages("vegan", repos = "http://cran.rstudio.com/")


### Note: Use sessionInfo()  to check what packages are installed


#===============================================================================
# STEP 3: SOURCE SUPPORTING CODE
#===============================================================================

#Supporting code needed for importing and formatting data:
source("../../../Functions_Fonctions/Data.Import.Code.R", encoding="UTF-8")

source("../../../Functions_Fonctions/FormattingScripts/read.data_wDates.R")
source("../../../Functions_Fonctions/FormattingScripts/add.rm.columns.R")  #contains: rm.NAcol, rm.NArows, add.Var.GR, add.flow, add.ParStn, add.season, YearMonth, add.allcolumns
source("../../../Functions_Fonctions/FormattingScripts/SplitDL.R")
source("../../../Functions_Fonctions/FormattingScripts/ReplaceND.R")
source("../../../Functions_Fonctions/FormattingScripts/Cons.Freq.FUN.R")
source("../../../Functions_Fonctions/FormattingScripts/excelUtils3.R")
source("../../../Functions_Fonctions/FormattingScripts/Decimal_date.FUN.R")
source("../../../Functions_Fonctions/FormattingScripts/Consistent.Frequency.R")
source("../../../Functions_Fonctions/FormattingScripts/LOG.DataImport.FUN.R")
source("../../../Functions_Fonctions/FormattingScripts/Export.2xls.R")
source("../../../Functions_Fonctions/FormattingScripts/check.var.FUN.R")
source("../../../Functions_Fonctions/FormattingScripts/check.spcnt.FUN.R")


# #supporting code needed for graphs and tables:
source("../../../Functions_Fonctions/PlotScripts/InfoPlotsFUN/BoxplotsOutliners.r")


source("../../../Functions_Fonctions/ModuleFunctions/Module1.FUN.R")
#source("Module2.FUN.R")

source("../../../Functions_Fonctions/CABINScripts/Verification.data.FUN.R", encoding = "UTF-8")

#} #end Function

