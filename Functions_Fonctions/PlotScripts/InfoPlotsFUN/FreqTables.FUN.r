#######################################################################################
#
#  FUNCTION TO CREATE THE FOLLOWING DATA FREQUENCY TABLES, and export to excel file
#  (Data.Import_PlotStats.R must be run prior to calling this function, otherwise some
#   required packages, function and variables may not be available)
#
#   1) FREQ BY YEAR AND MONTH
# 
#   2) PROPORTION OF TOTAL SAMPLES BY YEAR AND MONTH
#
#######################################################################################

FreqTables <- function(dat, plotfundir, savedir, LANG){
  
#===============================================================================
# Required Packages and Variables
#===============================================================================

#===============================================================================
# Source required functions
#===============================================================================

#   setwd(plotfundir)
#   source("Summary.tables.functions.R")
#   source("MonthlyNtables.2xls.R")
#   setwd(homedir)

#===============================================================================
# IF dat is not a list --> convert to a list class
#===============================================================================

if (is.data.frame(dat)==TRUE){
  dat <- list(dat)
  names(dat) <- unique(dat[[1]]$Parameter)
}


#===============================================================================
# APPLY freq.MonthlyN and prc.MonthlyN FUNCTIONS TO all dataframes in dat
#===============================================================================

freq.MonthlyN <- llply(dat, freq_MonthlyN)
names(freq.MonthlyN) <- names(dat)

prc.MonthlyN <- llply(dat, prc_MonthlyN)
names(prc.MonthlyN) <- names(dat)


#Export to excel
MonthlyN.2xls(prc.ls=prc.MonthlyN, freq.ls=freq.MonthlyN,savedir, savename="FreqTables", stnID=unique(dat[[1]]$StationID), LANG=LANG)

}
