#######################################################################################
#
#  FUNCTION TO CREATE THE FOLLOWING DATA SUMMARY TABLES, and export to excel file
#  (Data.Import_PlotStats.R must be run prior to calling this function, otherwise some
#   required packages, function and variables may not be available)
#
#   1) RECORD INFO SUMMARY:
#       - total number of samples (N)
#       - record start and end dates
#       - number and percent censored data
#         (% calculated as #values <MDL/total number of samples)
#       - min & max values 
# 
#   2) CENSORED DATA SUMMARY: 
#       - number of censoring limits in dataset; 
#         (note: will not include a DL if no data is <MDL for that DL)
#       - first and last date for each limit (based on <MDL values in dataset only; 
#         therefore may not represent the exact dates on which the MDL changed), 
#       - percent of dataset below each censoring limit 
#         (calculated as #values <MDL limit/total number of samples)
#
#   3) DATASET WITH ALL PARAMETERS COMBINED 
#      - Useful for pivot tables in excel
#
#######################################################################################

InfoTables <- function(dat, plotfundir, savedir, LANG="English"){
#===============================================================================
# Required Packages and Variables
#===============================================================================

#===============================================================================
# Source required functions
#===============================================================================

  #setwd(plotfundir)
  #source("Summary.tables.functions.R")
  #setwd(homedir)

  #source("Export.2xls.R")

#===============================================================================
# IF dat is not a list --> convert to a list class
#===============================================================================

if (is.data.frame(dat)==TRUE){
  dat <- list(dat)
  names(dat) <- unique(dat[[1]]$Parameter)
}

#===============================================================================
# APPLY RANGE.TABLE FUNCTION TO all dataframes in dat
#===============================================================================

  Range.T <- ldply(dat, Range.table)

#===============================================================================
# APPLY CenSummary.table FUNCTION TO all dataframes in dat
#===============================================================================
  
  CenSummary.table <- ldply(dat, CenSummary.table)


#===============================================================================
# COMBINE ALL TABLES INTO A LIST AND EXPORT TO EXCEL
# each tables will be exported to a seperate tab in the excel spreadsheet
#===============================================================================

#Combine all datasets in dat list into a dataframe; add Tab to dat columns
dat2 <- ldply(dat)

dat2$Tab="DATASET"

#combine into list
ll <- list(RangeT=Range.T, 
           CenSum = CenSummary.table,
           dataset = dat2                     #included to allow pivot tables in excel
      )


#Export to excel
list.2xls(ll, savename="SummaryTables", savedir, stnID=unique(dat2$StationID), LANG=LANG)

}

