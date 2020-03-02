# --------------------------------------------------------------------------
# FUNCTION TO SPLIT DF by alpha level and export to seperate sheets in EXCEL 
#  -written for out.list list from MDT.extract.R SCRIPT
#  -input=list of dataframes (1 df/alpha level)
#  -writes each GROUP(i.e. alpha level) table in list to a different sheet
# --------------------------------------------------------------------------

#Input if run outside of function:
#savedir <- resultsdir
#results.list <-  out.list  
#stnID <- stationID
#savename <- "MDTPowerResults"

outLS_alpha.2xls <- function(savedir, results.list, savename){
  
  #install.packages("RDCOMClient", repos = "http://www.omegahat.org/R")
  require("RDCOMClient")
  #source("http://www.omegahat.org/RDCOMClient/examples/excelUtils3.R")    #functions for RDCOMClient
  
  #To create a new instance of a registered COM server class, we use the function 
  #the resulting R objects (ex, word and ie) are references to a generic DCOM object
  xls <- COMCreate("Excel.Application")  
  
  #we can now move on and start accessing its data (properties) and calling its methods (functions).
  xls[["Visible"]] <- TRUE  #opens excel 
  #xls[["Visible"]] <- FALSE  #runs without opening?
  wb = xls[["Workbooks"]]$Add(1)  #opens a new workbook
  
  
  
  #######
  #function to export from list (each df in list to new sheet with group name)
  ###NEED TO CHANGE THIS DEPENDING ON HOW YOU WANT TO OUTPUT
  rdcomexport <- function(df) {  
    sh=wb[["Worksheets"]]$Add()
    sh[["Name"]] <- paste("MDT",as.character(df$alpha[1]))   
    exportDataFrame(df, at = sh$Range("A1"))
  }
  ######
  
  #apply export function to list
  l_ply(results.list, rdcomexport)
  
  #CLEAN UP AND SAVE WORKBOOK:
  xls$Sheets("Sheet1")$Delete()
  
  setwd(savedir)
  
  filename <- paste(getwd(),"/",savename ,".xlsx",sep="")
  filename <- gsub("\\/","\\\\",filename)
  wb$SaveAs(filename)
  wb$Close(filename)
  
  cat("Export to excel file complete.")
  
}
