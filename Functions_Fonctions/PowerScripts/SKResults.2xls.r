#SKResults.2xls                                                       #SKResults.2xls

######################################################################################
# FUNCTIONS TO WRITE POWER RESULTS TO XLS
######################################################################################

######################################################################################
# FUNCTION TO WRITE RESULTS DF to XLS ()
#  -input=dataframe all parameters
#  - TAB 1: Summary of all results combined
#  - writes each Parameter to a seperate tab
######################################################################################

resultsSK.2xls <- function(savedir, results.df, savename, LANG){
  
  #install.packages("RDCOMClient", repos = "http://www.omegahat.org/R")
  #require("RDCOMClient")
  #source("http://www.omegahat.org/RDCOMClient/examples/excelUtils3.R")    #functions for RDCOMClient
  
  #To create a new instance of a registered COM server class, we use the function 
  #the resulting R objects (ex, word and ie) are references to a generic DCOM object
  xls <- COMCreate("Excel.Application")  
  
  #we can now move on and start accessing its data (properties) and calling its methods (functions).
  #xls[["Visible"]] <- TRUE  #opens excel 
  xls[["Visible"]] <- FALSE  #runs without opening excel
  wb = xls[["Workbooks"]]$Add(1)  #opens a new workbook
  

  ########################################################################## 
  
  #function to write export data to xls with Sheet name 
  rdcomexport <- function(df) {  
    sh=wb[["Worksheets"]]$Add()
    sh[["Name"]] <- as.character(unique(df$Parameter))   #Names the Sheet for Parameter value in row1
    exportDataFrame(df, at = sh$Range("A1"))         
  }
  ########################################################################## 
  
  stnID <- as.character(unique(results.df$StationID))
  
  #Splits the results.df by Parameter column, and 
  # applies rdcomexport function to each df from split
  d_ply(results.df,.(Parameter), rdcomexport) 
  
  
  #ADD another worksheet with all parameters
  sh=wb[["Worksheets"]]$Add()
  sh[["Name"]] <- "AllParameters"
  exportDataFrame(results.df, at = sh$Range("A1"))
  
  
  #CLEAN UP AND SAVE WORKBOOK:
  if (LANG=="French"){
    xls$Sheets("Feuil1")$Delete()
  }else if (LANG=="English") {
    xls$Sheets("Sheet1")$Delete()
  }
  
  #savedir <- paste(savedir,"\\",stnID, "\\","SK_PowerResults", sep="")
  setwd(savedir) 
  filename <- paste(getwd(),"/",savename ,"_", stnID, ".xls",sep="")
  filename <- gsub("\\/","\\\\",filename)
  wb$SaveAs(filename)
  wb$Close(filename)
  
  cat("Export to excel file complete.","\n")
  cat("\n")
  
}

