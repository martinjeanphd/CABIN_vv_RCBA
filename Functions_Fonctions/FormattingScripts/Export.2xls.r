#====================================================================================
#  CODE TO EXPORT A LIST OF DATAFRAMES TO EXCEL
#  W/ each list item exported to seperate tab (tab names by list name)
#
# currently used for: summary.tables; 
#====================================================================================

list.2xls <- function(list.df, savename, savedir, stnID, LANG="English"){
  
  #install.packages("RDCOMClient", repos = "http://www.omegahat.org/R")
  #require("RDCOMClient")
  #source("http://www.omegahat.org/RDCOMClient/examples/excelUtils3.R")    #functions for RDCOMClient
  
  #To create a new instance of a registered COM server class, we use the function 
  #the resulting R objects (ex, word and ie) are references to a generic DCOM object
    xls <- COMCreate("Excel.Application")  
  
  #we can now move on and start accessing its data (properties) and calling its methods (functions).
  #xls[["Visible"]] <- TRUE  #opens excel 
  xls[["Visible"]] <- FALSE  #runs without opening?
  wb = xls[["Workbooks"]]$Add(1)  #opens a new workbook
  
  #open existing workbook
  #wb = xls$Workbooks()$Open("C:/Users/macdonaldj/Desktop/Rcode_CURRENT/OTHER CODES/STATS AND GRAPHS CODE/Results/TestResults/Testxl.xlsx")
  
  #function to export from df
  rdcomexport1 <- function(df) {  
    sh=wb[["Worksheets"]]$Add()
    sh[["Name"]] <-  as.character(unique(df$Tab))
    exportVector(as.vector(stnID), at=sh$Range("B1"))
    exportVector(as.vector("Station:"), at=sh$Range("A1"))
    exportDataFrame(df, at = sh$Range("A3"))
    
    }#end function
  
  #apply export function list
  suppressWarnings(l_ply(list.df, rdcomexport1))

  
  
  #CLEAN UP AND SAVE WORKBOOK:
  if(LANG=="French") {
    xls$Sheets("Feuil1")$Delete()
  }else if (LANG=="English"){
    xls$Sheets("Sheet1")$Delete()
  }
    
  setwd(savedir)
  filename <- paste(getwd(),"/",savename, "_", stnID,".xlsx",sep="")
  filename <- gsub("\\/","\\\\",filename)
  wb$SaveAs(filename)
  wb$Close(filename)
  
  print("Export to excel complete")
  
}#end function



#***************************************************************************************
#example to write code for df input instead of list (split by variable)
#d_ply(iris,.(Species), rdcomexport) #apply export function to dataframe (split by Species)
#***************************************************************************************
