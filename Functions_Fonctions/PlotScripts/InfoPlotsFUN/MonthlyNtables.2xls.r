######################################################################################
# FUNCTION TO WRITE LIST OF MONTHLY SUMMARY TABLES to XLS
#  -written for FreqTables.FUN.R
#  -input=list of dataframes (1 df/summary table per group)
#  - writes each Parameter list to a different sheet
######################################################################################

MonthlyN.2xls <- function(freq.ls, prc.ls,  savedir, savename, stnID, LANG="English"){
 
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

#function to export from list (each df in list to new sheet with group name)
  rdcomexport2 <- function(prc, freq) {  
      if (!is.null(prc[[1]])){
        sh=wb[["Worksheets"]]$Add()
        if ((nchar(names(prc[1])))<=30){
          sh[["Name"]] <- names(prc[1])
        }else  {sh[["Name"]] <-  substring(names(prc[1]), 1, 29)}
        
        exportVector(as.vector(names(prc[1])), at=sh$Range("B2"))
        exportVector(as.vector("Variable:"),at=sh$Range("A2"))
        exportVector(as.vector(stnID), at=sh$Range("B1"))
        exportVector(as.vector("Station:"),at=sh$Range("A1"))
      
        df.freq<- as.data.frame(freq[[1]])
        exportDataFrame(df.freq,at=sh$Range("B6"))
        exportVector(rownames(df.freq),at=sh$Range("A7"))               
        exportVector(as.vector("Sample Frequency by Month"),at=sh$Range("A4"))
          
        df.prc <- as.data.frame(prc[[1]])
        exportDataFrame(df.prc, at = sh$Range("Q6"))
        exportVector(rownames(df.prc),at=sh$Range("P7"))
        exportVector(as.vector("Sample Proportions(%total samples by Month)"), at=sh$Range("P4"))
      }#END IF
  }#end function
  
  
#apply export function to list
  PLN <- length(freq.ls)
  lapply(1:PLN, function(i) rdcomexport2(prc=prc.ls[i],freq=freq.ls[i]))  
  
  
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




########################################################################## 

#example to write code for df input instead of list (split by variable)
  #rdcomexport <- function(df) {  
    #sh=wb[["Worksheets"]]$Add()
    #sh[["Name"]] <- as.character(df$Group[1])   
    #exportDataFrame(df, at = sh$Range("A1"))
  #}
  
  #d_ply(iris,.(Species), rdcomexport) #apply export function to dataframe
########################################################################## 
  
  
  
  
     
    