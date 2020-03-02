#-------------------------------------------------------------
# Function to add season medians as column
#-------------------------------------------------------------
add.SMcol <- function(dat,data.col){
  
  tmp <-dat
  
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  tmp$datacol <- tmp[,grep(paste("^",data.col,"$", sep=""), names(tmp), value=TRUE)]   
  
  #calculate seasonal medians
  SM.calc<- by(tmp$datacol, tmp$Season, median, na.rm=T)
  seas.medians <- data.frame(cbind(SM = as.numeric(SM.calc), Season = dimnames(SM.calc)[[1]]), stringsAsFactors=FALSE)
  seas.medians$SM.calc <- as.numeric(seas.medians$SM.calc)
  
  tmp$SM.calc <- seas.medians$SM.calc[match(tmp$Season, seas.medians$Season)] 
  
  #tmp$SMresid= tmp[,"datacol"] - tmp[,"SM.calc"]
  #tmp$DSmethod = "SeasMed"
  tmp <- tmp[,!names(tmp)=="datacol"]
  
  return(tmp)
}



#-------------------------------------------------------------
# 2) Add span, datcol and SM columns to DTDS.data
#-------------------------------------------------------------
add.modelparam <- function(mdf, model.DTDS, data.col){
#mdf <- DTDS.data

  #Adds sm of DT data, and the residuals after substracing sm from DT
  mdf <- add.SMcol(mdf,data.col="DT")   #which column should I use?

  #add span value for lowess
  if(unique(mdf$DTmethod) == "Lowess"){
  tmp$loess.span <- DTDSmodel.Input[[2]]$pars$span
  }
}
