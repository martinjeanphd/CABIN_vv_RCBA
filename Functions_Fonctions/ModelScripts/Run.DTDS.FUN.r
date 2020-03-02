#########################################
#  FUNCTION TO RUN DT AND DS MODELS
#  outputs: 
#   - DTDS.data dataframe
#   - model.DS, model.DT, model.DTDS
#   - DS.formula
#########################################

Run.DTDS <- function(data.df, dataCOL, span.value=NULL, flowYN = c("YES", "NO"), 
                     DT.option = c("Lowess", "SK"), DS.option = c("Fourier","Subtract.Medians", "None"), 
                     num.harmonics=NULL){
  
  #DT.loess, DS.fourier functions need to be sourced
 
  #------------------------------------------------------------------------------
  # ADD DETRENDED MODEL INFORMATION
  # creates:
  #   - df with 3 new columns: DT.resid, DT.fit, DT.method
  #   - model.DT = variable containing the model information for DT
  #------------------------------------------------------------------------------
  
  if (DT.option == "Lowess") {
    tmp <- DT.loess(df=data.df, datcol=dataCOL, l.span=span.value,l.degree=2, l.family="symmetric", flowYN)
    
    model.DT <- tmp[[2]]       #lowess model output (includes fitted, residuals ect...)
    
    DT.data <- tmp[[1]] 
    DT.data <- DT.data[,!(names(DT.data) %in% c("datacol"))]  #removes the data.col column
    names(DT.data) <- gsub(grep("DT.resid", names(DT.data), value=TRUE), "DT", names(DT.data))
    
    rm(tmp)
    
  }else if (DT.option =="None") {
    DT.data <- data.df
    DT.data$datacol <-   DT.data[,grep(paste("^",dataCOL,"$", sep=""), names(DT.data), value=TRUE)]   
    DT.data$DTmethod <- "None"
    DT.data$DT.fit <- NA
    DT.data$DT <- DT.data$datacol 
    model.DT <- NULL
    
  }else if (DT.option=="SK"){
    
    tmp <- DT.SK(df=data.df, datcol=dataCOL)
    
    model.DT <- tmp[[2]]       #lowess model output (includes fitted, residuals ect...)
    
    DT.data <- tmp[[1]] 
    DT.data <- DT.data[,!(names(DT.data) %in% c("datacol"))]  #removes the data.col column
    names(DT.data) <- gsub(grep("DT.resid", names(DT.data), value=TRUE), "DT", names(DT.data))
  
    rm(tmp)
  }
  

  
  #------------------------------------------------------------------------------
  #how can I subtract from DT data??
  
  # ADD A CONSTANT BASED ON A BASE YEAR TO THE DT RESIDUALS (to make them +ve)
  #add baseline year median value OR add overall median year value back on DT resids??
  #------------------------------------------------------------------------------
  
  #sm of first year of data
  #tmp2 <- subset(data.df, Yr.index==1)
  #smDT <- median(as.numeric(by(tmp2$datacol, tmp2$Season, median, na.rm=T)))
  #DT.data$DTplusMED <- DT.data$DT + smDT
  
  #------------------------------------------------------------------------------
  # ADD DETSEASONALIZED  MODEL INFORMATION
  # creates:
  #   - df with 6 new columns: DS.resid, DS.fit, DS.method,  DTDS.resid, DTDS.fit, DTDS.method
  #   - model.DS = variable containing the model information for DS on orig data
  #   - model.DTDS = variable containing the model information for DT data
  #------------------------------------------------------------------------------

  if (DS.option == "Fourier") {
    #create DS
    tmp <- DS.fourier(df=DT.data, datcol=dataCOL, k=num.harmonics)
    model.DS <- tmp[[2]]
    DS.data <- tmp[[1]] 
    DS.formula <- tmp[[3]]
    
    DS.data <- DS.data[,!(names(DS.data) %in% c("datacol"))]   
    names(DS.data) <- gsub(grep("DS.resid", names(DS.data), value=TRUE), "DS", names(DS.data))
    names(DS.data)[names(DS.data) %in% c("DS.fit")] <- "DS1.fit"
    
    #create DTDS
    tmp1 <- DS.fourier(df=DS.data,datcol="DT", k=num.harmonics)
    model.DTDS <- tmp1[[2]]
    DTDS.data <- tmp1[[1]] 
    DTDS.formula <- tmp1[[3]]
    
  }else if (DS.option=="Subtract.Medians"){
    #create DS
    #trying using DTplusMED data
    tmp <- DS.Medians(df=DT.data, datcol=dataCOL)
    model.DS <-  tmp[[2]]    #model stores a df with SM and Season
    DS.data <- tmp[[1]]
    
    DS.data <- DS.data[,!(names(DS.data) %in% c("datacol"))]   
    names(DS.data) <- gsub(grep("DS.resid", names(DS.data), value=TRUE), "DS", names(DS.data))
    names(DS.data)[names(DS.data) %in% c("DS.fit")] <- "DS1.fit"
    
    #create DTDS
    tmp1 <- DS.Medians(df=DS.data, datcol="DT")
    model.DTDS <-  tmp1[[2]]    #model stores a df with SM and Season
    DTDS.data <- tmp1[[1]]
     
    DTDS.formula <- NULL
    DS.formula <- NULL
    
  }else if (DS.option=="None"){
    
    DS.data <- DT.data
    DS.data$DS <- data.df[,grep(paste("^",dataCOL,"$", sep=""), names(data.df), value=TRUE)]  
    DS.data$DS1.fit <- NA
    DS.data$DSmethod <- "None"
    DS.data$harmonics <- NA
    
    DTDS.data <- DS.data
    DTDS.data$DTDS.resid <- DS.data$DT
    DTDS.data$DTDS.fit <- NA
    
    model.DS <- NULL
    model.DTDS <- NULL
    DS.formula <- NULL
    DTDS.formula <- NULL
    
    
  }#else if....
  
  DTDS.data <- DTDS.data[,!(names(DTDS.data) %in% c("datacol"))]   
  names(DTDS.data) <- gsub(grep("DS.resid", names(DTDS.data), value=TRUE), "DTDS", names(DTDS.data))
  names(DTDS.data)[names(DTDS.data) %in% c("DS.fit")] <- "DTDS.fit"
  
  names(DTDS.data)[names(DTDS.data) %in% c("DS1.fit")] <- "DS.fit"


  out <- list(DTDS.data=DTDS.data, model.DT=model.DT, model.DS=model.DS, model.DTDS=model.DTDS , DS.formula=DS.formula, DTDS.formula= DTDS.formula) 
       
  return(out)
}#end fucntion