#ReplaceND.R                                                  #ReplaceND.R                                             


############################################################################
# STEP 1: REPLACE ND WITH A VALUE
#   OPTIONS: 
#   1) Replace with a value (0.5 * highest DL, zero ect)
#   2) Multiple imputation with random value between 0 and DL
#############################################################################

#===========================================================================
# FUNCTION 1: REPLACE WITH A VALUE
#
# creates a single df with ND values replaced with specified value
#
#  For REPLACE ND with 0.5 * DL -->  value = 0.5  (i.e. 0.5*DL)
#  For REPLACE ND with ZERO --> value = 0       (i.e. 0*DL)
#===========================================================================

replaceND <-function(df, replace.value){
  
  tmp <- df
  tmp$Data <- tmp$obs
  tmp <- within(tmp, Data[cen==TRUE] <- (Data[cen==TRUE])*replace.value)
  
  col_idx <- grep("Data", names(tmp))
  tmp <- tmp[, c(col_idx, (1:ncol(tmp))[-col_idx])]
  
  
  return (tmp)
}#end function

#===========================================================================
# FUNCTION 2: REPEAT replaceND for multiple columns 
#     - adds a column for replace method (Data_MDLVALUE)
#     - used for stats comparison, not for power code.
#===========================================================================

replaceND.mult <-function(dataset, replace.value = c(0, 0.5)){
  
  for (i in 1:length(replace.value)){ 
    tmp <- dataset
    tmp$datatmp <- tmp$obs
    tmp <- within(tmp, datatmp[cen==TRUE] <- (datatmp[cen==TRUE])*replace.value[i])
    replace.col<- paste("Data_MDL", replace.value[i], sep="") 
    dataset <- tmp
    dataset <- dataset[,c(ncol(dataset),1:(ncol(dataset)-1))]
    names(dataset)[1] <- replace.col
  }
  return (dataset)
}#end function


#rename the Data_MDLX to "Data"
#dataset.cen.ND <- llply(dataset.cen.ND, function(df) {names(df) <- gsub(grep("^Data_MDL", names(df), value=TRUE), "Data", names(df)); df})   


#===========================================================================
# FUNCTION 3: MULTIPLE IMPUTATION --> REPLACE ND WITH STRAIGHT RANDOM SAMPLE BETWEEN 0 and MDL
# creates a list of NSims dataframes, each df will have slighly different values for ND
#
# NSims=number of imputations
#===========================================================================

  #----------------------------------------------------------------
  #FUNCTION to count number of decimal places
  #
  # needed because the interval used for replacement values will
  # on the precision of the dataset
  # e.g. if data =0.05 --> 0.0
  #----------------------------------------------------------------
  
  decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }


  #----------------------------------------------------------------
  # FUNCTION to replace ND with values bewteen 0 and MDL for 1 dataset
  #----------------------------------------------------------------

  ND.SRS <- function(ds){
    
    for (i in 1:nrow(ds)) 
    {
      if (ds$cen[i] == "TRUE")
      {
        #assumes that the MDL is placed in the obs column when value is <MDL
        MDL <-  ds$obs[i]             
        
        #increments for 'draws' defined as the number of decimal places in MDL
        byvalue <- decimalplaces(MDL) 
        
        draws <- seq(0, MDL, by=(10^(-byvalue))) 
        ds$Data[i] <- sample(draws, 1, replace=TRUE)
        
      }  
    }
    return(ds)
  }#end function

  #----------------------------------------------------------------
  # FUNCTION to repeat random sample function on the same initial dataset to create multiple datasets
  # - calls ND.SRS function
  #----------------------------------------------------------------

  Impute.ND <- function(ds, NSims){
    
    #add an orig.obs column with same data as obs
    tmp1 <- ds
    tmp1$Data <- tmp1$obs     
    col_idx <- grep("Data", names(tmp1))
    tmp1 <- tmp1[, c(col_idx, (1:ncol(tmp1))[-col_idx])]
    
    #creates a list with NSims df of tmp1 replicated    
    Simdata <- rep(list(tmp1),NSims)         
    
    #apply the ND.SRS function to each replicated dataset in the list
    Simdata.ND <- llply(Simdata,ND.SRS)  
    #str(Simdata.ND)
    
    #returns a list of datasets each with ND replaced using random sampling   
    rm(tmp1)
    return(Simdata.ND)
    
  }#end function


