#-------------------------------------------------------------------------
# Code to check if variables exists
# Used in the RUNCODE_PowerSteps - Step 5 to make sure all variables are loaded
# before proceeding.
#--------------------------------------------------------------------------

var.check <- function(var1){
  if(!exists(var1)){
    cat("The '", var1, "' variable is missing. You must load or create this variable before proceeding.", sep=""); cat("\n")
    return(x=1)
  } else {return(x=0)}
}


var.all <- function(var.ls){
  xx <- 0
  for (i in 1: length(var.ls)){
    x <- var.check(var.ls[[i]])
    xx <- xx + x
  }
  if(xx == 0) {cat("There are no missing variables. Continue to the next step.")}
}


#-------------------------------------------------------------------------
# Code to check if results folders exists and may be overwritted
# Used in the RUNCODE_PowerSteps - Step 
#--------------------------------------------------------------------------


dir.check <- function(par1){
  if(!exists(par1)){
    cat("    The '", par1, "' folder already exists in the SK_PowerResults folder.", sep=""); cat("\n")
    cat("        Results for this parameter will be overwritten.","\n")
    cat("        Recommend moving existing results to another location OR renaming the folder before proceeding.", "\n")
    cat("\n")
    return(x=1)
  } else {return(x=0)}
}

dir.all <- function(par.ls, pwrdf, savedir=resultsdir){
  #check for SKResults folder
  cat("RUNNING CHECK TO CONFIRM IF ANY RESULTS WILL BE OVERWRITTEN", "\n")
  cat("-----------------------------------------------------", "\n")
  stndir1 <- paste(savedir,"/", unique(pwrdf$StationID), sep="")
  SKresultsdir1 <- paste(stndir1, "/", "SK_PowerResults", sep="")
  if(!file.exists(stndir1)){
    cat("The station folder for ", unique(pwrdf$StationID), " does not currently exist.", sep=""); cat("\n")
    cat("No results will be overwritten. OK TO PROCEED.", "\n")
  }else if (!file.exists(SKresultsdir1)){
    cat("The SK_PowerResults folder does not currently exists in the", unique(pwrdf$StationID), "station folder.", "\n")
    cat("No results will be overwritten. OK TO PROCEED.", "\n")
  }else {
    cat("An SK_PowerResults folder currently exists in the", unique(pwrdf$StationID), "station folder", "\n")
    cat("The PowerResults_stnID xls and .RDAta files will be overwritten.","\n")
    cat("Suggest renaming or moving these files before proceeding.", "\n")
    cat("\n"); cat("Checking Individual Parameter folders: ", "\n")
  }
    
      
  #check for parameter folders
  if(file.exists(SKresultsdir1)){
    xx <- 0
    for (i in 1: length(par.ls)){
      x <- dir.check(par.ls[[i]])
      xx <- xx + x
    }
    if(xx == 0) {
      cat("No parameter folders to be created currently exist in the SKPower_Results folder.", "\n")
      cat("None of the parameter specific results will be overwritten.", "\n")
    }
  }
}#end function

  