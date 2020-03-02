# Scenarios Function

#####################################################################################
# FUNCTION TO CREATE: 
#    1) A DATAFRAME OF SCENARIO PARAMETERS/OPTIONS
#    2) A LIST OF scen.index index for each set of parameters
#####################################################################################

Create.scenarios <- function(NSimYears, subpools, sampling, savedir, start_date) {

  #------------------------------------------------------------------------
  # ADDS A NEW ROW FOR EACH NSimYears for each row in sampling, and 
  # adds NSimYr column to sampling dataframe
  #------------------------------------------------------------------------

  ny <- rep(NSimYears, nrow(sampling))
  s <- sampling[rep(seq_len(nrow(sampling)), each=length(NSimYears)),]
  s$NSimYears <- ny 
   
  #------------------------------------------------------------------------
  # ADDS ScenID column to each 
  # This is a unique identifying of each scenario that will be created/tested
  #------------------------------------------------------------------------
  
  s$scenID <- as.factor(paste(s$freqName, "_", s$NSimYears, "yr_", s$sp.option, sep=""))
  
  #------------------------------------------------------------------------
  # APPLIES THE FUNCTION create.t.scenarios TO ADD scen.index to each list of scenarios
  # t.index = date/time in decimal of year (uses 1.00 for Jan 1 of first year of data)
  #------------------------------------------------------------------------

  scen.index<- dlply(s, .(scenID), create.t.scenarios, start_date, sub.pools=subpools)
  #returns a list of dataframes each with 
  
#   scen.index <- list()
#   for (i in 1: nrow(s)){
#     aa <- create.t.scenarios(s[i,], start_date, sub.pools=subpools )
#     scen.index[[i]] <- aa
#   }

  #------------------------------------------------------------------------
  # PRINT subpools and sampling scenario dataframes to station directory:
  #------------------------------------------------------------------------
  
  #print subpools and sampling dataframes to station directory:
  setwd(savedir) 
  #sinkfile <- paste("Scenario Parameters.txt", sep="")        
  sink( paste("Scenario Parameters.txt", sep=""),split=FALSE)
  cat("=======================================", "\n")
  cat("\n")
  cat("Simulated Record Lengths: ",NSimYears, "\n")
  cat("\n")
  cat("Subpools: ", "\n")
  cat("------------------------------------", "\n") 
  print(subpools)
  cat("\n")
  cat("Sampling Scenarios: ", "\n")
  cat("------------------------------------", "\n") 
  print(s) 
  cat("\n")
  cat("Number of data points by Month for each scenario : ", "\n")
  cat("------------------------------------", "\n")     
  cat("\n")
  cp <- count.npts(ldply(scen.index))
  print(cp)
  
  sink()               
  setwd(homedir)
  
#    scenarios.list <- list(scenarios=s, scen.index=scen.index)   
#    return(scenarios.list)
  
  assign("scenarios", s, envir = .GlobalEnv)
  assign("scen.index", scen.index, envir = .GlobalEnv)
  
}#end function


