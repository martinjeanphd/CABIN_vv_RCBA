
#Create.bootres.FUN.R"        

#################################################################################
#  "Create.bootres" FUNCTION
#
#FUNCTION to create a set of bootres residuals for 1 scenarios
#################################################################################

Create.bootres <- function(SimParams, scenIND, residVALS, I ){
#SimParam = 1 row from SimParameters (1 set of simulation parameter)

  scen.name <- as.character(SimParams$scenID)
  
    #scenNMdir <- paste(paramdir, "/", scen.name, sep="")  #add folder by scen.name to paramdir
    #dir.create(scenNMdir)
  
  Sim.index <-  subset(scenIND, scenID==scen.name)
    
  sp.option <-   SimParams$sp.option   #eg. "sp1"
  
  # Extract a vector of subpool indentifying in the Scen.index
   #Sim.sp <- scenIND$Sim.sp
  
  # Extract residual.pools for the sp.option of the scenario
  R.pools <-  subset(residVALS, spOPT==sp.option)

  #-------------------------------------------------------------------------------
  # Calls the bootres.subpools FUNCTION to create I series of bootstrapped innovations
  # for the scenario
  # bootres = matrix contains I columns (all named ss) each containing 1 set of bootstrap 
  #           residuals based on scen.index Sim.sp column
  #-------------------------------------------------------------------------------
  
    cat("RUNNING BOOTRES.SUBPOOLS FOR: ", scen.name, "\n")

    bootres <- bootres.subpools(sim.sp= Sim.index$Sim.sp, R.pools=R.pools, I)
    #R.pools = df telling what subpool each resid values belongs to 
    #sim.sp = index of subpools from scen.index that needs to be resampled

    #add scenarioID, Parameter and scen.index; AND SAVE FOR REFERENCE
      bootres.df <- data.frame(bootres, Sim.index[,1:4], ScenarioID= Sim.index$scenID, stringsAsFactors="FALSE")
    
#       setwd(scenNMdir)
#       save(bootres, file="bootres.RData")  
#       save(bootres.df, file="bootres.df.RData")  #alternatively append to master bootres?
#   

return(bootres.df)
}

