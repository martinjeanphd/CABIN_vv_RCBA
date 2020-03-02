
#===============================================================================
# Function DEFINITION for running 1 dataframe (parameter):
#   APPLIES THE Perform_SPowerSims.R function to ONE input dataframe
#===============================================================================

#Function DEFINITION for 1 dataframe:
#df1 = dataset dataframe for 1 parameter only

run.power.df <- function(df1,  savedir, homedir, data.col,                    
                         SimParameters, Sim.Index, subpools, 
                         DS.option, num.harmonics, 
                         DT.option, span.value, flowYN, 
                         trend.levels, I, alpha, SKseas, 
                         LANG, SEP, DEC){
  
  TOT.1par <- Sys.time()
  
  ParName <- unique(df1$Parameter)
  
  cat("############################################################", "\n")
  cat("Start of Power Analysis Simulations for ", ParName, "\n"); cat("\n")
  
  #------------------------------------------------------------------------------------
  #APPLY Perform_SK_Power_Simulations FUNCTION
  #------------------------------------------------------------------------------------

  out <- Perform_SK_PowerSims(df1,  savedir, homedir, data.col, 
                              SimParameters, Sim.Index, subpools, 
                              DS.option, num.harmonics, 
                              DT.option, span.value, flowYN, 
                              trend.levels, I, alpha,SKseas, 
                              LANG, SEP, DEC)
  
  
  cat("\n"); cat("\n")
  cat("End of Power Analysis Simulations for ", ParName, " - ", "\n")
  
  cat(" TOTAL RUNTIME FOR ", ParName, ":"); print(Sys.time()-TOT.1par)
  cat("\n")
  cat("############################################################", "\n")
  cat("\n")

  return(out)
}#end function



#==================================================================================
# FUNCTION TO RUN POWER ANALYSIS FOR ALL PARAMETERS 
#==================================================================================

run.power.All <- function(dfall,  savedir, homedir, data.col,                   
                          SimParameters, Sim.Index, subpools, 
                          DS.option, num.harmonics, 
                          DT.option, span.value, flowYN, 
                          trend.levels, I, alpha,SKseas, 
                          LANG, SEP, DEC) {
  
  
  stndir <- paste(savedir, "/", unique(dfall$StationID), sep="")
  if (!file.exists(stndir)){ dir.create(stndir)}
  setwd(stndir)
  
   
  #------------------------------------------------------------------------------------
  # START AN OUTPUT LOG TO SAVE SCREEN output for power run
  #------------------------------------------------------------------------------------

  sink("LOG.PowerRUN.txt", append=FALSE, split=TRUE)
  
  #------------------------------------------------------------------------------------
  # APPLY run.power.df FUNCTION TO POWER.DATASETS LIST OF DFs
  #------------------------------------------------------------------------------------
  
  TOT.time <- Sys.time()
  
  # out.results.list = list of results DF (1/parameter)
  PowerResults <- ddply(dfall, .(Parameter), run.power.df, 
                            savedir, homedir, data.col,                    
                            SimParameters, Sim.Index, subpools, 
                            DS.option, num.harmonics, 
                            DT.option, span.value, flowYN, 
                            trend.levels, I, alpha,SKseas, 
                            LANG, SEP, DEC)
  
  cat("\n")
  cat(" TOTAL RUNTIME FOR ALL PARAMETERS: "); print(Sys.time()-TOT.time)
  
  #------------------------------------------------------------------------------------
  # EXPORT outall TO XLS
  #------------------------------------------------------------------------------------#
  #source("http://www.omegahat.org/RDCOMClient/examples/excelUtils3.R")    #functions for RDCOMClient
  
  resultsSK.2xls(savedir=SKresultsdir, results.df=PowerResults, savename="SKPowerResults", LANG=LANG)
  
  #------------------------------------------------------------------------------------
  # Save to .RData file to use for graphing power curves
  #------------------------------------------------------------------------------------#
  setwd(SKresultsdir)
  save(PowerResults, file="PowerResults_AllParameters.RData" )
  
  sink()
  
  return(PowerResults)
  
}#end function


