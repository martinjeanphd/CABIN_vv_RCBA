create.plotvar1 <- function(dataset.ls, resultsdir) {

#=====================================================================================
# 1) CREATE ADDITIONAL VARIABLES FROM 
#=====================================================================================

  #CREATE A LIST OF PARAMETER NAMES
  #paramlist = vector of all parameter names
  paramlist <- names(dataset.ls)
  assign("paramlist", paramlist, envir = .GlobalEnv)
  
  #CREATE A stationID variable
  ds <- ldply(dataset.ls)
  stationID <- unique(ds$StationID)
  assign("stationID", stationID, envir = .GlobalEnv)


#=====================================================================================
# 2) CREATE PLOTS DIRECTORY IN THE STATION DIR
#=====================================================================================

  #CREATE STATION DIRECTORY IN THE RESULTS FOLDER
  stndir <- paste(resultsdir, "\\", stationID, sep="")   #creates a Station folder under results
  if (file.exists(stndir)){
    cat("A Station directory for ", stationID, " already exists in the resultsdir.", sep="");  cat("\n")
    cat("\n")
  }else {
    dir.create(stndir)
  }
  assign("stndir",stndir, envir = .GlobalEnv)

  plotdir <- paste(stndir, "\\InfoPlots", sep="")   #creates a Plots folder under stndir
  if (file.exists(plotdir)){
    cat("An InfoPlots directory already exists in the stndir for ", stationID,  sep="");  cat("\n")
    cat("\n")
  }else {
    dir.create(plotdir)
  }  
  
  #Note: Can create additional subplot directories if desired
  #Example - create AllPArameterPlots directory
  plot2dir <- paste(plotdir, "\\AllParameterPlots", sep=""); 
  if (file.exists(plot2dir)){
    cat("An AllParameterPlots directory already exists in the InfoPlots directory for ", stationID,  sep="");  cat("\n")
    cat("Warning: Plots may be overwritten");  cat("\n")
    cat("\n")
  }else {
    dir.create(plot2dir)
  }  
  
  plot3dir <- paste(plotdir, "\\ONEParameterPlots", sep="")
  if (file.exists(plot3dir)){
    cat("A ONEParameterPlots directory already exists in the InfoPlots directory for ", stationID,  sep="");  cat("\n")
    cat("Warning: Plots may be overwritten");  cat("\n")
    cat("\n")
  }else {
    dir.create(plot3dir)
  }  
 
    plot4dir <- paste(plotdir, "\\FacetPlots", sep="")
  if (file.exists(plot4dir)){
    cat("A FacetPlots directory already exists in the InfoPlots directory for ", stationID,  sep="");  cat("\n")
    cat("Warning: Plots may be overwritten");  cat("\n")
  }else {
    dir.create(plot4dir)
  }  


}#end function