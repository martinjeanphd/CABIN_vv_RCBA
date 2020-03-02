#add.beta                                                           #add.beta

#################################################################################
#  FUNCTION TO ADD BETA VALUES TO SIMULATION PARAMETERS
#
#  1) DEFINES BETA from defined trend.level 
#  2) ADDS BETA vector to SimParameters List
#
#  Output: SimParameters dataframe new columns for beta values
#
#################################################################################

#SimParams <- subset(SimParameter, scenID=="SCENNAME")                      #1 set of simulation parameters as list of 1
#trend.levels = 
#data.df = column from dataset to use for defining stratefied median

#=====================================================================
# FUNCTION TO add.beta where trend.levels are defined as:
# Total Trend (as %of stratified median) over the period of record
#====================================================================

add.beta.CONC <-function(SimParams, trend.levels, sm) {
  #trend.levels are defined as a Total Trend (as %of stratified median)
  
  
  #mgL.tot <- (trend.levels/100)*sm
  #mgL.yr <- ((trend.levels/100)*sm)/SimParams$NSimYears
  
  #c=%change per year
  c <- trend.levels/SimParams$NSimYears
  beta <- (c/100)*sm  #change per year in units
  
  #nt <- length(trend.levels)   #=number of columns to add
  
  for (i in 1:length(trend.levels)){
    tmp1 <- beta[i]
    SimParams <- cbind(SimParams, tmp1)
    names(SimParams)
    names(SimParams)[grep("^[tmp1]", names(SimParams))] <- paste("beta_", trend.levels[i], sep="")
  }
  
  return(SimParams)
  
}#end add.beta.CONC

#=====================================================================
# FUNCTION TO add.beta where trend.levels are defined as:
# % annual change
#====================================================================

add.beta.APC <- function(SimParams, trend.levels){
  
  beta <- log(1+(trend.levels/100))
  
  for (i in 1:length(trend.levels)){
    tmp1 <- beta[i]
    SimParams <- cbind(SimParams, tmp1)
    names(SimParams)
    names(SimParams)[grep("^[tmp1]", names(SimParams))] <- paste("beta_", trend.levels[i], sep="")
  }
  
  return(SimParams)
  
}
