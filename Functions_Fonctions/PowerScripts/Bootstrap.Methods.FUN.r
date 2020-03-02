#Bootstrap.Methods.FUN.R                                                 

###########################################################################################################
# SUPPORTING FUNCTIONS for bootstrap resampling processes
###########################################################################################################

#=======================================================================
#FUNCTION TO ADD SUBPOOL COLUMN TO THE RESIDUALS DATAFRAME
#=======================================================================

add.subpools<- function(sp.df, residuals, season.df){
  #NOTE:  
  # residuals =  df containin the following columns: DTDS, Season, Month
  # sub.pools is a single subpool scenarios (e.g. spX column from subpools?)
  
  #-------------------------------------------------------------
  #1) ADD a subpool column (spX) to the residuals based on subpool 
  #   matrix defined in sub.pools
  #-------------------------------------------------------------
  
    R <- residuals   
    
    #creates a vector of the sp column in sub.pools
    sp.index <- sp.df$value
    seas.index <- season.df$Season
    
    #creates a vector of the Mth columns in sub.pools
    #month.index <- sub.pools$Mth
    
    #Add a subpool column the the residuals df (R) identifying a subpool by row
      # match returns a vector of the positions of (first) matches of its first argument in its second.
      # Example: 
      #   - R$Month = values to be matched, month.index = values to be matched against 
      #   - match(R$Month,month.index) --> returns an integer vector giving the position in month.index of the first match in R$Month?
      #   - sp.index[R$Month,month.index] --> returns the sp.index value for each position from match vector
      
      R$subpool <- sp.index[match(R$Month,sp.df$Mth)]  
      R$Season <- seas.index[match(R$Month, season.df$Mth)]   
      
      R$spOPT <- unique(sp.df$sp.col)
  
  return(R)
  # R is dataframe w/ new subpool column added
  
}#end add.subpools function
  
#=======================================================================
#FUNCTION TO CREATE THE RESIDUAL DATA POOLS
#=======================================================================

create.resid.ls <- function(R.df){  
#   tmp <- R.df[, names(R.df) %in% c("Date", "Month", "resid", "subpool", "spOPT")]
#   test <-  melt(tmp, id.vars=c("spOPT", "subpool", "Date", "Month"), variable.name="sp.data")
#   
  tmp <- R.df[, names(R.df) %in% c("resid", "subpool", "spOPT")]
  out <-  melt(tmp, id.vars=c("spOPT", "subpool"), variable.name="sp.data")
  
    return(out)
  } #end create.resid.ls


#=======================================================================
# FUNCTION to resample w/in correct subpools; returns 1 resampled value
#=======================================================================

# R.pools = dataframe of residual values for each subpool (for 1 scenarios)

sample.ss <- function(scen.sp,  R.pools){
  
  # scen.sp = 1 value from Sim.sp column from scen.index 
  #           (which identifies the subpool that this Sim.Date 
  #            value should be resampled from)
  
  #Subset the R.pools dataframe for only the scen.sp subpool values
  ## NOTE: if scen.sp doesn't exist in resid list this will create an ERROR
    resid.sp <- subset(R.pools, subpool == scen.sp)
  
  # extract the value column as a vector
    vals <- resid.sp$value
  
  #take random sample from vals
  s <- sample(vals, size=1, replace=TRUE)
  
  return(s)
  
}#end sample.ss function

#=======================================================================
# FUNCTION TO SAMPLE RANDOMLY FROM DEFINED SUBPOOLS 
# - creates a matrix of I sets of innovations (columns named ss) of bootstrap residuals
#=======================================================================

bootres.subpools<- function(sim.sp, R.pools, I){
  #sim.sp = Sim.sp vector for 1 scen.index from SimParameters (ie what subpools each datapt should be resampled from)
  #R.pools = dataframe of residuals for 1 subpools scenario (ie what subpools the resid belong too)
  #source(sample.ss)
  
  #-------------------------------------------------------------
  # APPLY function sample.s I times (where I = number of simulated datasets to be created)
  #-------------------------------------------------------------
  
#   innovations.bootstrap <- matrix(nrow=length(sim.sp), ncol=I, byrow=FALSE)
#   #system.time({
#   for (j in 1:I){
#     innovations.bootstrap[,j] <- aaply(sim.sp, 1, sample.ss, R.pools)  #1 indicates applied over rows  
#   }
#   #})   
  
  innovations.bootstrap <- matrix(nrow=length(sim.sp), ncol=I, byrow=TRUE)
  
  #set.seed(111)  #needed if you want to reproduce innovations.bootstrap
  for(j in 1:length(sim.sp)){  #for each row
    innovations.bootstrap[j,] <- sample.sr(sim.sp[j],R.pools,I)
}

  
  return(innovations.bootstrap)
}#end function


#=======================================================================
# sample by rows instead of individually
#=======================================================================

sample.sr <- function(scen.sp,  R.pools, I){
  resid.sp <- subset(R.pools, subpool == scen.sp)

# extract the value column as a vector
vals <- resid.sp$value

#take random sample from vals
#set.seed(111)  #needed if you want to reproduce s
s <- sample(vals, size=I, replace=TRUE)

return(s)
}  

#=======================================================================
#sample by subpool 
#=======================================================================
# 
# sample.ssub <- function(scen.sp,  R.pools, I){
#   
#   a <- sort(R.pools$subpool, index.return=TRUE)
#   resid.index <- a$ix
# 
#   resid.sp <- subset(R.pools, subpool == scen.sp)
#   
#   # extract the value column as a vector
#   vals <- resid.sp$value
# #which(x %in% c(2,4))
  