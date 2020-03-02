
#------------------------------------------------------------------------
#Check: 
# 1) ALL SCENARIOS MUST HAVE GREATER THAN 3 DATAPOINTS IN EACH SEASON, 
#       otherwise 'Kendall' will fail 
#       (i.e. for monthly data, would need >3 years of data w/ no missing values in any season)
#
#2) MAY GET WARNING: Error exit, tauk2. IFAULT =  12 if n in each season is <4
#   (pvalues may not be accurate)
#------------------------------------------------------------------------

#------------------------------------------------------------------------
count.npts <- function(df){
  
  nn <- ddply(df, .( scenID,Sim.Mth), summarize, count=length(Sim.Mth)) 
  tmp <- melt(nn, id=c("scenID", "Sim.Mth"))
  tmp2 <- dcast(tmp, scenID~Sim.Mth)
  return(tmp2)
}  
#------------------------------------------------------------------------
# cp <- count.npts(ldply(scen.index))



#------------------------------------------------------------------------
count.npts.bySKSeas <- function(df){
  
  nn <- ddply(df, .(scenID,SK.Season), summarize, count=length(SK.Season)) 
  tmp <- melt(nn, id=c("scenID", "SK.Season"))
  tmp2 <- dcast(tmp, scenID~SK.Season)
  return(tmp2)
}  
#------------------------------------------------------------------------



#------------------------------------------------------------------------
check.npts <- function(scen.ind, season.SK){
  
  sink(paste("Check npts per SK group.txt", sep=""),split=TRUE)
  
  Mth.index <- rep(1:12)
  SK.index <- season.SK
  
  fun1 <- function(df, season.SK, Mth.index){
    xx <- SK.index[match(df$Sim.Mth, Mth.index)]
    df$SK.Season <- xx
    return(df)
  }

 df1 <- llply(scen.ind, fun1, season.SK=season.SK, Mth.index=Mth.index)
    
 cp2 <- count.npts.bySKSeas(ldply(df1))
  
 cp2.melt <- melt(cp2, id=c("scenID"))
  
 cat("Number of data points by SK group for each scenario : ", "\n")
 cat("------------------------------------", "\n")  
 print(cp2)
  cat("\n")
 
 fun2 <- function(df, npts){
  a <- subset(df, df$value < npts)
  nr <- nrow(a)
  return(nr)
}
  
  fun3 <- function(df, npts){
    a <- subset(df, df$value < npts)
    scen.nm <- unique(a$scenID)
    for (i in 1:length(scen.nm)){
      scen.nm1 <- scen.nm[i]
      SKgr <- unique(a$variable)
            
      cat("    Scenario ", scen.nm1, " has < ", npts, " datapoint in SK groups: ", paste(SKgr, collapse = ' '), sep="")
      cat("\n")
    }
  }
  
  cat("Scenarios with <3 datapoints per group in SK trend test will not run", "\n")
  cat("---------------------------------------------------------------------------", "\n")
  xx3 <- ddply(cp2.melt, .(scenID, variable), fun2, npts=3)
  if (!any(xx3$V1 >0)) {
    cat("All Scenarios have greater than ", 3, " data points in each SK grouping.", sep="")
    cat("\n"); cat("OK to proceed with all scenarios"); cat("\n")
  } else if (any(xx3$V1 >0)) {
    cat("The following scenarios contain SK groupings with less than ", 3, " datapoints.", sep="")
    cat("\n"); cat("The Power Code cannot run these scenarios."); cat("\n")
    fun3(cp2.melt, npts=3)
  }
  cat("\n")
  cat("Scenarios with <4 datapoints per group in SK trend test will run, but pvalues may not be accurate", "\n")
  cat("---------------------------------------------------------------------------", "\n")
  xx12 <- ddply(cp2.melt, .(scenID, variable), fun2, npts=4)
    if (!any(xx12$V1 >0)) {
      cat("All Scenarios have greater than ", 4, " data points in each SK grouping.", sep="")
      cat("\n"); cat("OK to proceed with all scenarios"); cat("\n")
    } else if (any(xx12$V1 >0)) {
      cat("The following scenarios contain SK groupings with less than ", 4, " datapoints.", sep="")
      cat("\n"); cat("The Power Code will run, but p-values may not be accurate."); cat("\n")
      cat("The 'Error exit, tauk2. IFAULT =  12' will be shown."); cat("\n")
      cat("\n")
      fun3(cp2.melt, npts=4)
    }
  sink()
}