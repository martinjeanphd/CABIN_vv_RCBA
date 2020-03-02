######################################################
# Check to ensure that there is at least
####################################################

#dataset
#df <- dataset.NDNA.C[[1]]

#scenarios
#scenario <- scenarios[1,]

check.sp <- function(df, scenarios, subpools, cnt){
  
  sink(paste("Check npts per subpool by scenario.txt", sep=""),split=TRUE, append=TRUE)
  
  SP.CNT <- data.frame(subpool=numeric(), npts=numeric(), scenID =character())
  
  cat("***************PARAMETER: ",unique(df$Parameter),"**************************", "\n")
 
  for (i in 1:nrow(scenarios)){
    
    #-------------------------------
    #1) add subpools column to df  
    #-------------------------------
  
    scen1 <- scenarios[i,]
    
    #creates a vector of the sp column in sub.pools, and extract vector from subpools
    sp.opt <- scen1$sp.option
    sp.ind <- subpools[,names(subpools) == sp.opt]
  
    df$subpool <- sp.ind[match(df$Month,subpools$Mth)]  
  
    #-------------------------------
    #2)split Data by subpools
    #-------------------------------
    sp.indcnt <- data.frame(subpool=sp.ind)
    sp.cnt1 <- ddply(df, .(subpool), summarize, count=length(Data))
    #add in any missing subpools from sp1 as count = 0 
    sp.cnt <- merge(sp.indcnt, sp.cnt1, by="subpool", all.x=TRUE)
    sp.cnt[is.na(sp.cnt)] <- 0

    sp.cnt$scenID <- as.character(scen1$scenID)
 
    cat("Number of data points by subpool group for scenario ",as.character(scen1$scenID)," :", "\n")
    cat("------------------------------------", "\n")  
    print(sp.cnt)
    cat("\n")
    
    SP.CNT <- rbind(SP.CNT, sp.cnt)
  }#end for
    
    #-------------------------------
    #2) print out scenarios that do not contain sufficient data
    #-------------------------------
  
    fun2 <- function(cnt.df, npts){
      a <- subset(cnt.df, cnt.df$count <= npts)
      nr <- nrow(a)
      return(nr)
    }
    
    fun3 <- function(cnt.df, npts){
      a <- subset(cnt.df, cnt.df$count <= npts)
      scen.nm <- unique(cnt.df$scenID)
      for (i in 1:length(scen.nm)){
        scen.nm1 <- scen.nm[i]
        gr <- unique(a$subpool)
        
        cat("    Scenario ", scen.nm1, " has <= ", npts, " datapoint in the following subpool groups: ", paste(gr, collapse = ' '), sep="")
        cat("\n")
      }
    }
  
  cat("Scenarios with ZERO datapoints in one subpool group will produce an ERROR", "\n")
  cat("---------------------------------------------------------------------------", "\n")
  xx3 <- ddply(SP.CNT, .(scenID), fun2, npts=0)
  if (!any(xx3$V1 > 0)) {
    cat("All Scenarios have greater than ", 0, " data points in each subpool grouping.", sep="")
    cat("\n"); cat("OK to proceed with all scenarios"); cat("\n")
  } else if (any(xx3$V1 > 0)) {
    cat("The following scenarios contain subpools groupings which contain no datapoints.", sep="")
    cat("\n"); cat("The Power Code cannot run these scenarios."); cat("\n")
    cat("\n"); cat("Suggest Removing these scenarios OR change the subpool option. "); cat("\n")
    fun3(SP.CNT, npts=0)
  }
  cat("\n")
  
  cat("Scenarios with ", cnt, "datapoints in one subpool group are not recommended", "\n")
  cat("---------------------------------------------------------------------------", "\n")
  xx3 <- ddply(SP.CNT, .(scenID), fun2, npts=cnt)
  if (!any(xx3$V1 > 0)) {
    cat("All Scenarios have greater than ", cnt, " data points in each subpool grouping.", sep="")
    cat("\n"); cat("OK to proceed with all scenarios"); cat("\n")
  } else if (any(xx3$V1 > 0)) {
    cat("The following scenarios contain subpools groupings which contain less than ", cnt, " datapoints.", sep="")
    cat("\n"); cat("It is not recommended to run these scenarios."); cat("\n")
    fun3(SP.CNT, npts=cnt)
  }
  cat("\n")
  cat("============================================", "\n")  
  cat("\n")
  sink()
}