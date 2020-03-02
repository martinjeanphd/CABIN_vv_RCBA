#################################################################################
#  THIS CODE DEFINEs: 
#   1) functions for four different methods for reducing input df to a 
#      consistent sampling frequency:
#         - NperSeason.random
#         - NperSeason.byday
#         - OnePerMonth.byday
#         - NPerMonth.byday
#     2) Function to apply one of the four methods to a df when n>NperSeas
# #################################################################################

#========================================================================
# FUNCTION to reduce to a frequency based on number of week intervals
# (creates an index of dates based on W.INT weeks; 
#  then selects the closest value to the expected sample date within a specified buffer)
#  - the first date in the index is the first date in the record)
#     dat = input dataset to be reduced to a 1 sample every w.int weeks
#     buf = buffer on each side of projected sample date (in DAYS)
#     w.int = interval between sampling dates (in WEEKS)
#
#    addMV = TRUE or FALSE; if TRUE a date with no data will be inserted into the output df
#    (note: all other columns (e.g. Parameter, StationID, will also be NA))
#========================================================================

sample.by.weeks <- function(dat, BUF, W.INT, addMV = FALSE) {
  
  #require(lubridate)
  
  buf1 <- days(BUF)       #period class in lubridate
  w.int1 <- weeks(W.INT)  #period class in lubridate
  
  #-------------------
  # CREATE date index of expected sample dates
  #-------------------
    start.date <- dat$Date[1]
    end.date <- dat$Date[nrow(dat)] + buf1
    #calculate n.int (as number of intervals)
    n.int <- (as.numeric(difftime(end.date, start.date, units="weeks"))/W.INT)+1
    
    int.index <- start.date + c(0:floor(n.int)) * w.int1
    df <- data.frame(int.index)   #dataframe of the expected sample date index
    df$BufL <- with(df, int.index-buf1)
    df$BufU <- with(df, int.index+buf1) 
  
  #-------------------
  # select the closest sample to each expected sample date (df)
  # if one exists in the defined  buffer
  #-------------------
  
  H <- NULL
  for (i in 1:nrow(df)){
    #extract samples withing BufL and BufU
    x1 <- subset(dat, Date <= df$BufU[i] & Date >= df$BufL[i])
    
    #select the sample closest to the int.interval date
    keep <- x1[which.min(abs(x1$Date - df$int.index[i])),]
    
    if(addMV == TRUE){
      if (nrow(keep)==0){   #if no sample dates exist
        x2 <- dat[1,]; x2[1,] <- NA
        x2$Date <- df$int.index[i]
        #x2$Year <- year(x2$Date); x2$Month <- month(x2$Date)
        #x2$decDate <- date2decyear(x2$Date)
        #x2$MonthName <- month(x2$Date, label=TRUE, abbr=TRUE)
        
        keep <- x2
      }
    }
    H <- rbind(H, keep)
  } #end for
  
          
  #----------------
  # Print out to .txt file the data included/excluded
  #---------------
  
    "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
      excl <- rownames(df) %w/o% rownames(H)     #vector of rownames of samples that were removed
      rm <- dat[rownames(dat) %in% excl,]    
      #rm = the rows excluded (removed) from the dataset
  
      #incl <- dat[!rownames(dat) %in% excl, ]  #print(H)
  
  cat("\n");   cat("\n")
  cat("Selection method: ", sel.method, "\n")
  cat("Interval :", W.INT, " week(s)", "\n")
  cat("Buffer :", BUF, " day(s)", "\n")
  cat("\n")
  cat("The following dates were REMOVED from the dataset:", "\n")
  cat("\n")
  print(rm)
  cat("\n")
  cat("The following dates were INCLUDED from the dataset:", "\n")
  cat("\n")
  print(H)
  
  return(H)
     
}#end sample.by.weeks function


#---------------------------------------------------------------------------------------------
# NperSeason.random: function to randomly select NperSeas values within each Season 
# ** input S3 data = subset contining data for ONE year and ONE season only
#---------------------------------------------------------------------------------------------

NperSeas.random <- function(S3, NperSeas){
  #S3 <- subset(S2, Season==M[m])              
  R3 <- as.numeric(rownames(S3))
  
  S4 <- S3[rownames(S3) %in% sample(R3, size=NperSeas, replace = FALSE), ]  
  
  "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
  excl <- rownames(S3) %w/o% rownames(S4)     #vector of rownames of samples that were removed
  S5 <- S3[rownames(S3) %in% excl,]            
  
  cat("   FOR SEASON: ",  unique(S3$Season)); cat("\n")
  cat("   ------------", "\n")
  cat("   The following dates were included in the dataset:", "\n")
  cat("\n")
  print(S4)
  
  cat("\n")
  cat("   The following dates were REMOVED from the dataset:", "\n")
  cat("\n") 
  print(S5)
  cat("\n")
  
  return(S4)
}#end function

#---------------------------------------------------------------------------------
# OnePerMonth.byday: function to select closest a specified day of the month (use 15 for middle) 
#---------------------------------------------------------------------------------

OnePerMonth.byday <- function(S3, day.of.month = 15){
  #S3 <- subset(S2, Season==M[m])              
  #S3 <- subset(S2, Month==M[m])    #Note: assumes monthly seasons 
  #R3 <- as.numeric(rownames(S3))
  
  #CHECK: NperSeas must = length(day.of.month)
#   if(length(day.of.month != NperSeas){
#     stop("The number of values in the day.of.month vector must equal NperSeas")
#   }else {
    
  S3$Day <- day(S3$Date)
  S4 <- S3[which.min(abs(S3$Day-day.of.month)),]
  
  
  "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
  excl <- rownames(S3) %w/o% rownames(S4)     #vector of rownames of samples that were removed
  S5 <- S3[rownames(S3) %in% excl,]            
  
  cat("   FOR ",  as.character(unique(S3$MonthName)), "\n")
  cat("------------", "\n")
  
  cat("   The following dates were included in the dataset:", "\n")
  cat("\n")
  print(S4)
  
  cat("\n")
  cat("   The following dates were REMOVED from the dataset:", "\n")
  cat("\n") 
  print(S5)
  cat("\n")
  
  S4 <- subset(S4, select = -Day )
  
  return(S4)
}#end function
