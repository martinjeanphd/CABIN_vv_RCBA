#===============================================================================
# FUNCTIONS TO CREATE 2 tables using censummary in NADA:
# 
#  1) RANGE.TABLE FUNCTION to record info summary:
#       - total number of samples (N)
#       - record start and end dates
#       - number and percent censored data
#         (% calculated as #values <MDL/total number of samples)
#       - min & max values 
# 
#   2) CenSummary.table FUNCTION to record censored data summary info: 
#       - number of censoring limits in dataset; 
#         (note: will not include a DL if no data is <MDL for that DL)
#       - first and last date for each limit (based on <MDL values in dataset only; 
#         therefore may not represent the exact dates on which the MDL changed), 
#       - percent of dataset below each censoring limit 
#         (calculated as #values <MDL limit/total number of samples)
#===============================================================================

#required packages:
# - NADA

#-----------------------------------------------------------------------------
# Range.table FUNCTION
#-----------------------------------------------------------------------------

#Range.table <- function(TSdata, dateformat){
Range.table <- function(TSdata){
  #TSdata = dataframe with the following columns: Parameter, Date (as Date format), obs, cen (T/F)

  G <- TSdata           
  #param <- G$Parameter[1]      #parameter name

  #G.na <- G[na.omit(G)  #omit NA values from each dataframe individually
  #need to do this because censummary includes NA values in total N and min/max, 
  # and as uncen values at zero censor limit
  
  #if(nrow(G.na) != 0){    #throws error if no data in column
  if(nrow(G) !=0){

    x <- censummary(G$obs, G$cen)
    
    ### CREATE SUMMARY TABLE: showing number of censoring limits with info from x.sum and table.R
    #param, start date, end date, N, min, max, n.cen, prc.cen, n.limit, min.limit, max.limit
  
    summary.table <-  data.frame(Variable=unique(G$Parameter), 
                                  #startdate = as.Date(min(G$Date), format=dateformat),   #first record in dataset
                                  #enddate = as.Date(max(G$Date), format=dateformat),      #last record in dataset    
                                  startdate = min(G$Date),   #first record in dataset
                                  enddate = max(G$Date),      #last record in dataset    
                                  N=x$all[1],                                            #total number of records 
                                  min=x$all[4], max=x$all[5],                            #min and max values
                                  n.cen=x$all[2], pct.cen=x$all[3],                      #number and percent censored data
                                  n.limits=length(x$limit$limit),                        #number of censoring limits*
                                  min.limit=min(x$limit$limit),                          #min censoring limit
                                  max.limit=max(x$limit$limit),                          #max censoring limit
                                  row.names=NULL, stringsAsFactors=FALSE)        
  }#end if   
  
  # *Note: censoring limits are based on cen column; a censoring limit that has no <MDL values represented
  #        will not be counted
  
  ##convert Variable back to a factor
  summary.table$Variable <- as.factor(summary.table$Variable)
    
  #add table name as variable
  summary.table$Tab <- "RangeTable"
 
  return(summary.table)

  }#end function

#-----------------------------------------------------------------------------
# CenSummary.table FUNCTION
#-----------------------------------------------------------------------------

CenSummary.table <- function(TSdata){
  #TSdata = dataframe with the following columns: Parameter, Date (as Date format), obs, cen (T/F)
  
  G <- TSdata           
  #param <- unique(G$Parameter)      #parameter name
  
  #G.na <- G[na.omit(G)  #omit NA values from each dataframe individually
  #need to do this because censummary includes NA values in total N and min/max, 
  # and as uncen values at zero censor limit
  
  #if(nrow(G.na) != 0){    #throws error if no data in column
  if(nrow(G) !=0){
   
    startdate = min(G$Date)   #first record in dataset
    enddate = max(G$Date)    #last record in dataset    
    
    x <- censummary(G$obs, G$cen)
               
    ### CREATE CENSORING LIMIT TABLE using limits from censummary above: 
      # showing prc cen for each censoring limit
      xl <- cbind(Variable=unique(G$Parameter),x$limit,p.cen = (1-x$limit$pexceed))
    
    #remove rows in x.limit where limit=0 (b/c sometimes adds limit = 0 row)
    xl <- xl[which(xl$limit!=0), ]
    
    #Initiate empty range dataframe;then populate
    range <- data.frame(limit=numeric(), 
                        startdate=as.Date(character()), 
                        enddate=as.Date(character()), 
                        stringsAsFactors=FALSE)
    
    
    #initialize summary tables and range vector
    cen.limits.table <- data.frame(Variable=character(), 
                                   limit=numeric(), 
                                   n=numeric(), 
                                   uncen=numeric(), 
                                   startdate=as.Date(character()), 
                                   enddate=as.Date(character()), 
                                    p.cen=numeric(), 
                                   pexceed=numeric(),
                                   stringsAsFactors=FALSE)
    
    
    #
    jj = nrow(cen.limits.table) + 1      
    
    if (nrow(xl) != 0){
      
      m=1
      for (kk in (1:nrow(xl))) {
        
        lim <- xl$limit[kk]
        
        y <- G[which(G$cen==T & G$obs ==lim),]
        first <- min(y$Date)
        last <- max(y$Date)
        range[m,] <- data.frame(lim,first=as.Date(first,format="%Y-%m-%d"),last=as.Date(last,format="%Y-%m-%d"))
        
        m=m+1       
      }# close for
      
      cen.limits.table <- merge(xl,range,all.x=TRUE, all.y=TRUE)
      #new2$Variable <- as.character(new2$Variable)
      
    } else { #to add row for variables with no censored data
      cen.limits.table[jj, "Variable"] <- unique(G$Parameter)
      cen.limits.table[jj, "n"] <- as.numeric(x$all[1])
      cen.limits.table[jj, "p.cen"] <- 0
      cen.limits.table[jj, "uncen"] <-as.numeric(x$all[2])
    }  
    #closed if (nrows(x.limits.omit != 0)) at end of else
    
  }#close if (nrow(G.na)

  #change order of columns
  cen.limits.table <- cen.limits.table[c(2,1,3,4,7,8,6,5)]  
  
  #convert Variable back to a factor
  cen.limits.table$Variable <- as.factor(cen.limits.table$Variable)
  
  #add table name as variable
  cen.limits.table$Tab <- "CenSummary"
  
  return(cen.limits.table)

}#close function


#-----------------------------------------------------------------------------
# freq_MonthlyN FUNCTION
# SUMMARY of the NUMBER of samples by year and Season
#-----------------------------------------------------------------------------

freq_MonthlyN <- function (TSdata) {
       
    if(nrow(TSdata) != 0){
      
      #Frequency Tables by Year/Month for 1 parameter:
      #YM.freq3 <- addmargins(xtabs(~ Year + Month, data=G.na));  YM.freq   #create and add total columns
      #Note: table excludes missing values in counts
      YM.freq<- with(TSdata, table(Year, MonthName))
      YM.freq2 <- rbind(YM.freq, Total=colSums(YM.freq))                          #add row total
      YM.freq <- cbind(YM.freq2, Total=rowSums(YM.freq2))
    
    } else {  #append empty tables with parameter name
      YM.freq <- NULL
    }#end  if else
  
  return(YM.freq)
  
} #end function

#SAME AS: 

#test <- G[,c("Data",  "Year", "MonthName")]
#test2 <- melt(test, id=c("Year", "MonthName"))
#test3 <- dcast(test2, Year ~ MonthName, length,  margins=TRUE)   #subset=.(variable=="Data)

#OR 
#t <- ddply(dat,.(Year, MonthName),summarise,count = length(Data))
#tt <- dcast(t, Year~MonthName, sum, margins=TRUE, value.var="count")

#tt/subset(tt, Year=="(all)")[,c("(all)")]

#-----------------------------------------------------------------------------
# prc_MonthlyN FUNCTION
#-----------------------------------------------------------------------------

prc_MonthlyN <- function (TSdata) {

    if(nrow(TSdata) != 0){
      
      #Show counts as percentage of total samples in table
      YM.prc <- (100*(prop.table(xtabs(~ Year + MonthName, data=TSdata))))  #show counts as percentage of total samples                                 
      YM.prc2 <- rbind(YM.prc, Total=colSums(YM.prc))     #add Total percent 
      YM.prc <- cbind(YM.prc2, Total=rowSums(YM.prc2))  
      YM.prc <- signif(YM.prc, digits=2)                   #round to 2 significant digits
               
    } else {  #append empty tables
      YM.prc <- NULL
      
    }#end  if else
  
  return(YM.prc)
  
} #end function

