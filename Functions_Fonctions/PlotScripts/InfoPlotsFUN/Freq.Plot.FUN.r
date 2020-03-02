#===============================================================================
# GRAPH: Create Frequency Graphs by Month and Year
#===============================================================================

#------------------------------------------------------------------
# Function to print N samples/year : N, Num.Months by Year
#------------------------------------------------------------------
YNoS.tableYR <- function(df){
  cat("Sample Frequency for",  unique(df$Parameter),"\n")
  
  YNoS <- ddply(df, .(Year), summarise, 
                N    = length(obs),
                Num.Months.Sampled = length(unique(Month))
  )
  
  #add stationID to df
  YNoS$Parameter<- unique(df$Parameter)
  
  print(YNoS)
  
  return(YNoS) 
} #end function


#--------------------------------------------
# Function to print plot of Frequency by Year
#--------------------------------------------
YNoS.plotYR<- function(df){

  #require(ggplot2)
  
  YNoS <- ddply(df, .(Year), summarise, 
                N    = length(obs),
                Num.Months.Sampled = length(unique(Month))
  )
  
  
  YNoS$Year <- as.factor(YNoS$Year)

  
  p <- ggplot(data=YNoS, aes(x=Year, y=N)) + 
    geom_bar(colour="black", stat="identity", fill="grey") + 
    guides(fill=FALSE) +
    scale_y_discrete("N", name="Number of Samples") + 
    scale_x_discrete(levels(YNoS$Year), name="Year") +
    theme_bw() +
    geom_text(aes(label = N), hjust=-0.3) +  
    coord_flip() 
  
  return(p)
}


#-----------------------------------------------------------------
#Function to print number of samples by Month over record
# N, Num.Months by Month
#-----------------------------------------------------------------
YNoS.tableMTH <- function(df){
  cat("Number of Samples per Month for", unique(df$Parameter),"\n")
  
  YNoM <- ddply(df, .(MonthName), summarise, 
                N.Yrs    = length(unique(Year)),
                N.Samples = length(obs)
  )
  
  print(YNoM)
  
  #add stationID to df
  YNoM$Parameter<-  unique(df$Parameter)
  
  return(YNoM) 
} #end function
#test <- YNoS.tableMTH(dataset.cen.NDNA[[1]])




#--------------------------------------------
# Function to print plot of frequency by Month
#--------------------------------------------
#require(ggplot2)

YNoM.plot<- function(df){
  
  YNoM <- ddply(df, .(MonthName), summarise, 
                N.Yrs    = length(unique(Year)),
                N.Samples = length(obs)
  )
  
  p <- ggplot(data=YNoM, aes(x=MonthName, y=N.Samples)) + 
    geom_bar(colour="black", stat="identity", fill="grey") + 
    guides(fill=FALSE) +
    scale_y_discrete("N.Samples", name="Number of Samples") + 
    #ylab("Number of Samples")+
    scale_x_discrete(levels(df$MonthName), name="Month") +
    theme_bw() +
    geom_text(aes(label = N.Samples), vjust=-0.8) #+  
    #coord_flip() 
  
  return(p)
}


#--------------------------------------------
# Function to print off N by Month and Year
#--------------------------------------------

YNoS.tableYM <- function(df){
  cat("Number of Samples per Month over Record for", df$Parameter[1],"\n")
  
  YNoM <- ddply(df, .(MonthName, Year), summarise, 
                N.Yrs    = length(unique(Year)),
                N.Samples = length(obs), .drop=FALSE
  )
  
  print(YNoM)
  
  #add stationID to df
  YNoM$Parameter<- df$Parameter[1]
  
  return(YNoM) 
} #end function
#test <- YNoS.tableYM(dataset.cen.NDNA[[1]])

#--------------------------------------------
# Function to print plot of frequency by Month AND YEAR
#--------------------------------------------

YNoM.plotYM<- function(df){
  
  #require(ggplot2)
  
  YNoM <- ddply(df, .(MonthName, Year), summarise, 
                N.Yrs    = length(unique(Year)),
                N.Samples = length(obs), .drop=FALSE
  )
  
  #idx <- which(YNoM$N.Samples==0)
  #YNoM[idx, "N.Samples"] <- NULL
  YNoM$N.Samples <- as.factor(YNoM$N.Samples)
  
  # compare with
  p <-ggplot(data=YNoM, aes(x=MonthName, y=N.Samples, fill=N.Samples)) + 
    geom_bar(stat="identity") +
    facet_wrap(~ Year, ncol=25) +
    facet_grid(.~Year) +
    guides(fill=FALSE) +
    xlab("Month (Jan-Dec)") + ylab("Number of Samples") +
    ggtitle(paste("Monthly Sample Frequency over Record by Year")) +
    theme_bw() + theme(axis.text.x=element_blank())
  
  return(p)
}


#---------------------------------------------------------------------
# FUNCTION TO PLOT YNoM.plotYM, YNoM.plot, YNoS.plot on 1 page
#---------------------------------------------------------------------
AllFreq.Plots<- function (df) {
  
  cat("Plotting: Frequency Plots (AllFreq.Plots) for ", unique(df$Parameter), "\n") 
  
  #Nsamples by Year
  p1 <- YNoS.plotYR(df)
  
  #Nsamples by Month
  p2 <- YNoM.plot(df)
  
  #Nsamples per Month and Year
  p3<- YNoM.plotYM(df)
  
#  lo <- matrix(c(1,2,3,3), nrow=2,byrow=T)
#  multiplot(p1,p2,p3, layout=lo)
  
  #OR TO INCLUDE TITLE: 
  grid.newpage()
   pushViewport(viewport(layout = grid.layout(3, 2, heights = unit(c(0.5, 4, 4), "null"))))
   grid.text(paste("FREQUENCY PLOTS FOR ", unique(df$Parameter), " at ", unique(df$StationID)),
             vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
   print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
   print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
   print(p3, vp = viewport(layout.pos.row = 3, layout.pos.col = 1:2))

  #print(mp)
  #return(mp)
  
}#end function







  