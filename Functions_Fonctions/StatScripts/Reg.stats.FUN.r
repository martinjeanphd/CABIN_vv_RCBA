##########################################
# FUNCTION TO CALCULATE 'regular' STATS
##########################################

Reg.stats <- function(df,data.col){ 
  
  #install.packages("e1071")
  require("e1071")  
  
  cat("Running Regular Stats (Reg.stats)", "\n")   
  
  G <- df
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  

  
  tmp <- ddply(G, .(Parameter), summarise,
               StationID = unique(StationID),
               nbr.null = sum(is.na(datacol)), 
               N = sum(!is.na(datacol)), 
               min=min(datacol, na.rm=TRUE),
               max=max(datacol, na.rm=TRUE),
               mean=mean(datacol, na.rm=TRUE), 
               median=median(datacol, na.rm=TRUE), 
               var = (sd(datacol, na.rm=TRUE))^2,
               sd = sd(datacol, na.rm=TRUE), 
               SE.mean=sd(datacol, na.rm=TRUE)/sqrt(sum(!is.na(datacol))),
               CV = sd(datacol, na.rm=TRUE)/mean(datacol, na.rm=TRUE),
              skew = skewness(datacol, na.rm=TRUE), 
              kurt = kurtosis(datacol, na.rm=TRUE)
              )
  
  #--------------------------------------------------------------
  # FUNCTION FOR stratified seasonal median for the original data 
  sms <-function(dat){
    s.med <- median(as.numeric(unlist(by(dat$datacol, dat$Month, median, na.rm=T))))
    s.mean <- mean(as.numeric(unlist(by(dat$datacol, dat$Month, mean, na.rm=T))))
    s.df <- data.frame(s.med=s.med, s.mean=s.mean)
    row.names(s.df)<- names(dat)[1]
    return(s.df)
  } 
  #--------------------------------------------------------------
  
  s.stat <- ddply(G, .(Parameter), sms) 
   
  tmp1 <- merge(tmp, s.stat, by="Parameter")
  
  tmp1$Method <- "Reg.Stats"
  tmp1$datacol <- data.col
  detach(package:e1071)
  

  return(tmp1)
}
#end regular stats function

#To RUN FOR MONTHLY DATA: 
#Reg.stats.month <-  ddply(df, .(MonthName), Reg.stats, data.col="Data") ; Reg.stats.month
