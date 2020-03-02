
#--------------------------------------------------------------------------
#FUNCTION TO extract row with smallest value greater than a given power level
#---------------------------------------------------------------------------

ext.MDL <- function(df, pwr.lvl=0.8){
  pwr.lvl <- as.numeric(pwr.lvl)
  alpha <- unique(df$alpha)

  
  x <- subset(df, power >= pwr.lvl)
  min.x <- subset(x, power == min(x$power))
  if (nrow(x)==0){=
    xx <- df[df$TPC==max(df$TPC),]
    xx$TPC <- paste(">", max(df$TPC))
                    #not finished fixing past here
    xx$Tot.Trend[1] <- paste(">", xx$Tot.Trend[1])
    xx$Slope[1] <- paste(">", xx$Slope[1])
    xx$Slope.prc[1] <- paste(">", xx$Slope.prc[1]) 
    
  }else (xx <- x[which.min(x[,alpha.index]),])
  return(xx)
}
