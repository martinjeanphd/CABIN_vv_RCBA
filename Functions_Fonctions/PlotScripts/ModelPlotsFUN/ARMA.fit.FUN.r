#Isabellas ARMA Code (Revised a bit)

##--------------------------------------------------------------------------------
# Fit an ARMA(p,q) model to the series x
# by trying out various combinations of orders (p,q), where 
# p = {0, 1, 2} and q = {0, 1, 2}.  For each ARMA(p,q) model fit, 
# an AIC value is computed, which combines information on the goodness of the 
# model fit and the model complexity. The ARMA(p,q) model yielding the lowest 
# AIC value is deemed to be optimal for the data, where the data consist of 
# the nonparametric regression residuals computed previously.  
# For this optimal model, p = mi and q = mj.    
##--------------------------------------------------------------------------------


bulkfit.ARMA <- function(x){
 
  #require(TSA)
 
  #-------------------------
  # bulk fit AIC from AR and MA 0 to 2
  #----------------------------
  
  df.AIC <- data.frame(AR=numeric(), MA=numeric(), AIC=numeric())
  minaic <- 99999999
  for (i in seq(0,2)){
    for (j in seq(0,2)) {
      A <- arima(x, order=c(i,0,j), method="CSS-ML")
      aic <- A$aic
      tmp <- data.frame(AR=i, MA=j, AIC=aic)
      df.AIC <- rbind(df.AIC, tmp) 
      if (aic < minaic)
      {
        minaic <- aic
        mi <- i
        mj <- j
      }
    }
  }
  
  assign("df.AIC", df.AIC, envir = .GlobalEnv)
  assign("mi", mi, envir = .GlobalEnv)
  assign("mj", mj, envir = .GlobalEnv)
  assign("minaic", minaic, envir = .GlobalEnv)
  
  #-------------------
  # Re-fit the optimal ARMA(p,q) model to the data and print out a 
  # summary of the model fit
  #-------------------
  
  A <- arima(x, order=c(mi, 0, mj), method="CSS-ML", include.mean = TRUE)
  
  assign("A",  A, envir = .GlobalEnv)

}
#--------------------------
#FUNCTION TO PRINT OUTPUT
#---------------------------
out.cat <- function(minaic, mi, mj, A) {
  
  cat("\n")
  str <- paste("Best AIC achieved = ", as.character(minaic))
  cat(str)
  
  cat("\n")
  str <- paste("   AR order = ", as.character(mi))
  cat(str)
  
  cat("\n")
  str <- paste("   MA order = ", as.character(mj))
  cat(str); cat("\n")
  cat("\n"); cat("------------------------------------","\n")
  
  cat("\n")
  cat("Best ARMA model fit found is as follows:")
  print(A)
  
  cat("\n"); cat("------------------------------------","\n")
  
  B <- portman(A$residuals, 15)
  cat("\n")
  cat("Portmanteu (Box-Ljung) test", "\n")
  cat(sub="Ho: Data are random - no memory", "\n")
  print(B) 
  
}

portman = function(x,lag1 = 15){
  
  #acf(x, lag.max= lag1,na.action = na.pass) 
  acfx = acf(x,lag.max = lag1,na.action = na.pass, plot=FALSE)
  #print(acfx)
  RAWDATA = arima(x,c(0,0,0))
  print(LB.test(RAWDATA,lag1))
  
}


