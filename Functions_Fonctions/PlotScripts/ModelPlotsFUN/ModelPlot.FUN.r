
##--------------------------------------------------------------------------------
#function to identify if trend is NS or SIG
##--------------------------------------------------------------------------------
S.NS <- function(SKplot){
  if(SKplot$PVAL < 0.05){  #reject Ho: No trend
    p.sig <- "SIG Trend"
  }else p.sig <- "NS Trend"
}


################################################################################
## FUNCTION TO PLOT "How well has the model captured the trend present in the data?"
################################################################################

model.plot.trend <- function(dat, savedir=NULL){
  
  #source(seaken_noout)

  stnID <- unique(dat$StationID)
  param <- unique(dat$Parameter)
  span.value <- unique(dat$loess.span)

  #add data.col to DTDS.data
  dat$datacol <-dat[,grep(paste("^",unique(dat$Data.Method),"$", sep=""), names(dat), value=TRUE)]   
  
 
  #if savedirectory is specified save as pdf
  if(!is.null(savedir)){
    setwd(savedir)
    pdf(paste(stnID, "_", param, "_DT_Model_Fit.pdf"), width=11,height=8.5)
  }  
  
  xrange=with(dat, c(min(dates.dec), max(dates.dec)))

if(unique(dat$DTmethod) != "None"){
  #-----------------------------------------------
  # TOP PANEL: Observed concentrations and the LOESS smoothed values 
  #-----------------------------------------------
  #par(mfrow=c(3,1))
  par(oma=c(1,2,1,1))
  
  par(fig=c(0,1, 0.55, 1))
  par(mar=c(0,4,4,2))
  
  #plot(model.T$y~model.T$x, cex=1.2, xlab="",pch=1, ylab=paste(param, "Conc."),xlim=xrange, xaxt="n")
  plot(dat$datacol~dat$dates.dec, cex=0.8, cex.axis =0.7, cex.lab = 0.7,
       xlab="",pch=1, ylab=paste(param, "Conc."),xlim=xrange, xaxt="n", yaxt="n")
  axis(2, las=2, cex.axis=0.7)
  axis(1, at=dat$Year, tick=TRUE,labels=FALSE, tck=0.02)
  
  
 
  #plot Sen's slope estimate
  SK.orig <- seaken_noout(x=dat$dates.dec, y=dat$datacol, group=dat$Season)
  p.sig3 <- S.NS(SK.orig)
  abline(a=SK.orig$INT,b=SK.orig$SLOPE,lwd=1,col = "red")
  mtext(paste("Seasonal Sen Slope Estimate=",round(SK.orig$SLOPE, 6),""), line=-1, cex=0.5, adj=1)
  mtext(paste("p.value=", round(SK.orig$PVAL, 4), "(", p.sig3, ")"),line=-1.5, cex=0.5, adj=1)
 
  #lines(model.T$x, model.T$y, type="l", col="black", lty=2)
  #lines(model.T$x, fitted(model.T), col="blue")
  lines(dat$dates.dec, dat$datacol, type="l", col="black", lty=1)
 
    lines(dat$dates.dec, dat$DT.fit, col="blue")
    legend("topleft", c("Lowess","Sen Slope"), bty="n",lty=c(1,1), lwd=c(1,1),col=c("blue","red"), cex=0.7)
 
  #-----------------------------------------------
  #SECOND PANEL: The difference bewteen obs and smoothed values (residuals=DT data) 
  #-----------------------------------------------
  par (fig=c(0,1,0.33, 0.55),new=T)
  par(mar= c(0,4,0,2))
  
  #plot(model.T$residuals~model.T$x, cex=1.2, xlab="",pch=1, ylab=paste("DT"), xlim=xrange, xaxt="n")
  plot(dat$DT~dat$dates.dec,  xlab="",cex=0.8, cex.axis =0.7, cex.lab = 0.7,
       pch=1, ylab=paste("DT residuals"), xlim=xrange, xaxt="n", yaxt="n")
  axis(2, las=2, cex.axis=0.7)
  axis(1, at=dat$Year, tick=TRUE,labels=FALSE, tck=0.02)
  
  #lines(model.T$residuals~model.T$x, lty=2)
  lines(dat$DT~dat$dates.dec, lty=2)
  
  #SK.DT <- seaken(x=dat$dates.dec, y=dat$DT, group=dat$Season)
  SK.DT <- seaken_noout(x=dat$dates.dec, y=dat$DT, group=dat$Season)
  p.sig2 <- S.NS(SK.DT)
  abline(SK.DT$INT,SK.DT$SLOPE,lwd=1,col = "red")
  mtext(paste("Seasonal Sen Slope Estimate=",round(SK.DT$SLOPE, 6),""), line=-1,cex=0.5, adj=1)
  mtext(paste("p.value=", round(SK.DT$PVAL, 4), "(", p.sig2, ")"),line=-1.5,cex=0.5, adj=1)
  
  #add horizontal line at h=0 (zero trend)
  abline(h=0, col=2, lty=2, tck=-0.05)
  
  #-----------------------------------------------
  #THIRD PANEL: DT Residuals plot
  #-----------------------------------------------
  par (fig=c(0,1,0, 0.33),new=T)
  par(mar= c(3,4,0,2))
  
  plot(dat$DT~dat$dates.dec, type="h", ylab="DT residuals", cex=0.8,  cex.lab=0.7,cex.axis =0.7, yaxt="n", xaxt="n")
  axis(2, las=2, cex.axis=0.7)
  axis(1, at=dat$Year, las=2, cex.axis=0.6)
  
  #plot(model.T$residuals~model.T$x, type="h", ylab="DT residuals")
  abline(h=0, col=2, lty=2)
  
 
    title(paste("LOWESS TREND MODEL for ",param," at ", stnID, sep=""), outer = TRUE, line = -2)
  
}else {
    
  plot(dat$datacol~dat$dates.dec, cex=0.8, cex.axis =0.7, cex.lab = 0.7,
         xlab="",pch=1, ylab=paste(param, "Conc."),xlim=xrange, xaxt="n", yaxt="n", 
         main=paste("Plot Showing Seasonal Sens Slope for ",param," at ", stnID, sep=""), 
         sub="Note: No seasonal model applied to dataset")
    axis(2, las=2, cex.axis=0.7)
    axis(1, at=dat$Year, tick=TRUE,labels=FALSE, tck=0.02)
    
    
    
    #plot Sen's slope estimate
    SK.orig <- seaken_noout(x=dat$dates.dec, y=dat$datacol, group=dat$Season)
    p.sig3 <- S.NS(SK.orig)
    abline(a=SK.orig$INT,b=SK.orig$SLOPE,lwd=1,col = "red")
    mtext(paste("Seasonal Sen Slope Estimate=",round(SK.orig$SLOPE, 6),""), line=-1, cex=0.5, adj=1)
    mtext(paste("p.value=", round(SK.orig$PVAL, 4), "(", p.sig3, ")"),line=-1.5, cex=0.5, adj=1)
    
    #lines(model.T$x, model.T$y, type="l", col="black", lty=2)
    #lines(model.T$x, fitted(model.T), col="blue")
    lines(dat$dates.dec, dat$datacol, type="l", col="black", lty=1)
    
    lines(dat$dates.dec, dat$DT.fit, col="blue")
    legend("topleft", c("Sen Slope"), bty="n",lty=c(1), lwd=c(1),col=c("red"), cex=0.7)
    
  }

  if(!is.null(savedir)){dev.off()}
}#end function

#################################################################################
## How well has the model captured the seasonal cycles present in the data?
################################################################################

model.plot.seas <- function(dat, savedir=NULL){
  
  stnID <- unique(dat$StationID)
  param <- unique(dat$Parameter)
  
  #add data.col to DTDS.data
  dat$datacol <-dat[,grep(paste("^",unique(dat$Data.Method),"$", sep=""), names(dat), value=TRUE)]   
  
  
  #if savedirectory is specified save as pdf
  if(!is.null(savedir)){
    setwd(savedir)
    pdf(paste(stnID, "_", param, "_DS_Model_Fit.pdf"), width=11,height=8.5)
  }  

  layout(matrix(c(1,1,1,2,3,4), 2, 3, byrow = TRUE), height=c(0.65,1))
  
  xrange <- with(dat, c(min(dates.dec), max(dates.dec)))
  yrange <- c(min(dat$DT), max(dat$datacol))
  
  #calculate seasonal medians for the DT dataset
  seas.medDT <- by(dat$DT, dat$Season, median, na.rm=T)
  seas.medDT.df <- data.frame(cbind(SM = as.numeric(seas.medDT), Season = dimnames(seas.medDT)[[1]]), stringsAsFactors=FALSE)
  dat$SM.DT <-  seas.medDT.df$SM[match(dat$Season, seas.medDT.df$Season)]
  
  ## Plot seasonal fitted values 
  plot(dat$DT ~ dat$dates.dec, pch="*", cex=0.8, ylab="DT Residuals", xlab="", ylim=yrange, 
       xaxt="n", yaxt="n")
  lines(dat$DT ~ dat$dates.dec, type="l", col="black", lty=2)
  axis(2, las=2, cex.axis=0.7)
  axis(1, at=dat$Year, las=1, cex.axis=0.8)
  
  grid(ny=NA)
  
  if(unique(dat$DSmethod) == "SeasMed"){
    #lines(fitted(model.S) ~dat$dates.dec, type="l", col="red", lty=1, lwd=2)
    lines(dat$DTDS.fit~ dat$dates.dec, type="l", col="blue", lty=1, lwd=2)
    #add lengend
    legend("topright", # places a legend at the appropriate place 
           c("Seasonal Medians"), # puts text in the legend      
           lty=c(1), # gives the legend appropriate symbols (lines) 
           lwd=c(0.5),col=c("blue") # gives the legend lines the correct color and width
    )
  }else if (unique(dat$DSmethod) == "Fourier"){
    #overlay the "sin/cos" fitted values
    lines(dat$DTDS.fit~ dat$dates.dec, type="l", col="red", lty=1, lwd=2)
    #overlay seasonal median plot
    lines(dat$SM.DT ~ dat$dates.dec, type="l", col="blue", lty=1, lwd=1)
    #add lengend
    legend("topright", # places a legend at the appropriate place 
           c("Fitted Sin/Cos","Seasonal Medians"), # puts text in the legend      
           lty=c(1,1), # gives the legend appropriate symbols (lines) 
           lwd=c(1,0.5),col=c("red","blue") # gives the legend lines the correct color and width
    )  
  }else if (unique(dat$DSmethod) == "None"){
    lines(dat$SM.DT ~ dat$dates.dec, type="l", col="blue", lty=1, lwd=1)
    legend("topright", # places a legend at the appropriate place 
           c("Seasonal Medians"), # puts text in the legend      
           lty=c(1), # gives the legend appropriate symbols (lines) 
           lwd=c(0.5),col=c("blue") # gives the legend lines the correct color and width
    )
  }

  
 
  

  ## Boxplot the original data for each season - ORIGINAL DATA
  seasonal.medians <- by(dat$datacol, dat$Season, median, na.rm=T)
  seasonal.medians <- as.numeric(seasonal.medians)       
  SM1 <- median(seasonal.medians)
  dat$DTpS <- dat$DT + SM1
  dat$DTDSpS <- dat$DTDS + SM1
  alldat <- c(dat$DTpS, dat$datacol, dat$DTDSpS)
  yrange.b <- c(min(alldat), max(alldat))

  boxplot(dat$datacol ~ dat$Season, cex.axis=0.8,xlab="Season", ylab="Original Conc.", main="ORIGINAL CONC.", ylim=yrange.b)

  #add seasonal median line to plot
  
  lines(1:length(unique(dat$Season)), seasonal.medians, col="blue", lwd=2)
  
  abline(h=SM1, col=2, lty=2)
  
  #add lengend
  legend("topright", # places a legend at the appropriate place 
         c("Overall Stratified Median","Seasonal Medians"), # puts text in the legend      
         lty=c(2,1), # gives the legend appropriate symbols (lines) 
         lwd=c(0.5,0.5),col=c("red","blue"),# gives the legend lines the correct color and width
         cex=0.6,  bty="n"
         )
  
  
# Boxplot for each season - DT DATA
#     boxplot(dat$DT ~ dat$Season, cex.axis=0.8, xlab="Month", ylab="DT Residuals", main="DETRENDED RESIDUALS", ylim=yrange)
#     seasonal.medians <- by(dat$DT, dat$Season, median, na.rm=T)
#     seasonal.medians <- as.numeric(seasonal.medians)         
#     lines(1:length(unique(dat$Season)), seasonal.medians, col="blue", lwd=2)
#     #abline(h=0, col=2, lty=2)
#     abline(h=SM1, col=2, lty=2)

#Boxplot of DT residuals(w/ median added)
   
   #dat$DTpS <- dat$DT + SM1
   if(unique(dat$DTmethod) != "None"){
     boxplot(dat$DTpS ~ dat$Season, cex.axis=0.8, xlab="Season", ylab="DT + stratified median", main="DT + Stratified Median", ylim=yrange.b)
      seasonal.medians <- by(dat$DTpS, dat$Season, median, na.rm=T)
      seasonal.medians <- as.numeric(seasonal.medians)         
      lines(1:length(unique(dat$Season)), seasonal.medians, col="blue", lwd=2)
      abline(h=SM1, col=2, lty=2)
   }else if (unique(dat$DTmethod) == "None"){
     plot.new()
     mtext("Dataset was Not DETRENDED")
   }
  ## Boxplot for each season - DTDS DATA (no median added)
#     boxplot(dat$DTDS ~ dat$Season, cex.axis=0.8,xlab="Month", ylab="DTDS Residuals",  main="DTDS RESIDUALS", ylim=yrange)
#     seasonal.medians <- by(dat$DTDS, dat$Season, median, na.rm=T)
#     seasonal.medians <- as.numeric(seasonal.medians)         
#     lines(1:length(unique(dat$Season)), seasonal.medians, col="blue", lwd=2)
#     abline(h=0, col=2, lty=2)
  
  #OR w/ median added
   #dat$DTDSpS <- dat$DTDS + SM1
  if(unique(dat$DSmethod) != "None"){
   boxplot(dat$DTDSpS ~ dat$Season, cex.axis=0.8,xlab="Season", ylab="DTDS + Stratified Median",  main="DTDS + Stratified Median", ylim=yrange.b)
   seasonal.medians <- by(dat$DTDSpS, dat$Season, median, na.rm=T)
   seasonal.medians <- as.numeric(seasonal.medians)         
   lines(1:length(unique(dat$Season)), seasonal.medians, col="blue", lwd=2)
   abline(h=SM1, col=2, lty=2)
  }else if (unique(dat$DSmethod) == "None"){
  plot.new()
  mtext("Dataset was Not DESEASONALIZED")
  }
  
  
  k=unique(DTDS.data$harmonics)
  
  if(unique(dat$DSmethod)=="Fourier"){
    title(paste(k," harmonics sin/cos SEASONAL MODEL for ",param," at ", stnID, sep=""), outer = TRUE, line = -2)
  }else {
    title(paste("SEASONAL MODELS for ",param," at ", stnID, sep=""), outer = TRUE, line = -2)
  }
 
  
  if(!is.null(savedir)){dev.off()}
  #dev.off()
}#end function


#################################################################################
## Other Diagnostic Plots
################################################################################

#======================================================================
# Diagnostic Plots for ORIGINAL DATASET
#=====================================================================


#======================================================================
# Diagnostic Plots for DETRENDED DATASET
#=====================================================================


DT.diagnostics <- function(dat, model.T, model.TS, savedir){
  
  if(unique(dat$DTmethod) != "None"){
  #require(NADA)
  stnID <- unique(dat$StationID)
  param <- unique(dat$Parameter)
  p.units <- unique(dat$Units)
  
  #add data.col to DTDS.data
  dat$datacol <-dat[,grep(paste("^",unique(dat$Data.Method),"$", sep=""), names(dat), value=TRUE)]   
  
  #if savedirectory is specified save as pdf
  if(!is.null(savedir)){
    setwd(savedir)
    pdf(paste(stnID, "_", param, "_ModelDiagnoticPlots_DT.pdf"), width=11,height=8.5)
  }  
  
  
 
  #*******PAGE 1************************
  par(oma=c(1,2,1,1))
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  #layout.show()
  

  
  #-----------------------------------------
  # PLOT 1: TS orig data w/ DT FIT OVERLAYED
  #-----------------------------------------
   
      plot(dat$datacol~dat$dates.dec, cex=1.2, xlab="",pch=1, ylab=paste(param, "Conc."), 
         main = "Trend Model fit to Dataset")
      lines(dat$dates.dec, dat$datacol, type="l", col="black", lty=2)
      lines(dat$dates.dec, dat$DT.fit, col="blue")
   
  #-----------------------------------------
  # PLOT 2: DT residuals
  #-----------------------------------------
    plot(dat$DT~dat$dates.dec, xlab="",pch=1, ylab="Detrended Residuals", type="h", 
         main = "DT Residuals vs Time")
    abline(h=0, col=2, lty=2)
  
    #-----------------------------------------
    # PLOT 3: Resid vs Fitted
    # Check for constant variance and linearity
    #-----------------------------------------
    #plot(model.T$residuals~model.T$fitted, ylab="DT residuals", xlab="DT fitted")
    plot(dat$DT~dat$DT.fit, , ylab="Residuals", xlab="Fitted values")#, main="Residuals vs Fitted")
    mtext("Residuals vs Fitted", 3, 0.25)
    abline(h=0, col="red", lty="dashed")  
    #OR using plot.lm
    #plot(which=c(1), model.DS, sub.caption="")
  
  
    #-----------------------------------------
    # PLOT 4: Boxplots of residuals by Season
    #----------------------------------------- 
    
    boxplot(dat$DT ~ dat$Season, cex.axis=0.8,xlab="Season", ylab="DT Residuals", 
            main = "DT Residuals by Season")
    

  
  #-----------------
  # Add title to page
  #-----------------
 
  if(unique(dat$Data.Method)=="Data"){
    title(paste("DT RESIDUALS: DIAGNOSTIC PLOTS for ", param, "( ", p.units, ")", sep=""), outer = TRUE, line = 0)
  }else if (unique(dat$Data.Method)=="Data.log"){
    title(paste("DT RESIDUALS: DIAGNOSTIC PLOTS for Log(", param, "( ", p.units, ") )", sep=""), outer = TRUE, line = 0)
  }
  
  #*******PAGE 2************************
  par(oma=c(1,2,1,1))
  layout(matrix(c(1,2,0,0), 2, 2, byrow = TRUE))
  #layout.show()
  
  #-----------------------------------------
  #plot 1 : qqplot of residuals
  #-----------------------------------------
  
  #require(car) or qualityTools (differ qqPlot functions)
  #car::qqPlot(dat$DT, main="QQ Plot")
 
#     qqnorm(dat$DT  , ylab = paste("DT Residuals", sep=""),
#          main="Normal Q-Q Plot")
#     qqline(dat$DT, distribution=qnorm)
    car::qqPlot(dat$DT, ylab="Quantiles for DT Residuals", xlab="Normal Quantiles", 
                main="QQ Plot for DT Residuals", col.lines="blue", id.n=5, lwd=1, id.cex=0.7) 
    
  #-----------------------------------------
  #Add shapiro.wilks results
  #-----------------------------------------
  #require(gplots)
  textplot(capture.output(shapiro.test(dat$DT)),  mar=c(2,2,2,2)) 
  title(main="Shapiro-Wilk Normality Test", sub="")         
  mtext("Ho: Sample comes from a normal distribution ", 3, 0.25, cex=0.7)
  
  #OR using plot.lm
  #plot(which=c(2), model.DS, sub.caption="")

  #-----------------
  # Add title to page
  #-----------------
  if(unique(dat$Data.Method)=="Data"){
    title(paste("DT RESIDUALS: DIAGNOSTIC PLOTS - NORMALITY for ", param, "( ", p.units, ")", sep=""), outer = TRUE, line = 0)
  }else if (unique(dat$Data.Method)=="Data.log"){
    title(paste("DT RESIDUALS: DIAGNOSTIC PLOTS - NORMALITY for Log(", param, "( ", p.units, ") )", sep=""), outer = TRUE, line = 0)
  }
  
  #*******PAGE 3************************
  par(oma=c(1,2,1,1))
  layout(matrix(c(1,2,3,0), 2, 2, byrow = TRUE))
  #layout.show()
  
  #-----------------------------------------
  #plot 1 and 2: ACF AND PACF PLOTS
  #-----------------------------------------

  acf(dat$DT, na.action=na.pass, main="ACF Plot of DT data")
  pacf(dat$DT, na.action=na.pass, main="PACF Plot of DT data")
  
  #-----------------------------------------
  #Add sportmanteau (Box-Ljung) results
  #-----------------------------------------
  #require(gplots)  
  
  #from Practical Stats
  portman = function(x,lag1 = 15){   
    require(TSA)  #portman in TSA package
    #acf(x, lag.max= lag1,na.action = na.pass)
    acfx = acf(x,lag.max = lag1 , na.action = na.pass, plot=FALSE)
    #print(acfx)
    RAWDATA = arima(x,c(0,0,0))
    print(LB.test(RAWDATA,lag1))    
  }
  
  textplot(capture.output(portman(dat$DT, 15)), valign="top", halign="center", mar=c(2,2,2,2)) 
  title(main="Box-Ljung (Portmanteau) Test", sub="") 
  mtext("Ho: Data are random - no memory", 3, 0.25, cex=0.7)         
    
  #dwtest(Levels ~ Year, alternative="greater", data=huron)

  
  #-----------------
  # Add title to page
  #-----------------

  if(unique(dat$Data.Method)=="Data"){
    title(paste("DT RESIDUALS: DIAGNOSTIC PLOTS - SERIAL CORRELATION for ", param, "( ", p.units, ")", sep=""), outer = TRUE, line = 0)
  }else if (unique(dat$Data.Method)=="Data.log"){
    title(paste("DT RESIDUALS: DIAGNOSTIC PLOTS - SERIAL CORRELATION for Log(", param, "( ", p.units, ") )", sep=""), outer = TRUE, line = 0)
  }
  
  
  if(!is.null(savedir)){dev.off()}

  }else {cat("NO PDF CREATED - dataset was not detrended", "\n")}  
}

#======================================================================
# Diagnostic Plots for DTDS DATASET
#=====================================================================

DTDS.diagnostics <- function(dat, model.T, model.TS, savedir){
  
  if(unique(dat$DSmethod) != "None"){
  
  #require(NADA)
  stnID <- unique(dat$StationID)
  param <- unique(dat$Parameter)
  p.units <- unique(dat$Units)
  
  #add data.col to DTDS.data
  dat$datacol <-dat[,grep(paste("^",unique(dat$Data.Method),"$", sep=""), names(dat), value=TRUE)]   
  
  #if savedirectory is specified save as pdf
  if(!is.null(savedir)){
    setwd(savedir)
    pdf(paste(stnID, "_", param, "_ModelDiagnoticPlots_DTDS.pdf"), width=11,height=8.5)
  }  
  
  #*******PAGE 1************************

  
    par(oma=c(1,1,3,1))
    layout(matrix(c(1,2,2,3,4,5), 2, 3, byrow = TRUE))
    #layout.show()
  
  #-----------------------------------------
  # PLOT 1: TS DT data with seasonal model
  #-----------------------------------------
  
  plot(dat$DT~dat$dates.dec, cex=1.2, xlab="",pch=1, ylab=paste(param, "DT Residuals"),
       main = "Seasonal Model fit to DT Residuals")
  if(unique(dat$DSmethod) == "Fourier"){
    mtext(paste("Seasonal Model: ",unique(dat$harmonics), "Sin/Cos", sep=""),  3, 0.25, cex=0.7) 
    lines(dat$DT~dat$dates.dec, type="l", col="black", lty=2)
    lines(dat$DTDS.fit~dat$dates.dec, col="red")
  }else if (unique(dat$DSmethod) == "SeasMed"){
    mtext(paste("Seasonal Model: ",unique(dat$DSmethod), sep=""),  3, 0.25, cex=0.7) 
    lines(dat$DT~dat$dates.dec, type="l", col="black", lty=2)
    lines(dat$DTDS.fit~dat$dates.dec, col="red")
  } #else if (unique(dat$DSmethod) == "None") {
    #mtext(paste("Seasonal Model: ",unique(dat$DSmethod), sep=""),  3, 0.25, cex=0.7) 
  #}

  #lines(model.TS$fitted~dat$dates.dec, col="red")
  
  
  #-----------------------------------------
  # PLOT 2: Summary of DTDS model  
  #-----------------------------------------
  #require(gplots)
  if(unique(dat$DSmethod)=="Fourier"){
    textplot(capture.output(summary(model.TS)), valign="top", halign="left", mar=c(1,1,2,1))
    title(paste("Model Summary: ",unique(dat$harmonics), "harmonics Sin/Cos Regression Model", sep="")) 
  }else if (unique(dat$DSmethod)=="SeasMed") {
    textplot(model.TS, valign="top", halign="left", mar=c(1,1,2,1), show.rownames=FALSE, cmar=2, cex=1.6)  
    title("Model Summary: Subtract Seasonal Medians") 
  }#else if (unique(dat$DSmethod)=="None") {
   # plot.new()
  #  title("Model Summary: NO Seasonal Model") 
  #}

  #-----------------------------------------
  # PLOT 3: DT residuals
  #-----------------------------------------
  
  plot(dat$DTDS~dat$dates.dec, xlab="",pch=1, ylab="DTDS Residuals", type="h", 
       main = "DTDS Residuals vs Time")
  abline(h=0, col=2, lty=2)
  
  #-----------------------------------------
  # PLOT 4: Resid vs Fitted
  # Check for constant variance and linearity
  #-----------------------------------------
  #plot(model.T$residuals~model.T$fitted, ylab="DT residuals", xlab="DT fitted")
  #plot(dat$DT~dat$DT.fit, , ylab="Residuals", xlab="Fitted values")#, main="Residuals vs Fitted")
  #mtext("Residuals vs Fitted", 3, 0.25)
  #abline(h=0, col="red", lty="dashed")  
  #OR using plot.lm
  if(unique(dat$DSmethod)=="Fourier"){
    plot(which=c(1), model.TS, sub.caption="")
  }else if(unique(dat$DSmethod)=="SeasMed") {
    plot(dat$DTDS~dat$DTDS.fit, , ylab="Residuals", xlab="Fitted values")#, main="Residuals vs Fitted")
    mtext("Residuals vs Fitted", 3, 0.25)
    abline(h=0, col="red", lty="dashed")
  }
  
  #-----------------------------------------
  # PLOT 4: Boxplots of residuals by Season
  #----------------------------------------- 
  
  boxplot(dat$DTDS ~ dat$Season, cex.axis=0.8,xlab="Season", ylab="DTDS Residuals", 
          main = "DTDS Residuals by Season")
  
 
  
  #-----------------
  # Add title to page
  #-----------------
  
  if(unique(dat$Data.Method)=="Data"){
    title(paste("DTDS RESIDUALS: DIAGNOSTIC PLOTS for ", param, "( ", p.units, ")", sep=""), outer = TRUE, line = 0)
  }else if (unique(dat$Data.Method)=="Data.log"){
    title(paste("DTDS RESIDUALS: DIAGNOSTIC PLOTS for Log(", param, "( ", p.units, ") )", sep=""), outer = TRUE, line = 0)
  }
  
  
  
  #*******PAGE 2************************
  par(oma=c(1,1,3,1))
  layout(matrix(c(1,2,3,0,0,0), 2, 3, byrow = TRUE))
  #layout.show()

  #-----------------------------------------
  #plot 1 and 2: qqplot of residuals
  #-----------------------------------------
  car::qqPlot(dat$DTDS, ylab="Quantiles for DTDS Residuals", xlab="Normal Quantiles", 
              main="QQ Plot for DTDS Residuals", col.lines="blue", id.n=5, lwd=1, id.cex=0.7) 
  
  if (unique(dat$DSmethod) == "Fourier"){
    plot(which=c(2), model.DS, sub="", main ="QQ Plot for DTDS Residuals")
  } else {plot.new()}
  
  #-----------------------------------------
  #Add shapiro.wilks results
  #-----------------------------------------
  #require(gplots)
  textplot(capture.output(shapiro.test(dat$DTDS)), valign="top", mar=c(2,2,3,2)) 
  title(main="Shapiro-Wilk Normality Test", sub="") 
  mtext("Ho: Sample comes from a normal distribution ", 3, 0.25, cex=0.7)
  
  #-----------------
  # Add title to page
  #-----------------
 
  if(unique(dat$Data.Method)=="Data"){
    title(paste("DTDS RESIDUALS: DIAGNOSTIC PLOTS - NORMALITY for ", param, "( ", p.units, ")", sep=""), outer = TRUE, line = 0)
  }else if (unique(dat$Data.Method)=="Data.log"){
    title(paste("DTDS RESIDUALS: DIAGNOSTIC PLOTS - NORMALITY for Log(", param, "( ", p.units, ") )", sep=""), outer = TRUE, line = 0)
  }
  
  #*******PAGE 3************************
  par(oma=c(1,1,3,1))
  layout(matrix(c(1,2,3,0), 2, 2, byrow = TRUE))
  #layout.show()
  
  #-----------------------------------------
  #plot 1 and 2: ACF AND PACF PLOTS
  #-----------------------------------------
  
  acf(dat$DTDS, na.action=na.pass, main="ACF Plot of DT data")
  pacf(dat$DTDS, na.action=na.pass, main="PACF Plot of DT data")
  
  #-----------------------------------------
  #Adds portmanteau (Box-Ljung) results
  #-----------------------------------------
  #require(gplots)  
  
  #from Practical Stats
  portman = function(x,lag1 = 15){   
    require(TSA)  #portman in TSA package
    #acf(x, lag.max= lag1,na.action = na.pass)
    acfx = acf(x,lag.max = lag1 , na.action = na.pass, plot=FALSE)
    #print(acfx)
    RAWDATA = arima(x,c(0,0,0))
    print(LB.test(RAWDATA,lag1))    
  }
  
  textplot(capture.output(portman(dat$DTDS, 15)), valign="top", mar=c(2,2,3,2)) 
  title(main="Box-Ljung (Portmanteau) Test", sub="") 
  mtext("Ho: Data are random - no memory", 3, 0.25, cex=0.7)         
  
  #dwtest(Levels ~ Year, alternative="greater", data=huron)
  
  
  #-----------------
  # Add title to page
  #-----------------
  
  if(unique(dat$Data.Method)=="Data"){
    title(paste("DTDS RESIDUALS: DIAGNOSTIC PLOTS - SERIAL CORRELATION for ", param, "( ", p.units, ")", sep=""), outer = TRUE, line = 0)
  }else if (unique(dat$Data.Method)=="Data.log"){
    title(paste("DTDS RESIDUALS: DIAGNOSTIC PLOTS - SERIAL CORRELATION for Log(", param, "( ", p.units, ") )", sep=""), outer = TRUE, line = 0)
  }
  #dwtest(DT ~ dates.dec, alternative="greater", data=dat)
  
 
  #*******PAGE 4************************
  par(oma=c(1,1,3,1))
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  #layout.show()

  #-----------------------------------------
  #Fit an ARMA(p,q) model to the series (test different models)
  #-----------------------------------------
  
    bulkfit.ARMA(dat$DTDS)
    df.AIC.m <- as.matrix(df.AIC)
  
    #----------------
    # print out all AIC values
    #-------------------
    textplot(df.AIC.m, valign="top", mar=c(2,2,3,2), cex=0.8, show.rownames=FALSE, rmar = 0.7, cmar=2) 
    title("ARMA Model Comparison-  AIC")
  
    #PRINT OUT BEST MODEL SUMMARY
    textplot(capture.output(out.cat(minaic,mi,mj,A)), valign="top", mar=c(2,2,3,2)) 
    title("Best ARMA Model Fit-  AIC")
  
    #PRINT OUT ACF and PACF OF THE BEST MODEL
    acf(A$residuals, na.action=na.pass, main="ACF Plot of BEST FIT ARMA MODEL Residuals")
    pacf(A$residuals, na.action=na.pass, main="PACF Plot of BEST FIT ARMA MODEL Residuals")
  
  #-----------------
  # Add title to page
  #-----------------
  if(unique(dat$DSmethod) != "None"){
    if(unique(dat$Data.Method)=="Data"){
      title(paste("ARMA MODEL FIT RESIDUALS: DIAGNOSTIC PLOTS - SERIAL CORRELATION for ", param, "( ", p.units, ")", sep=""), outer = TRUE, line = 0)
    }else if (unique(dat$Data.Method)=="Data.log"){
      title(paste("ARMA MODEL FIT RESIDUALS: DIAGNOSTIC PLOTS - SERIAL CORRELATION for Log(", param, "( ", p.units, ") )", sep=""), outer = TRUE, line = 0)
    }
  }else {
    if(unique(dat$Data.Method)=="Data"){
      title(paste("ARMA MODEL FIT RESIDUALS: DIAGNOSTIC PLOTS - SERIAL CORRELATION for ", param, "( ", p.units, ")", sep=""), outer = TRUE, line = 0, 
            sub="Note: DTDS residuals represent DT residuals since no seasonal was fit to the DT residuals")
    }else if (unique(dat$Data.Method)=="Data.log"){
      title(paste("ARMA MODEL FIT RESIDUALS: DIAGNOSTIC PLOTS - SERIAL CORRELATION for Log(", param, "( ", p.units, ") )", sep=""), outer = TRUE, line = 0, 
            sub="Note: DTDS residuals represent DT residuals since no seasonal was fit to the DT residuals")
    }
  }
  
  if(!is.null(savedir)){dev.off()}
  
}else{cat("No PDF Created: No Seasonal Model fit to DT residuals")}

}  
