################################################################################
#  GGPLOTS for individual models
###############################################################################

#     model.DSx = model for DS on original data (DS1=fourier, DS2=seasonal medians)
#     DSx.formula = formula for DS on original data
#     model.DTx.DSx = model for DS on DT data (DS then DT)
#     DTx.DSx.formula = formula for DS on DT data (DS then DT)
#     model.DSx.DTx = model for DT on DS data (DT then DS)
#     DS1.DT1.data = dataframe with original, DS, DT, DSDT, DTDS data

#---------------------------------------------------------
# FUNCTION FOR DECOMPOSITION PLOT 1  showing:
# - TOP: TS plot
# - 2nd: TREND ONLY
# - 3rd: SEASONAL ONLY
# - BOTTTOM: REMAINDER (DTDS)
#----------------------------------------------------------

#model.S <- model.DS1
#DSDT.data <- DS1.DT1.data
#model.D <- model.DT1
#model.TS <- model.DT1.DS1

gg.Decomp.DTDS <- function(dat,  savedir=NULL){
  
  k= k=unique(DTDS.data$harmonics)
  #k=num.harmonics
  
  stnID <- unique(dat$StationID)
  param <- unique(dat$Parameter)
  p.units <- unique(dat$Units)
  
  #add data.col to DTDS.data
  dat$datacol <-dat[,grep(paste("^",unique(dat$Data.Method),"$", sep=""), names(dat), value=TRUE)]   
  
  #SET THE THEME TO BW GLOBALLY: (all plots will be drawn with this)
  theme_set(theme_bw())
  
  #-----------------------------------------------
  #PLOTS 1: TS data w/ loess and Sen slope overlay
  #-----------------------------------------------
  
  p <- ggplot(data=dat, aes(x=dates.dec, y=datacol)) + geom_point() +
    geom_line(linetype="dashed")
  
  if(unique(DTDS.data$Data.Method) =="Data"){
    p1 <- p + ylab(paste(param, "Conc. (", p.units, ")")) + xlab("")
  } else if (unique(DTDS.data$Data.Method) =="Data.log"){
    p1 <- p + ylab(paste("Log", param, "Concentation (", p.units, ")")) + xlab("")
  }
  
  
  #-----------------------------------------------
  #SECOND PANEL: TREND ONLY (i.e. fitted values) 
  #-----------------------------------------------
  
  q <- ggplot(data=dat, aes(x=dates.dec, y=datacol)) 
  
 #q1 <- q + geom_line(aes(y=model.T$fitted))
  q1 <- q + geom_line(aes(y=DT.fit))
  
  q2 <- q1 + ylab("TREND") + xlab("") +
    geom_text(y= max(dat$DT.fit), x =max(dat$dates.dec),label=paste(unique(DTDS.data$loess.span)," lowess"), hjust=1) 
  
  #option w/datapts
    q3 <- q1 + geom_point(color="grey") + ylab("TREND w/ Data") +
    geom_text(y= max(dat$datacol), x =max(dat$dates.dec),label=paste(unique(DTDS.data$loess.span)," lowess"), hjust=1) 
  
 
  
  #-----------------------------------------------
  #THRID PANEL: SEASONAL MODEL (fit to DT)
  #-----------------------------------------------
  
  r <- ggplot(data=dat, aes(x=dates.dec, y=DT)) 
  
  r1 <- r + geom_line(aes(y=DTDS.data$DTDS.fit), lwd=0.7)
  
  #r2 <- r1 + geom_line(aes(y=DTDS.data$SM), colour="red")
  
  r2 <- r1+ geom_hline(aes(intercept=0), color="red", linetype="dashed")
  
  r3 <- r2 + ylab("SEASONAL") + xlab("") + 
    geom_text(y= max(DTDS.data$DTDS.fit), x = max(dat$dates.dec),label=paste(k," harmonics (sin/cos) model"), hjust=1)
 
  #option w/datapts

    r4 <- r2 + geom_point(color="grey") 
    r5 <- r4 + geom_line(aes(y=DTDS.data$DTDS.fit), lwd=1)
    r6 <- r5 + ylab("SEASONAL w/ DT") + xlab("") +  
          geom_text(y= max(dat$DT), x = max(dat$dates.dec),label=paste(k," harmonics (sin/cos) model"), hjust=1)

    
  #-----------------------------------------------
  #Fourth PANEL: DTDS Resdiuals w/ Hline=0
  #-----------------------------------------------
  
  s <- ggplot(data=dat, aes(x=dates.dec, y=DTDS)) + geom_point()  
  
  s2 <- s+ geom_hline(aes(intercept=0), color="red", linetype="dashed") +
    geom_linerange(ymin=0, ymax=dat$DTDS, linetype="dotted")
  
  s3 <- s2 + ylab("REAMINDER (DTDS)") + xlab("") 
  
  #-----------------------------------------------
  #Combine all four plots into one output?plot
  #-----------------------------------------------
  
#if savedirectory is specified save as pdf
if(!is.null(savedir)){
  setwd(savedir)
  pdf(paste(stnID, "_", param, "_DDecompositionPlot_DT1DS1.pdf"), width=11,height=8.5)
}  

  
  grid.arrange(p1, q2, r3, s3, ncol=1, main="DECOMPOSITION PLOT: DT via LOWESS, then DS via FOURIER")
  grid.arrange(p1, q2, r3, s3, main="DECOMPOSITION PLOT: DT via LOWESS, then DS via FOURIER")
  
  grid.arrange(p1, q3, r6, s3, ncol=1, main="DECOMPOSITION PLOT: DT via LOWESS, then DS via FOURIER")
  grid.arrange(p1, q3, r6, s3, main="DECOMPOSITION PLOT: DT via LOWESS, then DS via FOURIER")
  

  if(!is.null(savedir)){dev.off()}
  
}#end function
