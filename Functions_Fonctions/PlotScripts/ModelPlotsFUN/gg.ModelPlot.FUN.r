################################################################
# ggplot: Individual DT and DS model plots
###############################################################

#     model.DSx = model for DS on original data (DS1=fourier, DS2=seasonal medians)
#     DSx.formula = formula for DS on original data
#     model.DTx.DSx = model for DS on DT data (DS then DT)
#     DTx.DSx.formula = formula for DS on DT data (DS then DT)
#     model.DSx.DTx = model for DT on DS data (DT then DS)
#     DS1.DT1.data = dataframe with original, DS, DT, DSDT, DTDS data

#require(ggplot2)
#require(gridExtra)

#--------------------------------------------
# 
#--------------------------------------------

gg.DTDS.models <- function(dat, model.T, savedir=NULL){
  
  #require(ggplot2)
  #require(gridExtra)
  #source(seaken)
 
if(unique(dat$DSmethod !="None")  || unique(dat$DTmethod)!="None") {
  if (unique(dat$DSmethod)=="Fourier"){ k=unique(DTDS.data$harmonics) } 
  #k=num.harmonics
  
  stnID <- unique(dat$StationID)
  param <- unique(dat$Parameter)
  p.units <- unique(dat$Units)
  
  #add data.col to DTDS.data
  dat$datacol <-dat[,grep(paste("^",unique(dat$Data.Method),"$", sep=""), names(dat), value=TRUE)]   
  
  #SET THE THEME TO BW GLOBALLY: (all plots will be drawn with this)
  theme_set(theme_bw())
  
  #-----------------------------------------------
  #PLOT 1:  TS data w/ loess and Sen slope overlay
  # if DT = "None" omit the Loess but still show Sen slope
  #-----------------------------------------------
  
  #Calculate SK
  SK.orig <- seaken_noout(x=dat$dates.dec, y=dat$datacol, group=dat$Season)
  p.sig3 <- S.NS(SK.orig)
  
  #Base TS w/ data points and lines
  p <- ggplot(data=dat, aes(x=dates.dec, y=datacol)) + geom_point() +
    geom_line(linetype="dashed") 
  
  #add labels
  if(unique(DTDS.data$Data.Method) =="Data"){
    p1 <- p +
      ylab(paste(param, "Conc. (", p.units, ")")) + xlab("") +
      theme(axis.title.y=element_text(size=10))
  }else if (unique(DTDS.data$Data.Method) =="Data.log"){
    p1 <- p +  
      ylab(paste("log", param, "Conc.(", p.units, ")")) + xlab("") +
      theme(axis.title.y=element_text(size=10))
  }
  
  #add Lowess curve
  if(unique(dat$DTmethod)=="None"){
    p2 <- p1
  }else if (unique(dat$DTmethod)=="Lowess") {
    p2 <- p1 + geom_line(aes(y=DT.fit, color="a"))
  }
  
  #add SK slope
  p3 <- p2 + geom_abline(data=SK.orig, aes(intercept=INT, slope=SLOPE, color="b"))
  
 
  #add legend
  if(unique(dat$DTmethod)=="None"){
    p4 <- p3 +
      scale_colour_manual(name = "",breaks=c("b"), values = c("#FF0000"), 
                          labels=paste("Sen Slope=",round(SK.orig$SLOPE,4), " (", p.sig3, ")")) +
      theme(legend.justification=c(1,0.8), legend.position=c(1,1)) 
    
  }else if (unique(dat$DTmethod)=="Lowess") { 
     p4 <- p3 +
     scale_colour_manual(name = "",breaks=c("a","b"), values = c("blue", "#FF0000"), 
                        labels=c(paste("Lowess= ", model.T$pars$span), 
                                 paste("Sen Slope=",round(SK.orig$SLOPE,4), " (", p.sig3, ")"))) +
    theme(legend.justification=c(1,0.8), legend.position=c(1,1) ) 
  #theme(axis.text.x = element_text(angle = 90)) +   #theme(axis.text.x=element_blank())
  }
  
 
  #DIFFERENT OVERALL PLOTTING OPTIONS
  
    #Single Plot 
    p5 <- p4 + ggtitle(paste("Original Time Series showing Lowess and Sen's slope for", param)) + 
          scale_x_continuous(breaks=seq(min(dat$Year), max(dat$Year), by=1)) + 
          theme(axis.text.x = element_text(angle=90))
  
   #Simpler Plot w/ only TS --> use p1
  
   #No title --> p4

  
  
  #-----------------------------------------------
  #PLOT 2: The difference bewteen original Data 
  #              and smoothed values (residuals=DT data) 
  #-----------------------------------------------
  
  #Calculste SK on DT data
  SK.DT <- seaken_noout(x=dat$dates.dec, y=dat$DT, group=dat$Season)
  p.sig2 <- S.NS(SK.DT)
  
  # TS w/ data points and lines for DT data
  q <- ggplot(data=dat, aes(x=dates.dec, y=DT)) + geom_point()  +
    geom_line(linetype="dashed")
  
  #
  if(unique(dat$DTmethod)=="None"){
    q1 <- q + 
      geom_smooth(method=loess, se=TRUE, aes(color="a", fill="a"), alpha=0.1, level=0.95, 
                  na.rm=TRUE,  span=0.65, degree=2, show_guide=TRUE)
  }else if(unique(dat$DTmethod)=="Lowess"){
    q1 <- q + 
    geom_smooth(method=loess, se=TRUE, aes(color="a", fill="a"), alpha=0.1, level=0.95, 
                na.rm=TRUE,  span=model.T$pars$span, family=model.T$pars$family, 
                degree=model.T$pars$degree, show_guide=TRUE)
    
  }
  
  #add SK slope
  q2 <- q1 + geom_abline(data=SK.DT, aes(intercept=INT, slope=SLOPE, color="b", fill="b"))
  
  #add legend
  if(unique(dat$DTmethod)=="None"){
    q3 <- q2 +
    scale_colour_manual(name = "",breaks=c("a","b"), values = c("blue", "#FF0000"), labels=c(paste("Lowess= ", 0.65), paste("Sen Slope=",round(SK.DT$SLOPE,4), " (", p.sig2, ")"))) +
    scale_fill_manual(name = "",breaks=c("a", "b"), values = c("blue", "#FF0000"), labels=c(paste("Lowess= ",0.65), paste("Sen Slope=",round(SK.DT$SLOPE,4), " (", p.sig2, ")"))) +
    theme(legend.justification=c(1,0.8), legend.position=c(1,1)) 
  }else if(unique(dat$DTmethod)=="Lowess"){
    q3 <- q2 +
      scale_colour_manual(name = "",breaks=c("a","b"), values = c("blue", "#FF0000"), labels=c(paste("Lowess= ", model.T$pars$span), paste("Sen Slope=",round(SK.DT$SLOPE,4), " (", p.sig2, ")"))) +
      scale_fill_manual(name = "",breaks=c("a", "b"), values = c("blue", "#FF0000"), labels=c(paste("Lowess= ",model.T$pars$span), paste("Sen Slope=",round(SK.DT$SLOPE,4), " (", p.sig2, ")"))) +
      theme(legend.justification=c(1,0.8), legend.position=c(1,1)) 
  }
  
  
  
  #DIFFERENT OVERALL PLOTTING OPTIONS
  
    #Single Plot 
    q5 <- q3 + ylab("Detrended Data (Residuals)") + xlab("") +
      ggtitle(paste("Detrended Time Series showing Lowess and Sen's slope for", param)) + 
      scale_x_continuous(breaks=seq(min(dat$Year), max(dat$Year), by=1)) + 
      theme(axis.text.x = element_text(angle=90)) 
    
    #No title --> 
    q4 <- q3 + ylab("DT Residuals)") + xlab("") +
      scale_x_continuous(breaks=seq(min(dat$Year), max(dat$Year), by=1)) + 
      theme(axis.text.x = element_text(angle=90)) 
  
  #No title, no x-axis  
  q6 <- q3 + ylab("DT Residuals)") + xlab("") 
    
    
    
    #LOWESS ONLY WITH Concentration POINTS
    if(unique(dat$DTmethod)=="Lowess"){
      qq <- p1 + geom_line(data=dat, aes(y=DT.fit, color="aa"), lwd=2) +
        scale_colour_manual(name = "",breaks=c("aa"), values = c("black"), labels=c(paste("Lowess: ",model.T$pars$span))) +
        theme(legend.justification=c(1,0.8), legend.position=c(1,1)) +
        ylab("TREND w/ data")
    }else {
      qq <- p1 + ylab("TREND w/ data") + xlab("") +
        geom_text(y= max(dat$datacol), x =max(dat$dates.dec),label="No Trend Removed", hjust=1) 
    }
      
      #LOWESS ONLY with no data points (Trend only)
    if(unique(dat$DTmethod)=="Lowess"){
      qq1 <- ggplot(data=dat, aes(x=dates.dec, y=datacol)) 
      qq2 <- qq1 + geom_line(aes(y=DT.fit, color="aa"), lwd=2) +
            scale_colour_manual(name = "",breaks=c("aa"), values = c("black"), labels=c(paste("Lowess: ",model.T$pars$span))) +
            theme(legend.justification=c(1,0.8), legend.position=c(1,1)) +
            ylab("TREND")
    }else {
      qq1 <- ggplot(data=dat, aes(x=dates.dec, y=datacol)) 
      qq2 <-qq1 + geom_blank() + ylab("TREND") + xlab("") +
        geom_text(y= max(dat$datacol), x =max(dat$dates.dec),label="No Trend Removed", hjust=1) 
    }
    

  
  #-----------------------------------------------
  #PLOT 3: Detrended data (residuals) w/ seasonal model overlaid
  #-----------------------------------------------
   
  #baseplot w/ lines
  r0 <- ggplot(data=dat, aes(x=dates.dec, y=DT)) 
  
  #baseplot w/ DT data
  r <- r0 + geom_point(color="grey40")  +
    geom_line(linetype="dashed", color="grey40")
 
  #Add sin/cos fit curve
  if(unique(dat$DSmethod)=="Fourier"){
    r1 <- r + geom_point(data=dat, aes(y=DTDS.fit, color="a")) + 
    geom_line(data=dat, aes(y=DTDS.fit, color="a"), lwd=1) #+
    #geom_text(y= max(dat$DT), x =max(dat$dates.dec),label=paste(k," harmonics (sin/cos) model"), hjust=1, color="#FF0000") 
  }else {r1 <-r}
  
  #add Seasonal MEdian fit
    #add seas med column for the DT datacolumn
    SM.calc<- by(dat$DT, dat$Season, median, na.rm=T)
    seas.med <- data.frame(cbind(SM2 = as.numeric(SM.calc), Season = dimnames(SM.calc)[[1]]), stringsAsFactors=FALSE)
    seas.med$SM2 <- as.numeric(seas.med$SM2)  
  dat$SM.calc <- seas.med$SM2[match(dat$Season, seas.med$Season)] 
  
  r2 <- r1 + geom_line(data=dat, aes(y=SM.calc, color="b"))
  
  #add line at zero
  r3 <- r2+ geom_hline(aes(intercept=0), color="red", linetype="dashed")
  
  #add legend
  if(unique(dat$DSmethod)=="Fourier"){
    r4 <- r3 + 
      scale_colour_manual(name = "",breaks=c("a", "b"), values = c("red", "blue"), 
                          labels=c(paste(k, " harmonics (sin/cos) model", sep=""), "Seasonal Medians")) +
      theme(legend.justification=c(1,0.8), legend.position=c(1,1))    
  }else { 
    r4 <- r3 + 
      scale_colour_manual(name = "",breaks=c("b"), values = c("blue"), 
                          labels=c("Seasonal Medians")) +
      theme(legend.justification=c(1,0.8), legend.position=c(1,1)) 
  }
  
  #DIFFERENT OVERALL PLOTTING OPTIONS
  
    #Single Plot 
    r5 <- r4 + ylab("Detrended Data (Residuals)") + xlab("") +
      ggtitle(paste("Seasonal Model(s) fit to DT residuals for", param)) + 
      scale_x_continuous(breaks=seq(min(dat$Year), max(dat$Year), by=1)) + 
      theme(axis.text.x = element_text(angle=90)) 
    
    #No title --> 
    r6 <- r4 + ylab("DT Residuals") + xlab("") +
      scale_x_continuous(breaks=seq(min(dat$Year), max(dat$Year), by=1)) + 
      theme(axis.text.x = element_text(angle=90)) 
  
    #Simpler y, no x axis--> 
    r7 <- r4 + ylab("DT Residuals") + xlab("") 
    
    #Simpler w/no data
    if(unique(dat$DSmethod) != "None"){
  
      if(unique(dat$DSmethod)=="Fourier"){
        rr1 <- r0 + geom_line(aes(y=DTDS.data$DTDS.fit, color="bb"), lwd=0.7)
        rr2 <- rr1 + scale_colour_manual(name = "",breaks=c("bb"), values = c("black"), labels=c(paste(k, "harmonics (sin/cos) model"))) +
          theme(legend.justification=c(1,0.8), legend.position=c(1,1))
      }else if(unique(dat$DSmethod)=="SeasMed"){
        rr1 <- r0 + geom_line(aes(y=DTDS.data$DTDS.fit, color="bb"), lwd=0.7)
        rr2 <- rr1 + scale_colour_manual(name = "",breaks=c("bb"), values = c("black"), labels=c("Seasonal Medians")) +
            theme(legend.justification=c(1,0.8), legend.position=c(1,1))
      }else {
        
        rr2 <- r0 + geom_blank() +
          geom_text(y=max(dat$DT), x=max(dat$dates.dec),label="No Seasonality Removed", hjust=1) 
      }
      rr3 <- rr2 + geom_hline(aes(intercept=0), color="red", linetype="dashed") 
      rr4 <- rr3 + ylab("SEASONAL") + xlab("") 
    }
    
    
  #-----------------------------------------------
  #Fourth PANEL: DTDS Resdiuals w/ Hline=0
  #-----------------------------------------------
  
  #base plot w/ DTDS points   
  s <- ggplot(data=dat, aes(x=dates.dec, y=DTDS)) + geom_point(color="grey40") 
  
  s2 <- s+ geom_hline(aes(intercept=0), color="red", linetype="dashed") +
    geom_linerange(ymin=0, ymax=dat$DTDS, linetype="dotted")
  
  
  #DIFFERENT OVERALL PLOTTING OPTIONS
  
  #Single Plot 
  s3 <- s2 + ylab("Detrended & Deseasonalized Data (Residuals)") + xlab("") +
    ggtitle(paste("DTDS Residuals for ", param)) + 
    scale_x_continuous(breaks=seq(min(dat$Year), max(dat$Year), by=1)) + 
    theme(axis.text.x = element_text(angle=90)) 
  
  #Single Plot no title
  s4 <- s2 + ylab("Detrended & Deseasonalized Data (Residuals)") + xlab("")
    scale_x_continuous(breaks=seq(min(dat$Year), max(dat$Year), by=1)) + 
    theme(axis.text.x = element_text(angle=90)) 
  
  #Simpler labels
  s5 <- s2 + ylab("REMAINDER (DTDS)") + xlab("") 

   

  #-----------------------------------------------
  #Combine all four plots into one output?plot
  #-----------------------------------------------
  
  #if savedirectory is specified save as pdf
  if(!is.null(savedir)){
    setwd(savedir)
    pdf(paste(stnID, "_", param, "_DSDT_Model_plots.pdf"), width=11,height=8.5)
  }  
  

  #OVERALL PLOT 1: plots of TS w/ lowess, DT data w/ lowess, sin/cos fitted, residuals
  p7 <- p4 + theme(plot.margin=unit(c(1,1,-0.2,1), "cm"), axis.text.x=element_blank()) 
  q7 <- q6 +  theme(plot.margin=unit(c(-0.4,1,-0.2,1), "cm"), axis.text.x=element_blank()) 
  r8 <- r7 +  theme(plot.margin=unit(c(-0.5,1,-0.2,1), "cm"), axis.text.x=element_blank()) 
  s6 <- s5 +  theme(plot.margin=unit(c(-0.5,1,1,1), "cm"), axis.text.x=element_blank()) 
  
  
  grid.arrange(p7, q7, r8, s6, ncol=1, 
               main=paste("DECOMPOSITION PLOT: DT via ", unique(dat$DTmethod), " then DS via ",  unique(dat$DSmethod), sep="")) 
  #grid.arrange(p4, q5, r6, s4)
  
  #OVERALL PLOT 2: plots of TS w/ lowess, DT data w/ lowess, sin/cos fitted, residuals (NO DATA)
  pa1 <- p1 + theme(plot.margin=unit(c(1,1,-0.2,1), "cm"), axis.text.x=element_blank()) 
  qqa2 <- qq2 +  theme(plot.margin=unit(c(-0.2,1,-0.2,1), "cm"), axis.text.x=element_blank())
  if(unique(dat$DSmethod) != "None") {
    rra4 <- rr4 +  theme(plot.margin=unit(c(-0.4,1,-0.2,1), "cm"))
  }else if(unique(dat$DSmethod)  == "None") {
    rra4 <- rr4 + theme(plot.margin=unit(c(-0.40,1,-0.2,1), "cm"), axis.text.x=element_blank(), axis.text.y=element_text(color="white"))
  }
  sa5 <- s5 +  theme(plot.margin=unit(c(-0.5,1,1,1), "cm"))
  
  
  grid.arrange(pa1, qqa2,   rra4, sa5 , ncol=1, 
               main=paste("DECOMPOSITION PLOT: DT via ", unique(dat$DTmethod), " then DS via ",  unique(dat$DSmethod), sep="")) 
  #grid.arrange(p4, q5, r6, s4)
  
  
  #OVERALL PLOT 3:plot of TS w/ lowess overlay + DT data
  grid.arrange(p5, q5)
  
  #print individual plots
  print(p5)
  print(q5)
  print(r5)
  print(s3)
  
  if(!is.null(savedir)){dev.off()}
}else cat("NO PDF CREATED - Dataset was not detrended or deseasonalize", "\n")
  
}#end function
