#------------------------------------------------------------------------
# Function for MONTHLY BOXPLOTS IN GGPLOT.  Plot includes:
#  - Monthly Boxplots
#  - 
#------------------------------------------------------------------------

Box.GG <- function(df, data.col, group, add.jitter="NO"){
  
  cat("Running Boxplots (Box.GG) for ",  unique(df$Parameter),"\n")   
  
  G <- df
  
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  #ADD group COLUMN which is identical to the col dentified in group in function def 
  G$group <- G[,grep(paste("^",group,"$", sep=""), names(G), value=TRUE)]
  G$MED <- median(G$datacol, na.rm=TRUE)
  
  #calculate max MDL
  if(nrow(subset(G, cen==TRUE)) != 0){
    max.MDL <- max(unique(subset(G, cen==TRUE)$obs))
    
  } else {max.MDL <- as.numeric(NA)}
  
  G$maxMDL <- max.MDL
   
  #Function to place text at y=0
  give.n <- function(x){
    return(c(y = 0, label = length(x)))
  }
  #return(C(y=mean(x), label=length(x)))   places text at mean. 
  
  p <- ggplot(data=G, aes(x=group, y=datacol))+
    geom_boxplot(fill="grey", outlier.size=3, outlier.shape=8)  + 
    geom_hline(aes(yintercept=maxMDL, colour="c", linetype="c"),   show_guide=TRUE) +
    geom_hline(aes(yintercept=MED, colour="b", linetype="b"),   show_guide=TRUE) 
  
  
  if (add.jitter=="YES"){
    p1 <- p + geom_jitter(color="blue", size=2, position=position_jitter(height = 0))
  } else {p1 <- p}
  
    
  #add axes,labels and theme  
  p2 <- p1 +  xlab(grep(paste("^",group,"$", sep=""), names(G), value=TRUE)) + 
    ggtitle(paste("Boxplots by Month for", unique(G$Parameter)," at ", unique(G$StationID))) +
    stat_summary(fun.data = give.n, geom = "text", size=4) +
    scale_linetype_manual(name="", breaks=c("b", "c"), values=c("solid", "dashed"), labels=c("Record Median", "Max MDL")) +
    scale_colour_manual(name="", breaks=c("b", "c"), values=c("red", "red"),labels=c("Record Median", "Max MDL")) +
    theme_bw() +
    theme(legend.position = "top") 
  
  if(data.col=="Data.log"){
    p3 <- p2 +  ylab(paste("Log( ",unique(G$Parameter), "(", unique(G$Units) , ") )"))
  }else {
    p3 <- p2 + ylab(paste(unique(G$Parameter), "( ", unique(G$Units) , " )"))
  }
  
  #overall boxplot (all data)
#   g <- ggplot(data=G, aes(x = Parameter, y=datacol)) +
#     geom_boxplot(fill="grey", outlier.size=3, outlier.shape=8) +
#     theme_bw() +
#     theme(axis.title=element_blank(), axis.text=element_blank())
  
  suppressWarnings(print(p3))
 
  
  #graphics.off()
  
} #end function




#------------------------------------------------------------------------
# REG BOXPLOTS with base graphics
#------------------------------------------------------------------------

Reg.Box <- function(df, data.col, group){
  
  #require(PerformanceAnalytics)

  cat("Running Boxplots (Reg.box) for ",  unique(df$Parameter),"\n")   
  
  G <- df
  
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  #ADD group COLUMN which is identical to the col dentified in group in function def 
  G$group <- G[,grep(paste("^",group,"$", sep=""), names(G), value=TRUE)]
  
  #G$MED <- median(G$datacol, na.rm=TRUE)

  #y axis limits (to ensure 2 boxplots have the same y-axis)
  yax.min <- min(G$datacol)
  yax.max <-max(G$datacol)
  yax.range <- c(yax.min,yax.max)
  
  #setup plot parameters using split screen
  split.screen(c(1,1))
  par(oma=c(1,1,5,1))
  
  #overall PAGE Title
    mtext(paste("Boxplot for ", unique(G$Parameter), "at ", unique(G$StationID)), side =3, line=1, outer=TRUE, cex=1.4)
  
  #fig.mat <- c(0,0.27,0.7,0.7,0.27,0.7,1,1,0,0,0.4,0,1,1,1,0.6)
  #fig.mat <- matrix(fig.mat,nrow=4)
  fig.mat <- c(0,0.27, 0.27, 1, 0,0,1,1)
  fig.mat <- matrix(fig.mat,nrow=2)
  split.screen(fig.mat, screen=1)
  
  #PLOT 1: CENSORED BOXPLOTS - ALL DATA
  x <- with(G, censummary(datacol, cen))       #censored summary information
  
  N <- x$all[[1]];N                                      #total N including all censored data
  
  #max.MDL <- max(x$limit[1]); max.MDL               #highest MDL level (based on censored=TRUE data only)
  #calculate max MDL
  if(nrow(subset(G, cen==TRUE)) != 0){
    max.MDL <- max(unique(subset(G, cen==TRUE)$obs)) 
  } else {max.MDL <- as.numeric(NA)}
  
  n.uncen <- nrow(G[G$obs>max.MDL,]); n.uncen   #number samples where obs greater than highest MDL                                 
  nDL <- x$all[[1]] - x$all[[2]]         #n uncensored values in full dataset
 
    screen(2)
    par(mar=c(5,5,1,0)) 
    if(data.col=="Data.log"){
      with (G, boxplot(datacol, range=1.5, ylim=yax.range, ylab=paste("Log( ",unique(G$Parameter), " Concentration(", unique(G$Units), ") )"), names="ALL DATA"))
    }else {
      with (G, boxplot(datacol, range=1.5, ylim=yax.range, ylab=paste(unique(G$Parameter), " Concentration(", unique(G$Units), ")"), names="ALL DATA"))
    }
    
    if (!is.null(max.MDL)) {
      abline(h=max.MDL, col="red", lwd=2)  #horizontal red line at max MDL
      mtext(paste("Max MDL =", max.MDL, " (red line)"), side =3, line=0, cex=0.6)
    } else mtext(paste("NO CENSORED DATA"), side =3, line=0, cex=0.6)
    
    mtext(paste("ALL DATA, N=", N), side =1, line=1, cex=0.7)
    mtext(paste("(n>highest MDL=", n.uncen,")"), side =1, line=2, cex=0.6)
  
    grid(nx=NA, NULL)
  
#   ##PLOT BOXPLOT BY GROUP 
  
  screen(3)
  G2 <- G; G2$group <- droplevels(G2$group)
  if(length(unique(G2$group))>0) {           
    par(mar=c(5,0,1,1))
    with(G2, boxplot(datacol ~ group, top=TRUE, range=1.5, ylim=yax.range, yaxt="n", group=group,  cex.axis=0.6, las=2))
    seasonal.medians <- by(G2$datacol, G2$group, median, na.rm=T)
    seasonal.medians <- as.numeric(seasonal.medians)         
    lines(1:length(unique(G2$group)), seasonal.medians, col="blue", lwd=2)
    if (!is.null(max.MDL)) { abline(h=max.MDL, col="red", lwd=2)}
    #mtext(paste("Months with less than 2 uncensored values not shown"), side =1, line=2,cex=0.7)
    grid(nx=NULL, NULL)
  }else {text(0.5,0.6,paste("Insufficient Data to Plot Monthy Groups"), srt=55)}
 
  
  #legend("topright", legend = c("Max MDL"),
         #text.width = strwidth("1,000,000"),
  #       lty = 1, xjust = 1, yjust = 1)
  

#   screen(4)
#   #table on months with no data
#   if(length(months.MV)==0){
#     par(mar=c(3,0,1,1))   
#     PerformanceAnalytics::textplot("No Months with NO DATA", valign="top", halign="left", wrap=TRUE, cex=1)
#   }else {
#     PerformanceAnalytics::textplot(months.MV, show.rownames=FALSE, show.colnames=FALSE, valign="top", halign="left", wrap=TRUE,new=TRUE, cex=1)
#   }
#   mtext((paste(length(months.MV),"Months with NO DATA:")), side =3, line=1, cex=1, adj=0, font=2)
#   #box(which="plot", lty = "solid")
#   
#   
#   screen(5)
#   #table with N, ncen (alllvls), n and % exceeding highest MDL
#   PerformanceAnalytics::textplot(tot.TT2sub, show.rownames=FALSE,show.colnames=TRUE, valign="top",  rmar=0.1, cmar=1, halign="left", wrap=TRUE, cex=0.8)
#   mtext(paste("Censored Data Summary:"), side =3, line=4, cex=1, adj=0, font=2)
#   
  close.screen(all = TRUE)  
  
  #detach(package:PerformanceAnalytics)
  
  #p <- recordPlot()
  
  #graphics.off()
  #return(p)
  
}#end of function


#------------------------------------------------------------------------
# Censored Boxplots
#------------------------------------------------------------------------

Cen.Box <- function(df, data.col, group){
  
  #require(PerformanceAnalytics)
  cat("Running Censored Boxplots (Cen.box) for ",  unique(df$Parameter),"\n")   
  
  G <- df
  
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  #ADD group COLUMN which is identical to the col dentified in group in function def 
  G$group <- G[,grep(paste("^",group,"$", sep=""), names(G), value=TRUE)]
  
  #G$MED <- median(G$datacol, na.rm=TRUE)
  
  #y axis limits (to ensure 2 boxplots have the same y-axis)
  yax.min <- min(G$datacol)
  yax.max <-max(G$datacol)
  yax.range <- c(yax.min,yax.max)
  
  #setup plot parameters using split screen
  split.screen(c(1,1))
  par(oma=c(1,1,5,1))
  
  #overall PAGE Title
  mtext(paste("Censored Boxplot for ", unique(G$Parameter), "at ", unique(G$StationID)), side =3, line=1, outer=TRUE, cex=1.4)
  
  #fig.mat <- c(0,0.27,0.7,0.7,0.27,0.7,1,1,0,0,0.4,0,1,1,1,0.6)
  #fig.mat <- matrix(fig.mat,nrow=4)
  fig.mat <- c(0,0.27, 0.27, 1, 0,0,1,1)
  fig.mat <- matrix(fig.mat,nrow=2)
  split.screen(fig.mat, screen=1)
  
  #PLOT 1: CENSORED BOXPLOTS - ALL DATA
  x <- with(G, censummary(datacol, cen))       #censored summary information
  
  N <- x$all[[1]];N                                      #total N including all censored data
  
  #max.MDL <- max(x$limit[1]); max.MDL               #highest MDL level (based on censored=TRUE data only)
  #calculate max MDL
  if(nrow(subset(G, cen==TRUE)) != 0){
    max.MDL <- max(unique(subset(G, cen==TRUE)$obs)) 
  } else {max.MDL <- as.numeric(NA)}
  
  n.uncen <- nrow(G[G$obs>max.MDL,]); n.uncen   #number samples where obs greater than highest MDL                                 
  nDL <- x$all[[1]] - x$all[[2]]         #n uncensored values in full dataset
  
  if (nDL<2){    #overall boxplot will not plot without at least 2 uncensored data points?)
    screen(1)
    text(0.2,0.6,paste("Insufficient Data to Plot Overall Boxplot (N uncensored <2)"), srt=55)
  }
  
  if (nDL >=2){   
    screen(2)
    par(mar=c(5,5,1,0)) 
    with (G, cenboxplot(datacol, cen, log=FALSE, range=1.5, ylim=yax.range, ylab=paste(unique(G$Parameter), " Concentration (", unique(G$Units), ")" ), names="ALL DATA"))
    
    if (!is.null(max.MDL)) {
      abline(h=max.MDL, col="red", lwd=2)  #horizontal red line at max MDL
      mtext(paste("Max MDL =", max.MDL, " (red line)"), side =3, line=0, cex=0.6)
    } else mtext(paste("NO CENSORED DATA"), side =3, line=0, cex=0.6)
    
    mtext(paste("ALL DATA, N=", N), side =1, line=1, cex=0.7)
    mtext(paste("(n>highest MDL=", n.uncen,")"), side =1, line=2, cex=0.6)
    
  }
  grid(nx=NA, NULL)
  
  #   ##PLOT CENSORED BOXPLOT BY MONTH (include only if ncen >2)
  screen(3)
  
  if(length(unique(G$group))>0) {           
    par(mar=c(5,0,1,1))
    with (G, cenboxplot(datacol, cen, log=FALSE, range=1.5, ylim=yax.range, yaxt="n", group=group, cex.axis=0.6, las=2))
    if (!is.null(max.MDL)) { abline(h=max.MDL, col="red", lwd=2)}
    mtext(paste("Months with less than 2 uncensored values not shown"), side =1, line=2,cex=0.7)
    grid(nx=NULL, NULL)
  }else {text(0.5,0.6,paste("Insufficient Data to Plot Monthy Groups"), srt=55)}
  
  
  #   screen(4)
  #   #table on months with no data
  #   if(length(months.MV)==0){
  #     par(mar=c(3,0,1,1))   
  #     PerformanceAnalytics::textplot("No Months with NO DATA", valign="top", halign="left", wrap=TRUE, cex=1)
  #   }else {
  #     PerformanceAnalytics::textplot(months.MV, show.rownames=FALSE, show.colnames=FALSE, valign="top", halign="left", wrap=TRUE,new=TRUE, cex=1)
  #   }
  #   mtext((paste(length(months.MV),"Months with NO DATA:")), side =3, line=1, cex=1, adj=0, font=2)
  #   #box(which="plot", lty = "solid")
  #   
  #   
  #   screen(5)
  #   #table with N, ncen (alllvls), n and % exceeding highest MDL
  #   PerformanceAnalytics::textplot(tot.TT2sub, show.rownames=FALSE,show.colnames=TRUE, valign="top",  rmar=0.1, cmar=1, halign="left", wrap=TRUE, cex=0.8)
  #   mtext(paste("Censored Data Summary:"), side =3, line=4, cex=1, adj=0, font=2)
  #   
  close.screen(all = TRUE)  
  
  #detach(package:PerformanceAnalytics)
  
  #p <- recordPlot()
  
  #graphics.off()
  #return(p)
  
}#end of function
