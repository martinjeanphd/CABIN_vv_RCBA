########################################
# NORMALITY PLOTS for 1 parameter 
########################################

qqros <- function(df, data.col){
  
  cat("Running Normality Plots (qqros) for ",  unique(df$Parameter),"\n")  
  
  G <- df
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
   
  par(oma=c(2,2,2,0))  
  
  lo <- rbind(c(7,7,7,7,7), c(1,2,0,3,4), c(8,8,8,8,8), c(5,5,0,6,6))
  wd <- c(3,1,0.5,3,1,4,4,5,5)
  hd <- c(0.2,3,0.5,3)
  layout(lo, widths=wd, heights=hd)
  #layout.show(8)           
  
  #layout1:
  par(mar=c(4,4,2,0))
  with(G, qqnorm(datacol, main="Normal Q-Q plot", ylab="Concentration"))
  qqline(G$datacol, col="red")
  #mtext("Normal Q-Q Plot",outer=F,side=3,line=0.5, cex=0.8)
  #mtext("test", outer=F, side=2, line=2)
  
  #layout2:
  par(mar=c(3.5, 0, 2, 0))
  yhist <- hist(G$datacol, plot=FALSE) # note: this uses probability=TRUE
  barplot(yhist$counts,axes=FALSE,space=0, horiz=TRUE) 
  
  #layout3:
  par(mar=c(4,4,2,0))
  if (min(G$datacol) > 0){
    qqnorm(log(G$datacol),main="Normal Q-Q plot for Log(Conc.)", ylab="Log(Concentration)")
    qqline(log(G$datacol), col="red")
  }else {
    plot(1,1, col="White", axes=F, ylab="", xlab="")
    text(1,1, "Dataset contains Zero or Negative Values")
  }
  #mtext("Normal Q-Q Plot",outer=F,side=3,line=0.5, cex=0.8)
  #mtext("test", outer=F, side=2, line=2)
  
  #layout4:
  par(mar=c(3.5, 0, 2, 0))
  yhist <- hist(log(G$datacol), plot=FALSE) # note: this uses probability=TRUE
  barplot(yhist$counts,axes=FALSE,space=0, horiz=TRUE) 
  
  #layout5:
  #myros <- with(x, cenros(obs,cen))
  par(mar=c(4,4,4,4))
  myros <- with(G, cenros(datacol,cen, forwardT=NULL, reverseT =NULL))
  plot(myros)
  mtext("Normal QQ plot using ROS",outer=F,side=3,line=5,  font=2, cex=0.8)
  
  
  #layout6:ROS (lognormal)
  par(mar=c(4,4,4,4))
  if (min(G$datacol) > 0){
    myros2 <- with(G, cenros(datacol,cen, forwardT="log", reverseT ="exp"))
    plot(myros2)
    mtext("(LOG)Normal QQ plot using ROS",outer=F,side=3,line=5, font=2, cex=0.8)
  }else {
    plot(1,1, col="White", axes=F, ylab="", xlab="")
    text(1,1, "Dataset contains Zero or Negative Values")
  } 
  #layout7: Overall Title
  #par(mar=c(0,0,0,0))
  #plot.new()
  mtext(paste("Normality Plots for ", unique(G$Parameter), " at ", unique(G$StationID)), outer=T, line=-1,side=3, cex=0.8)
  
}#end function

#mycen <- cenfit(data.df$obs, data.df$cen)
#plot(mycen)

#mymle <- cenmle(data.df$obs, data.df$cen)
#plot(mymle, main="test")


#qqros(data.df)


########################################
# NORMALITY PLOTS using ggplot FACET (all parameters)
########################################

#NORMAL Q-Q PLOTS (w.ggplot)
QQ.Norm.FACET <- function(df, data.col, log.axis=c("YES", "NO"), log.data=c("YES", "NO")){
  
  cat("Running Normality Plots Facetted by parameter (QQ.Norm.FACET)","\n")  
  
  G <- df
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  
  theme_set(theme_bw())
  
  if(log.data=="NO") {
  p1 <- ggplot(G, aes(sample = datacol)) + 
    xlab("Theoretical Quantiles") + ylab("Concentration - note y axis varies by plot") +
    ggtitle(paste("Normal Q-Q Plot: Concentration vs Normal Quantiles at ",unique(G$StationID)))
  } else {
    p1 <- ggplot(G, aes(sample = log(datacol))) + 
      xlab("Theoretical Quantiles") + ylab("Log(Concentration) - note y axis varies by plot") +
      ggtitle(paste("Log-Normal Q-Q Plot: Log(Concentration) vs Normal Quantiles at ",unique(G$StationID)))
  }
  
  p2 <- p1  + stat_qq(distribution=qnorm, alpha=0.5) +
    facet_wrap(~Parameter, scales="free_y")+
    theme(plot.title = element_text(face="bold"))
  
  
  print(p2)
}#end QQ.Norm