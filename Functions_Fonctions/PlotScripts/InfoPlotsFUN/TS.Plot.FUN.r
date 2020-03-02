
#------------------------------------------------------------------------
# Function for Time Series Plot with GGPLOT.  Plot includes:
#  - Concentration vs TIME
#  - Lowess, SK and linear regression line
#  - Censored data shown as solid red
#------------------------------------------------------------------------

TSFplot.FF <- function(df, data.col){
  #df = dataframe with data column
  
  #require(ggplot2)
  #require(reshape2)
  
  cat("Running Time Series Plot (TSFplot.FF) for ",  unique(df$Parameter),"\n")   
  
  G <- df
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  G$MED <- median(G$datacol, na.rm=TRUE)
  
  #determines if there is flow data
  n.flow <- sum(G$Flow, na.rm=TRUE)
  
  #calculate max MDL
  #x <- with(df, censummary(obs, cen))       #censored summary information
  #max.MDL <- max(x$limit[1])    
  if(nrow(subset(G, cen==TRUE)) != 0){
    max.MDL <- max(unique(subset(G, cen==TRUE)$obs))
    
  } else {max.MDL <- as.numeric(NA)}
  
  G$maxMDL <- max.MDL
  
  
  #Concentration Plot
  if(data.col == "Data.log"){
    c1 <- ggplot(G,aes(x=Date,y=datacol)) +
    geom_line() + geom_point(shape=1, size=3) +
    ylab(paste("Log (",unique(G$Parameter), "(", unique(G$Units) , ") )")) +
    ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  }else {
    c1 <- ggplot(G,aes(x=Date,y=datacol)) +
      geom_line() + geom_point(shape=1, size=3) +
      ylab(paste(unique(G$Parameter), "( ", unique(G$Units) , " )")) +
      ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  }
  #overlay censored pts in red
  #c2 <- c1 + geom_point(aes(group=cen, color=cen),size=3,  shape=19)+ 
  if(nrow(subset(G, cen==TRUE))!=0){
    c2 <- c1 + geom_point(data=subset(G, cen==TRUE), size=3, shape=19, aes(colour="a", linetype="a")) +
      scale_x_date(breaks="1 year", labels=date_format("%Y")) +
      #scale_fill_discrete(name="", labels="Censored Data") +
      theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  } else { 
    
    #df$Null <- as.numeric(NA)
    
    c2 <- c1 +   
      geom_point(size=3, shape=19, aes(y=as.numeric(NA), colour="a", linetype="a")) +
      scale_x_date(breaks="1 year", labels=date_format("%Y")) +
      #scale_fill_discrete(name="", labels="Censored Data") +
      theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  }
  
  #overlay max.MDL if one exists
  #if (is.null(max.MDL == FALSE)){rm(G
  
  c3 <-  c2 + geom_hline(aes(yintercept=MED, colour="b", linetype="b"), show_guide=TRUE) +
    geom_hline(aes(yintercept=maxMDL, colour="c", linetype="c"),   show_guide=TRUE)           
  #}else {
  #overlay med 
  #  c3 <- c2 + geom_hline(aes(yintercept=median(df$datacol, na.rm=T), colour="b", linetype="b"), show_guide=TRUE) 
  #}
  
  #add appropriate legend
  c4 <- c3 +  scale_linetype_manual(name="",breaks=c("a","b", "c"), values=c("solid", "solid", "dashed"), labels=c("Censored Data", "Record Median", "Max MDL")) +
    scale_colour_manual(name="",  breaks=c("a","b", "c"), values=c("red", "red", "blue"), labels=c("Censored Data","Record Median", "Max MDL")) + 
    guides(colour = guide_legend(override.aes = list(linetype= c(0,1,2), shape=c(19, NA,NA))))   
  
  if(n.flow != 0){
  #fix axis and margins
  c5<- c4 +  theme(axis.text.x = element_blank(), 
                   axis.ticks.x=element_blank(),
                   axis.title.x=element_blank(),
                   plot.margin=unit(c(1,3,-0.2,1), "lines"), 
                   legend.position = "top")    #to reduce space bewteen plots
                                                #unit with the sizes of the top, right, bottom, and left margins

    #ADD Flow Plot
    f1 <- ggplot(G, aes(x=Date, y=Flow)) + 
      geom_line() + geom_point(shape=1, size=3) +
      xlab("Date") + ylab("Flow") +
      #scale_x_date(breaks=pretty_breaks(),minor_breaks = "1 year", labels=date_format("%Y"))+
      scale_x_date(breaks="1 year", labels=date_format("%Y"))+
      theme_bw() +  
      theme(plot.margin=unit(c(0,3,1,1), "lines"), 
            axis.text.x  = element_text(angle=90, vjust=0.5))
    
    #to plot multiple ggplots with axis lined up properly!
    gA <- ggplotGrob(c5)
    gB <- ggplotGrob(f1)
    maxWidth = grid::unit.pmax(gA$widths[2:3], gB$widths[2:3])
    gA$widths[2:3] <- as.list(maxWidth)
    gB$widths[2:3] <- as.list(maxWidth)
    grid.arrange(gA, gB, ncol=1)
    
  }else suppressWarnings(print(c4))
  #plot layout on 1 pages --> sometimes the axis doesnt line up (eg scale width differs)
  #grid.newpage()
  #vplayout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
  #pushViewport(viewport(layout = grid.layout(2, 2, widths=unit(c(0.5,9), "null"), heights = unit(c(5.5, 3), "null"))))
  #print(c2,vp=viewport(layout.pos.row = 1, layout.pos.col = 1)))
  #plot(c2, vp=vplayout(1,2))
  #print(f1,vp=vplayout(2,2))
  
  
  #graphics.off()
  #return(g2)
}#end function




#------------------------------------------------------------------------
# Function for Time Series Plot with GGPLOT.  Plot includes:
#  - Concentration (top graph) and Flow (bottom graph)
#  - Median and Max MDL
#  - Censored data shown as solid red
#-------------------------------------------------------------------------

TSplot.lines <- function(df, data.col, span.value){
    #df = dataframe with data column
    
    #require(ggplot2)
    #require(reshape2)
    
    cat("Running Time Series Plot with SK, LR and Lowess lines (TSplot.lines) for ",  unique(df$Parameter),"\n")   
    
    G <- df
    #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
    G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
    
    #Concentration Plot
    if(data.col == "Data.log"){
      c1 <- ggplot(G,aes(x=dates.dec,y=datacol)) +
      geom_line() + geom_point(shape=1, size=3) +
      ylab(paste("log (",unique(G$Parameter), "(", unique(G$Units) , ") )")) +
      ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
    }else {
      c1 <- ggplot(G,aes(x=dates.dec,y=datacol)) +
        geom_line() + geom_point(shape=1, size=3) +
        ylab(paste(unique(G$Parameter), "( ", unique(G$Units) , " )")) +
        ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
    }
    #overlay censored pts in red
    #c2 <- c1 + geom_point(aes(group=cen, color=cen),size=3,  shape=19)+ 
    #suppressWarnings(
    if(nrow(subset(G, cen==TRUE))!=0){
      c2 <- c1 + geom_point(data=subset(G, cen==TRUE), size=3, shape=19, colour="red") # +
        #scale_x_date(breaks="1 year", labels=date_format("%Y")) +
        #scale_fill_discrete(name="", labels="Censored Data") +
        #theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
    } else { 
      c2 <- c1 +   
        geom_point(size=3, shape=19, aes(y=as.numeric(NA)), colour="red") #+
        #scale_x_date(breaks="1 year", labels=date_format("%Y")) +
        #scale_fill_discrete(name="", labels="Censored Data") +
        #theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
    }
    
    #)
#add lowess 
c3 <-  c2 +  geom_smooth(method="loess", se=TRUE, level=0.95, 
                         na.rm=TRUE, span=span.value, show_guide=TRUE, 
                         aes(colour="b", fill="b"), alpha=0.1)       

#add LR 
c4 <- c3 + geom_smooth(method="lm", se=FALSE, 
                       aes(colour="c",  fill="c"), alpha=0.1)

#add Seasonal Kendall line
    #need to skip if n in a season < 3
      cnt <- ddply(df, .(Season), summarize, count=sum(!is.na(Data)))
      if (any(cnt$count < 3)) {
        SKres <- data.frame(INT=NA, SLOPE=NA)
      }else{
      SKres <- seaken_plot(x=G$dates.dec, y=G$datacol, group=G$Season)
      }
      
      c5 <- c4 + geom_abline(data=SKres, aes(intercept=INT, slope=SLOPE, colour="d",  fill="d"))
      
      #add appropriate legend
      lab <- c(paste("Lowess - span: ", span.value),
               "Linear Reg", 
               "Seasonal Thiel-Sen Slope")

      c6 <- c5 + scale_colour_manual(name="",  breaks=c("b", "c","d"), values=c("blue", "green", "red"), labels=lab) + 
                  scale_linetype_manual(name="",breaks=c("b", "c","d"), values=c("dashed", "dotted", "solid"), labels=lab) +
                  scale_fill_manual(name="",breaks=c("b", "c","d"), values=c("blue", "white", "white"), labels=lab )
#fix axis and margins
c7 <- c6 +  theme_bw() + 
  theme(axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        legend.position = "top")   

if (any(cnt$count < 3)) {
  c8 <- c7 
}else{    
  c8 <- c7 + geom_text(x = Inf, y = Inf, label = paste("Seasonal SenSlope: ", round(SKres$SLOPE, 3)), hjust = 1.2, vjust = 2, size=5)+
    geom_text(x = Inf, y = Inf, label = paste("pval: ", round(SKres$PVAL, 3)), hjust = 1.5, vjust = 4, size=5)
} 
    suppressWarnings(print(c8))
    
}#end function



#------------------------------------------------------------------------
# Function for Time Series Plot by Month with GGPLOT.  Plot includes:
#  - Concentration vs TIME
#  - monthly medians
#  - Censored data shown as solid red
#------------------------------------------------------------------------

TSMplot <- function(df, data.col){
  
  
  cat("Running Time Series Plot by Month (TSMplots) for ",  unique(df$Parameter),"\n")   
  
  G <- df
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  
  #Concentration Plot
  if(data.col == "Data.log"){
    c1 <- ggplot(G,aes(x=Date,y=datacol)) +
    geom_line() + geom_point(shape=1, size=3) +
    ylab(paste("Log( ", unique(G$Parameter), "(", unique(G$Units) , ") )")) +
    ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  }else {
    c1 <- ggplot(G,aes(x=Date,y=datacol)) +
      geom_line() + geom_point(shape=1, size=3) +
      ylab(paste(unique(G$Parameter), "( ", unique(G$Units) , " )")) +
      ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  }
  
  #overlay censored pts in red
  #c2 <- c1 + geom_point(aes(group=cen, color=cen),size=3,  shape=19)+ 
 # suppressWarnings(
    if(nrow(subset(G, cen==TRUE))!=0){
    c2 <- c1 + geom_point(data=subset(G, cen==TRUE), size=3, shape=19, colour="red") +
    scale_x_date(breaks="1 year", labels=date_format("%Y")) +
    #scale_fill_discrete(name="", labels="Censored Data") +
    theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  } else { 
    c2 <- c1 +   
      geom_point(size=3, shape=19, aes(y=as.numeric(NA)), colour="red") +
    scale_x_date(breaks="1 year", labels=date_format("%Y")) +
    #scale_fill_discrete(name="", labels="Censored Data") +
    theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  }
#)
  
  #add monthly medians
    c3 <- c2 + facet_grid(. ~ MonthName) +
          geom_hline(data=ddply(G,.(MonthName), summarise,mn = median(datacol, na.rm=TRUE)),  
                     aes(yintercept=mn, colour="a", linetype="a"), show_guide=TRUE) +
          geom_hline(aes(yintercept=median(datacol, na.rm=TRUE), colour="b", linetype="b"))
    
  #axis and labels:
  c4 <- c3 + xlab(paste("Years:",min(G$Year),"to", max(G$Year))) +
            theme_bw() + 
            theme(axis.text.x=element_blank(), 
                  legend.position = "top")+
            scale_color_manual(name="", breaks=c("a", "b"), values=c("red", "red"), labels=c("Monthly Medians", "Record Median")) +
             scale_linetype_manual(name="", breaks=c("a", "b"), values=c("solid", "dashed"), labels=c("Monthly Medians", "Record Median"))

  
  suppressWarnings(print(c4))
  
}#end function

#------------------------------------------------------------------------
# Function for Time Series Plot by Month with GGPLOT.  Plot includes:
#  - Concentration vs TIME
#  - monthly medians
#  - Censored data shown as solid red
#------------------------------------------------------------------------

TSMplot2 <- function(df, data.col){
  
  
  cat("Running Time Series Plot by Month (wrap) (TSMplots2) for ",  unique(df$Parameter),"\n")   
  
  G <- df
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  
  #Concentration Plot
  if(data.col == "Data.log"){
    c1 <- ggplot(G,aes(x=Date,y=datacol)) +
    geom_line() + geom_point(shape=1, size=3) +
    ylab(paste("Log( ", unique(G$Parameter), "(", unique(G$Units) , ") )")) +
    ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  }else {
    c1 <- ggplot(G,aes(x=Date,y=datacol)) +
      geom_line() + geom_point(shape=1, size=3) +
      ylab(paste(unique(G$Parameter), "( ", unique(G$Units) , " )")) +
      ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  }
  
  #overlay censored pts in red
  #c2 <- c1 + geom_point(aes(group=cen, color=cen),size=3,  shape=19)+ 
  if(nrow(subset(G, cen==TRUE))!=0){
    c2 <- c1 + geom_point(data=subset(G, cen==TRUE), size=3, shape=19, colour="red") +
      scale_x_date(breaks="1 year", labels=date_format("%Y")) +
      #scale_fill_discrete(name="", labels="Censored Data") +
      theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  } else { 
    c2 <- c1 +   
      geom_point(size=3, shape=19, aes(y=as.numeric(NA)), colour="red") +
      scale_x_date(breaks="1 year", labels=date_format("%Y")) +
      #scale_fill_discrete(name="", labels="Censored Data") +
      theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  }
  
  #add monthly medians
  c3 <- c2 + facet_wrap(~MonthName) +
    geom_hline(data=ddply(G,.(MonthName), summarise,mn = median(datacol, na.rm=TRUE)),  
               aes(yintercept=mn, colour="a", linetype="a"), show_guide=TRUE) +
    geom_hline(aes(yintercept=median(datacol, na.rm=TRUE), colour="b", linetype="b"))
  
  #axis and labels:
  c4 <- c3 + xlab(paste("Years:",min(G$Year),"to", max(G$Year))) +
    theme_bw() + 
    theme(axis.text.x=element_blank(), 
          legend.position = "top")+
    scale_color_manual(name="", breaks=c("a", "b"), values=c("red", "red"), labels=c("Monthly Medians", "Record Median")) +
    scale_linetype_manual(name="", breaks=c("a", "b"), values=c("solid", "dashed"), labels=c("Monthly Medians", "Record Median"))
  
  
  suppressWarnings(print(c4))
  
}#end function


#------------------------------------------------------------------------
# Function for Time Series Plot by Month with GGPLOT.  Plot includes:
#  - Concentration vs TIME
#  - monthly medians
#  - Censored data shown as solid red
#------------------------------------------------------------------------

TSYplot <- function(df, data.col){
  
  
  cat("Running Time Series Plot by Year (TSYplots) for ",  unique(df$Parameter),"\n")   
  
  G <- df
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  
  #Concentration Plot
  if(data.col == "Data.log"){
    c1 <- ggplot(G,aes(x=Month,y=datacol)) +
    geom_line() + geom_point(shape=1, size=3) +
    scale_x_continuous(breaks=seq(1,12, by=1)) +
    ylab(paste("Log (", unique(G$Parameter), "(", unique(G$Units) , ") )")) +
    ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  } else {
    c1 <- ggplot(G,aes(x=Month,y=datacol)) +
      geom_line() + geom_point(shape=1, size=3) +
      scale_x_continuous(breaks=seq(1,12, by=1)) +
      ylab(paste(unique(G$Parameter), "( ", unique(G$Units) , " )")) +
      ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  }
  
  #overlay censored pts in red
  #c2 <- c1 + geom_point(aes(group=cen, color=cen),size=3,  shape=19)+ 
  if(nrow(subset(G, cen==TRUE))!=0){
    c2 <- c1 + geom_point(data=subset(G, cen==TRUE), size=3, shape=19, colour="red") #+
      #scale_x_date(breaks="1 month", labels=date_format("%b")) +
      #scale_fill_discrete(name="", labels="Censored Data") +
      #theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  } else { 
    c2 <- c1 +   
      geom_point(size=3, shape=19, aes(y=as.numeric(NA)), colour="red") #+
      #scale_x_date(breaks="1 month", labels=date_format("%b")) +
      #scale_fill_discrete(name="", labels="Censored Data") +
      #theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  }
  
  #add monthly medians
  c3 <- c2 + facet_grid(~Year) +
    geom_hline(data=ddply(G,.(Year), summarise,mn = median(datacol, na.rm=TRUE)),  
               aes(yintercept=mn, colour="a", linetype="a"), show_guide=TRUE) +
    geom_hline(aes(yintercept=median(datacol, na.rm=TRUE), colour="b", linetype="b"))
  
  #axis and labels:
  c4 <- c3 + xlab(paste("Month: Jan to Dec")) +
    theme_bw() + 
    theme(axis.text.x=element_blank(), 
          legend.position = "top")+
    scale_color_manual(name="", breaks=c("a", "b"), values=c("red", "red"), labels=c("Annual Medians", "Record Median")) +
    scale_linetype_manual(name="", breaks=c("a", "b"), values=c("solid", "dashed"), labels=c("Annual Medians", "Record Median"))
  
  
  suppressWarnings(print(c4))
  
}#end function

#------------------------------------------------------------------------
# Function for Time Series Plot by YEar with GGPLOT.  Plot includes:
#  - Concentration vs TIME
#  - monthly medians
#  - Censored data shown as solid red
#
# by date, therefore good if n/month > 1
#------------------------------------------------------------------------

TSYplot2 <- function(df, data.col){
  
  
  cat("Running Time Series Plot by Year/Date (TSYplot2) for ",  unique(df$Parameter),"\n")   
  
  G <- df
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  
  #Concentration Plot
  if(data.col == "Data.log"){
    c1 <- ggplot(G,aes(x=Date,y=datacol)) +
    geom_line() + geom_point(shape=1, size=3) +
    #scale_x_continuous(breaks=seq(1,12, by=1)) +
    ylab(paste("log (", unique(G$Parameter), "(", unique(G$Units) , ") )")) +
    ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  }else {
    c1 <- ggplot(G,aes(x=Date,y=datacol)) +
      geom_line() + geom_point(shape=1, size=3) +
      #scale_x_continuous(breaks=seq(1,12, by=1)) +
      ylab(paste(unique(G$Parameter), "( ", unique(G$Units) , " )")) +
      ggtitle(paste("Time Series Plot for", unique(G$Parameter), "at", unique(G$StationID))) 
  }
  #overlay censored pts in red
  #c2 <- c1 + geom_point(aes(group=cen, color=cen),size=3,  shape=19)+ 
  if(nrow(subset(G, cen==TRUE))!=0){
    c2 <- c1 + geom_point(data=subset(G, cen==TRUE), size=3, shape=19, colour="red") +
    scale_x_date(breaks="1 month", labels=date_format("%b")) +
    #scale_fill_discrete(name="", labels="Censored Data") +
    theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  } else { 
    c2 <- c1 +   
      geom_point(size=3, shape=19, aes(y=as.numeric(NA)), colour="red") +
    scale_x_date(breaks="1 month", labels=date_format("%b")) +
    #scale_fill_discrete(name="", labels="Censored Data") +
    theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
  }
  
  #add monthly medians
  c3 <- c2 + facet_grid(~Year,  scale="free_x") +
    geom_hline(data=ddply(G,.(Year), summarise,mn = median(datacol, na.rm=TRUE)),  
               aes(yintercept=mn, colour="a", linetype="a"), show_guide=TRUE) +
    geom_hline(aes(yintercept=median(datacol, na.rm=TRUE), colour="b", linetype="b"))
  
  #axis and labels:
  c4 <- c3 + xlab(paste("Month: Jan to Dec")) +
    theme_bw() + 
    theme(axis.text.x=element_blank(),
          #axis.text.x  = element_text(angle=90, vjust=0.5),
          legend.position = "top")+
    scale_color_manual(name="", breaks=c("a", "b"), values=c("red", "red"), labels=c("Annual Medians", "Record Median")) +
    scale_linetype_manual(name="", breaks=c("a", "b"), values=c("solid", "dashed"), labels=c("Annual Medians", "Record Median"))
  
  
  suppressWarnings(print(c4))
  
}#end function


#------------------------------------------------------------------------
# Function for Time Series Plot GGPLOT FACETTED BY PARAMETER
#------------------------------------------------------------------------

TSplot.Facet <- function(df, data.col, span.value=0.65, log.axis=c("YES", "NO"), add.pts=c("YES", "NO")){
  
  
  cat("Running Time Series Plot (TSplot.Facet) Facet by parameter","\n")   
  
  G <- df
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  
  #Concentration Plot
  if (add.pts=="YES"){
    
    c1 <- ggplot(G,aes(x=Date,y=datacol)) +
    geom_line(alpha=0.5, lty=1) + geom_point(shape=1, size=3, alpha=0.5) 
 
    #overlay censored pts in red
    #c2 <- c1 + geom_point(aes(group=cen, color=cen),size=3,  shape=19)+ 
    if(nrow(subset(G, cen==TRUE))!=0){
      c2 <- c1 + geom_point(data=subset(G, cen==TRUE), size=3, shape=19, colour="red", alpha=0.5)   
    } else { 
      c2 <- c1 +  geom_point(size=3, shape=19, aes(y=as.numeric(NA)), colour="red", alpha=0.5) 
    }
  
  }else {c2 <-  ggplot(G,aes(x=Date,y=datacol))}
  
#   #add monthly medians
#   c3 <- c2 + facet_grid(. ~ MonthName) +
#     geom_hline(data=ddply(G,.(MonthName), summarise,mn = median(datacol, na.rm=TRUE)),  
#                aes(yintercept=mn, colour="a", linetype="a"), show_guide=TRUE) +
#     geom_hline(aes(yintercept=median(datacol, na.rm=TRUE), colour="b", linetype="b"))
#   
  
  #facet by parameter
  c3 <- c2 + facet_wrap(~Parameter, scales="free_y") 
  
  #add lowess smooth (to look at trend)
  c4 <- c3 + geom_smooth(method="loess", se=TRUE, level=0.95, 
                na.rm=TRUE, span=span.value, show_guide=TRUE, 
                aes(colour="a"), alpha=0.1)  + 
        geom_smooth(method="lm", se=TRUE, level=0.95, na.rm=TRUE, aes(color="b"), alpha=0.1)
      
  
  #axis and labels:
  c5 <- c4 + 
    ggtitle(paste("Time Series Plots for", unique(G$StationID))) +
    scale_colour_manual(name="",  breaks=c("a", "b"), values=c("blue", "dark green"), labels=c(paste("Lowess=", span.value), "Linear Reg")) + 
    scale_x_date(breaks="1 year", labels=date_format("%Y")) +
    theme_bw() + theme(axis.text.x  = element_text(angle=90, vjust=0.5)) +
    theme(legend.position = "top") 
  
  if(log.axis=="NO"){
    c6 <- c5 + ylab("Concentration")
  }else if (log.axis=="YES"){
    c6 <- c5 +
          #transform the data --> needs to be done earlier
          #transform the scales
          scale_y_log10() + ylab("Log(Concentration)")
          #scale_y_continuous(trans="log", breaks=trans_breaks("log","exp"), 
          #                   labels=trans_format("log", math_format(10^.x)))
          #transform the coordinate system
          #coord_trans(y = "log")

  }
  
  suppressWarnings(print(c6))
  
}#end function

