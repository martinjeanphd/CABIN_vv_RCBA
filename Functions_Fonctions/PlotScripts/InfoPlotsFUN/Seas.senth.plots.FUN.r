
#########################################################################
# PLOTS OF SEASONAL AND MK SLOPE
#########################################################################

plot.seasSK <- function(df, data.col, SCALES="fixed"){
 
  cat("Running Seasonal Thiel-Sen Plot(plot.seasSK) for ",  unique(df$Parameter),"\n")   
  
  G <- df
  stnID <- unique(G$StationID)
  param <- unique(G$Parameter)
  p.units <- unique(G$Units)
  
 
  #add data.col to DTDS.data 
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  
  #need to skip if n in a season < 3
  cnt <- ddply(G, .(Parameter), summarize, count=sum(!is.na(datacol)))
  cnt2 <- ddply(G, .(Season), summarize, count=sum(!is.na(datacol)))
  
  if (any(cnt$count < 4)) {
    cat("   No Plot Created: There is not sufficient data (n<4) to calculate a Thiel-Sen Slope","\n")  
  }else {
    
    if(any(cnt2$count < 4)){
    cond1 <- "TRUE"   #at least one season has <3 datapts
    ms <- subset(cnt2, count <= 4, select="Season")
    mss <- ms[,names(ms) %in% c("Season")]
    }else {cond1 <- "FALSE"}
  
  
    #SET THE THEME TO BW GLOBALLY: (all plots will be drawn with this)
    theme_set(theme_bw())
    
    #can't add senslope abline if x=Date
    p <- ggplot(data=G, aes(x=dates.dec, y=datacol)) + geom_point() +
      geom_line(linetype="dashed")

    if(data.col =="Data"){
      p1 <- p +
        ylab(paste(param, "Conc. (", p.units, ")")) + xlab("") +
        #scale_x_date(breaks="1 year", labels=date_format("%Y")) +    #this can only be used with Date format
        scale_x_continuous(breaks=seq(min(G$Year), max(G$Year)+1, by=1))+
        theme(axis.title.y=element_text(size=10)) + 
        theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
    }else if (data.col =="Data.log"){
      p1 <- p +  
        ylab(paste("log", param, "Conc.(", p.units, ")")) + xlab("") +
        #scale_x_date(breaks="1 year", labels=date_format("%Y"))+
        scale_x_continuous(breaks=seq(min(G$Year), max(G$Year)+1, by=1))+
        theme(axis.title.y=element_text(size=10)) + 
        theme(axis.text.x  = element_text(angle=90, vjust=0.5)) 
    }
    
    #overlay censored pts in red
    #c2 <- c1 + geom_point(aes(group=cen, color=cen),size=3,  shape=19)+ 
    if(nrow(subset(G, cen==TRUE))!=0){
      p2 <- p1 + geom_point(data=subset(G, cen==TRUE), size=3, shape=19, aes(colour="Censored Data", linetype="Censored Data")) 
    } else { 
      p2 <- p1 +   
        geom_point(size=3, shape=19, aes(y=as.numeric(NA), colour="Censored Data", linetype="Censored Data"), na.rm=TRUE)    
    }
  
   #add overall sens slope line to plot
    all.Sen <- with(G, senth(x=dates.dec, y=datacol))
    
    
    p3 <- p2 + geom_abline(data=all.Sen, aes(intercept=INTERCEPT, slope=MEDIANSLOPE, colour = "Overall Theil-Sen", linetype="Overall Theil-Sen"), show_guide=TRUE) 
    
    # GET SLOPE AND PVALUE AS STRING
    sen_eqn = function(df1, nm){  #from senth output
      m = senth(x=df1$dates.dec, y=df1$datacol)
      eq <- substitute(paste(nm1 == a*", ", pval== b),
                       list(a = format(m$MEDIANSLOPE, digits = 4), 
                            b = format(m$PVAL, digits = 2), 
                            nm1 = as.character(nm)))
        
      as.character(as.expression(eq))               
    }
    
    sen_eqn2 = function(df1, nm){   #from sea.senth output
      m = sea.senth(x=df1$dates.dec, y=df1$datacol, group=df1$Season)
      eq <- substitute(paste(nm1 == a*", ", pval== b),
                       list(a = format(m$SLOPE, digits = 4), 
                            b = format(m$PVAL, digits = 2),
                            nm1 = as.character(nm)))
      
      as.character(as.expression(eq))               
    }
      
    #ADD OVERALL MK SLOPE
    mod <- senth(G$dates.dec, G$datacol)
    eq1 <- data.frame(eq=unclass(sen_eqn(G, nm="Overall Slope"))) 
    
    #add min/max eq to eq1 because aes looks in data, then global environmnet
    eq1$min.eq <- min(G$dates.dec);  eq1$max.eq <- max(G$datacol)
    
    p4 <- p3 + geom_text(data=eq1, aes(x=min.eq, y = max.eq, label = eq), 
                         hjust=0,vjust=0,parse = TRUE, size=4, color="red") #+
      #annotate("text", x =eq1$min.eq, y =eq1$max.eq, label = "Overall Thiel-Sen Slope:",
               #hjust=0,size=4, color="red")
      
    if (cond1=="TRUE"){
      pp4 <-  p4  + ggtitle(paste("Overall Thiel Sen Slopes for", param, "at", stnID)) + 
        scale_linetype_manual(name="",breaks=c("Censored Data","Overall Theil-Sen"), values=c("solid", "dashed"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) +
        scale_colour_manual(name="",  breaks=c("Censored Data","Overall Theil-Sen"), values=c("red", "red"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) + 
        guides(colour = guide_legend(override.aes = list(linetype = c(0,2), shape=c(19, NA)))) + 
        theme(legend.position = "top") + xlab(paste("Note: Insufficient data (n<3) to calculate Seasonal Slope in seasons: ", paste(mss, collapse = ", ")))
    }                                          
                                              
    if (cond1=="FALSE"){
      all.SeaSen <- with(G, sea.senth(x=dates.dec, y=datacol, group=Season))
      p5 <- p4 + geom_abline(data=all.SeaSen, aes(intercept=INT, slope=SLOPE, colour = "Overall Seasonal Thiel-Sen Slope", linetype="Overall Seasonal Thiel-Sen Slope"), show_guide=TRUE) 
       
      #ADD OVERALL SEASONAL SEN SLOPE
      mod2 <- sea.senth(x=G$dates.dec, y=G$datacol, group=G$Season)
      eq2 <- data.frame(eq=unclass(sen_eqn2(G, nm="Overall Seasonal Slope"))) 
      
      #add min/max eq to eq1 because aes looks in data, then global environmnet
      eq2$min.eq <- min(G$dates.dec);  eq2$max.eq <- max(G$datacol)
      
      p6 <- p5+ geom_text(data=eq2, aes(x=min.eq, y = max.eq, label = eq), 
                           hjust=0,vjust=2,parse = TRUE, size=4, color="darkgreen") 
      
      p7 <- p6  + ggtitle(paste("Overall Thiel Sen Slopes for", param, "at", stnID)) + 
        scale_linetype_manual(name="",breaks=c("Censored Data","Overall Theil-Sen", "Overall Seasonal Thiel-Sen Slope"), values=c("solid", "dashed", "solid"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) +
        scale_colour_manual(name="",  breaks=c("Censored Data","Overall Theil-Sen", "Overall Seasonal Thiel-Sen Slope"), values=c("red", "red", "darkgreen"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) + 
        guides(colour = guide_legend(override.aes = list(linetype = c(0,2, 3), shape=c(19, NA,NA)))) + 
        theme(legend.position = "top")
    }else if (cond1=="TRUE") {p7 <- pp4}
  
  
  #++++++++++++++++++++++++++++++++
  #ADD FACETTED PLOTS BY SEASON
  #++++++++++++++++++++++++++++++++
  
  if (cond1=="FALSE"){
   G2 <- G 
   G3 <- G
   all.SeaSen <- with(G, sea.senth(x=dates.dec, y=datacol, group=Season))
   pp5 <- p3 + geom_abline(data=all.SeaSen, aes(intercept=INT, slope=SLOPE, colour = "Overall Seasonal Thiel-Sen Slope", linetype="Overall Seasonal Thiel-Sen Slope"), show_guide=TRUE) 
   s0 <- pp5
  }else if (cond1=="TRUE") {
    #insert NA in G for seasons in mss
    G2 <- G
    G2$datacol[G2$Season %in% mss] <- NA
    
    G3 <- subset(G2, !Season %in% mss)
    
    s0 <- p3
  }
  
  if(nrow(G3) != 0){
    
    df2 <- ddply(G3,.(Season),summarise,INT=senth(x=dates.dec, y=datacol)$INTERCEPT,
                 SLOPE=senth(x=dates.dec, y=datacol)$MEDIANSLOPE)
    
    s1 <- s0 + geom_abline(data=df2, aes(intercept=INT, slope=SLOPE, colour = "Seasonal Thiel-Sen", linetype="Seasonal Thiel-Sen"))
    s2 <- s1 + facet_wrap(~Season, scales=SCALES, drop=FALSE)
    
    eqns <- by(G3, G3$Season, sen_eqn, nm="Season Slope")
    df3 <- data.frame(eq = unclass(eqns), Season = as.numeric(names(eqns)))
    
    df3$min.eq <- min(G3$dates.dec);   df3$max.eq <-  max(G3$datacol); 
  
    s3 <- s2 + geom_text(data=df3, aes(x=min.eq, y = max.eq, label = eq), 
                         hjust=0,vjust=1,parse = TRUE, size=3, color="blue") #+
      #geom_text(data=eq1, aes(x=min.eq, y = max.eq, label = eq), 
      #          hjust=0,vjust=0.5,parse = TRUE, size=3, color="red") +
      #geom_text(data=eq2, aes(x=min.eq, y = max.eq, label = eq), 
      #          hjust=0,vjust=2,parse = TRUE, size=3, color="darkgreen")
    
  
    exp1 <- paste("Overall Thiel Sen Slope: ", format(mod$MEDIANSLOPE, digits = 4), " (pval=", format(mod$PVAL, digits = 2), ")", sep="")
  }
    
    if(cond1=="FALSE"){
      exp2<- paste("Overall Seasonal Thiel Sen Slope: ", format(mod2$SLOPE, digits = 4), " (pval=",  format(mod2$PVAL, digits = 2), ")", sep="")  
      xlabel <- paste(exp1, "\n" , exp2)
      
      s4 <- s3  +   #ggtitle(expression(atop(paste("Overall Thiel Sen Slopes for", param, "at", stnID), 
        #                        atop(italic(paste("Overall Thiel Sen Slope: ", mod$MEDIANSLOPE, "(pval= ", mod$PVAL, ")")),
        #                             italic(paste("Overall Seasonal Thiel Sen Slope: ", mod2$SLOPE, "(pval= ", mod2$PVAL, ")")))))) +
        #plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = -1) +
        ggtitle(paste("Thiel-Sen Slopes by Season for", param, "at", stnID)) + 
        scale_linetype_manual(name="",breaks=c("Censored Data","Seasonal Thiel-Sen", "Overall Theil-Sen", "Overall Seasonal Thiel-Sen Slope"), values=c("solid", "dashed",  "solid", "solid"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) +
        scale_colour_manual(name="",  breaks=c("Censored Data","Seasonal Thiel-Sen","Overall Theil-Sen", "Overall Seasonal Thiel-Sen Slope"), values=c("red",  "red", "darkgreen", "blue"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) + 
        guides(colour = guide_legend(override.aes = list(linetype = c("blank","solid","dashed","solid"), shape=c(19, NA, NA, NA)))) +
        theme(legend.position = "top")  +
        xlab(xlabel)
         
    }else if (cond1=="TRUE" && nrow(G3)!= 0){
      xlabel <- paste(exp1, "\n" , "Note: Insufficient data in some Seasons to Calculate Seasonal Slopes")
      
      s4 <- s3  +   #ggtitle(expression(atop(paste("Overall Thiel Sen Slopes for", param, "at", stnID), 
        #                        atop(italic(paste("Overall Thiel Sen Slope: ", mod$MEDIANSLOPE, "(pval= ", mod$PVAL, ")")),
        #                             italic(paste("Overall Seasonal Thiel Sen Slope: ", mod2$SLOPE, "(pval= ", mod2$PVAL, ")")))))) +
        #plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = -1) +
        ggtitle(paste("Thiel-Sen Slopes by Season for", param, "at", stnID)) + 
        scale_linetype_manual(name="",breaks=c("Censored Data","Seasonal Thiel-Sen", "Overall Theil-Sen"), values=c("solid", "dashed",  "solid"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) +
        scale_colour_manual(name="",  breaks=c("Censored Data","Seasonal Thiel-Sen","Overall Theil-Sen"), values=c("red",  "red", "blue"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) + 
        guides(colour = guide_legend(override.aes = list(linetype = c(0,1,2), shape=c(19, NA, NA)))) +
        theme(legend.position = "top")  +
        xlab(xlabel)
      
      
    } else if (cond1=="TRUE" && nrow(G3)== 0 ){
      xlabel <- paste(exp1, "\n" , "Note: Insufficient data in ALL Seasons to Calculate Seasonal Slopes")
      
      s4 <- s0 + facet_wrap(~Season, scales=SCALES, drop=FALSE)  +   #ggtitle(expression(atop(paste("Overall Thiel Sen Slopes for", param, "at", stnID), 
        #                        atop(italic(paste("Overall Thiel Sen Slope: ", mod$MEDIANSLOPE, "(pval= ", mod$PVAL, ")")),
        #                             italic(paste("Overall Seasonal Thiel Sen Slope: ", mod2$SLOPE, "(pval= ", mod2$PVAL, ")")))))) +
        #plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = -1) +
        ggtitle(paste("Thiel-Sen Slopes by Season for", param, "at", stnID)) + 
        scale_linetype_manual(name="",breaks=c("Censored Data", "Overall Theil-Sen"), values=c("solid", "dashed"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) +
        scale_colour_manual(name="",  breaks=c("Censored Data","Overall Theil-Sen"), values=c("red",  "red"))+#, labels=c("Censored Data", "Overall Thiel-Sen")) + 
        guides(colour = guide_legend(override.aes = list(linetype = c(0,1), shape=c(19, NA)))) +
        theme(legend.position = "top")  +
        xlab(xlabel)
    }
   
  #s5 <- s4 + theme(plot.margin = unit(c(1,1,1,1), "lines"))   # Make room for the group
  #s6 = s5 + geom_text(data=expdf, aes(label = xlabel, x = 1993, y = Inf), hjust = 5)

    print(p7)
    suppressWarnings(print(s4))
 
  #print(arrangeGrob(s4, xlab = textGrob(xlabel, gp=gpar(cex=0.7)), nrow=2, heights=c(1,0.1)))
  #print(arrangeGrob(s4, xlab = textGrob(xlabel, gp=gpar(cex=0.7) )))
  #grid.arrange(plot4, plot1, heights=c(3/4, 1/4), ncol=1, nrow=2)

}
}
