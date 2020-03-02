########################################
# Flow vs Concentration Scatterplots
########################################

#---------------------------------------
# Flow vs Concentration (SINGLE PARAMETER)
#---------------------------------------

Scatter.FlowConc <- function(df, data.col, span.value=0.65, log.axis="NO"){
  

  
  if(length(na.omit(df$Flow)) ==0){
    cat("    Cannot Run Flow vs Conc Scatterplot for ",unique(df$Parameter), " - No Flow Data Available.", "\n")
  }else {
  
  cat("Flow vs Conc Scatterplot (Scatter.FlowConc) for ",  unique(df$Parameter),"\n")  
  
  G <- df
  G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
  
  p1 <- ggplot(G, aes(x=Flow, y=datacol))+
    geom_point(aes(color=MonthName))
  
  p2 <- p1 +  stat_smooth(method=loess, span=span.value, alpha=0.1, color="red", aes(fill="a")) +    #defaul in stat_smooth is n=80 (i think span=0.65 would override this)
    stat_smooth(method=lm, color="blue", alpha=0.1, aes(fill="b")) + 
    scale_fill_manual(name="", breaks=c("a", "b"), values=c("red", "blue"),
                      labels=c(paste("lowess (span=", span.value,")"), "linear")) +
    theme_bw() + 
    ylab(paste(unique(G$Parameter), "(", unique(G$Units) , ")")) + 
    xlab("Flow") +
    ggtitle(paste("Flow vs Concentration at",  unique(G$StationID))) 
  
  if (log.axis == "NO") {
    p3 <- p2 
  }else if (log.axis=="YES"){
    p3 <-  p2 + scale_x_log10() + scale_y_log10() 
  }
    
  print(p3)
}
}#end function
  
#---------------------------------------
# Flow vs Concentration (FACET BY PARAMETER)
#---------------------------------------

Scatter.FlowConc.Facet <- function(df, data.col, span.value=0.65, log.axis="NO"){
    
  
  if(length(na.omit(df$Flow)) ==0){
    cat("    Cannot Run Flow vs Conc Scatterplot for ",unique(df$Parameter), " - No Flow Data Available.", "\n")
  }else {
    
    cat("Flow vs Conc Scatterplots (Scatter.FlowConc.Facet)", "\n")  
    
    G <- df
    G$datacol <- G[,grep(paste("^",data.col,"$", sep=""), names(G), value=TRUE)]  
    
    p1 <- ggplot(G, aes(x=Flow, y=datacol)) + geom_point(alpha=0.5)
    #  geom_point(aes(color=MonthName))
    
    p2 <- p1 +  stat_smooth(method=loess, span=span.value, alpha=0.1, color="red", aes(fill="a")) +    #defaul in stat_smooth is n=80 (i think span=0.65 would override this)
      stat_smooth(method=lm, color="blue", alpha=0.1, aes(fill="b")) + 
      scale_fill_manual(name="", breaks=c("a", "b"), values=c("red", "blue"),
                        labels=c(paste("lowess (span=", span.value,")"), "linear")) +
      theme_bw() + 
      ylab(paste(unique(G$Parameter), "(", unique(G$Units) , ")")) + 
      xlab("Flow") +
      ggtitle(paste("Flow vs Concentration at",  unique(G$StationID))) 
    
    
    #facet by parameter
    p3 <- p2 + facet_wrap(~Parameter, scales="free")
    
   
    if (log.axis == "NO") {
      p4 <- p3
    }else if (log.axis=="YES"){
      p4 <-  p3 + scale_x_log10() + scale_y_log10() 
    }
      
      #add legend and format axis
      p5 <- p4 + theme(#axis.text.x  = element_text(angle=90, vjust=0.5),
            legend.position = "top")
      
      suppressWarnings(print(p5))
    }#end function
}
