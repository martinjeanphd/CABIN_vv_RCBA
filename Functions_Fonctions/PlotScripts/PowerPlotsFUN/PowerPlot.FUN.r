##########################################################
# FUNCTION FOR CREATING POWER CURVE PLOTS
##########################################################

#test variables:
#   df <- SK.data
#   x.val <- "TPC"
#   #group.vars <- c("freqName", "NSimYrs") 
#   facet.col <- c("Parameter")
#   #facet.row <- c("Scenario")
#   color.var <- c("NSimYrs")
#   line.var <- c("freqName")
#   shape.var <- c("freqName")


#========================================================
# FUNCTION TO PLOT POWER CURVE (FACET GRID)
#========================================================

PwrCurve.grid <- function(df, x.val=c("TPC", "APC", "beta"), 
                            group.vars=NULL, facet.col=NULL, facet.row=NULL, 
                            color.var=NULL, line.var=NULL, shape.var=NULL,  
                            SCALES="fixed",subset.par) {
  
  #do I need to make variables factors?
  #df$TPC <- factor(df$TPC)
  df$NSimYrs <- factor(df$NSimYrs)
  df$alpha <- factor(df$alpha)
 
  #set up variables
  cl=NULL; sh=NULL; ln=NULL; grp=NULL; 
  if (is.null(facet.row)) {fr <- "."} else {fr <- facet.row }
  if (is.null(facet.col)) {fc<- "."} else {fc <- facet.col }
  
  fc  <- paste("~", paste(fc, collapse = '+'), sep="")
  fr <- paste(fr, collapse = '+')
  facet1 <- paste(fr, fc, sep="")
  
  if (length(group.vars) > 1){
    tmp <- paste(group.vars, collapse = ',')
    grp <- paste("interaction(", tmp, ")", sep="") 
  } else grp <- group.vars
  
  if(length(color.var)==1 && color.var=="group.vars"){cl <- grp}
  if(length(line.var)==1 && line.var=="group.vars"){ln <- grp}
  if(length(shape.var)==1 && shape.var=="group.vars"){sh <- grp}
  
  z <- data.frame(vars=c("color.var", "line.var", "shape.var"),
                  vn = c("cl", "ln", "sh"), stringsAsFactors=FALSE)

  for (i in 1:nrow(z)){
     lz <- length(get(z$vars[i]))
     zz <- z$vars[i]
     zn <- z$vn[i]

     if (lz >1 && get(zz) != "group.vars"){   
        tmp <- paste("interaction(", paste(get(zz), collapse = ','), ")", sep="")
        assign(zn, get(tmp))   
      }else if (lz ==1 && get(zz) != "group.vars"){
        assign(zn, get(zz)) #what environment is this going to?
      }
  }
  
  
  if(x.val=="TPC"){ 
    x.lab="Trend as: Total Percent Change over Record"
  }else if (x.val=="APC"){ 
    x.lab="Trend as: Annual Percent Change"
  }else if (x.val=="beta"){
      if(length(unique(df$Units)) > 1){
        x.lab=paste("Trend as: Change in concentration (units may vary))", sep="") 
      }else x.lab=paste("Trend as: Change in concentration (",unique(df$Units),")", sep="") 
  }
  
  #SET THE THEME TO BW GLOBALLY: (all plots will be drawn with this)
  theme_set(theme_bw())
  #create default theme

   #set up basic plot
    p <- ggplot(data=df, aes_string(x=x.val, y="power", group= grp, color=cl)) + 
          geom_point( aes_string(shape=sh), size=3) + 
          geom_line( aes_string(linetype=ln))

    #group.vars <- "Scenario"
    #group.vars <- c("freqName", "NSimYrs")
    #facet by: 
    #p1 <- p + facet_wrap(as.formula(facet1), ncol=2)
    p1 <- p + facet_grid(as.formula(facet1), scales=SCALES)
 
    #examples of adding axis titles
    p2 <- p1 + ylab("Power (%)") + xlab(x.lab) +
      ggtitle(paste("Power Curves for ", unique(df[,eval(subset.par)])))
  
    #format lengend
    p3 <- p2 + guides(colour = guide_legend(override.aes = list(shape = NA)))
    
    #format axis 
    #set breaks
    if(x.val == "beta"){
      p4 <- p3 +scale_y_continuous(breaks=seq(0,1, by=0.1), labels=seq(0,100, by=10))
    }else {
      brks <- df$TPC
    
      p4 <- p3 +scale_y_continuous(breaks=seq(0,1, by=0.1), labels=seq(0,100, by=10)) +
          scale_x_continuous(breaks=brks)
    }
  
  print(p4)
  }
 # dev.off()

#ex of using function to define breaks
#scale_x_continuous(breaks = myBreaks,labels = percent_format()) 
#myBreaks <- function(x){
#  breaks <- c(min(x),median(x),max(x))
#  names(breaks) <- attr(breaks,"labels")
#  breaks
#}


#========================================================
# FUNCTION TO PLOT POWER CURVE (FACET WRAP)
#========================================================

PwrCurve.wrap <- function(df, x.val=c("TPC", "APC", "beta"), 
                          group.vars=NULL, facet.col=NULL, facet.row=NULL, 
                          color.var=NULL, line.var=NULL, shape.var=NULL, NCOL=2, 
                          SCALES="free" ,subset.par) {
  
  #do I need to make variables factors?
  #df$TPC <- factor(df$TPC)
  df$NSimYrs <- factor(df$NSimYrs)
  df$alpha <- factor(df$alpha)
  
  #set up variables
  cl=NULL; sh=NULL; ln=NULL; grp=NULL; 
  if (is.null(facet.row)) {fr <- ""} else {fr <- facet.row }
  if (is.null(facet.col)) {fc<- ""} else {fc <- facet.col }
  
  fc  <- paste("~", paste(fc, collapse = '+'), sep="")
  fr <- paste(fr, collapse = '+')
  facet1 <- paste(fr, fc, sep="")
  
  if (length(group.vars) > 1){
    tmp <- paste(group.vars, collapse = ',')
    grp <- paste("interaction(", tmp, ")", sep="") 
  } else grp <- group.vars
  
  if(length(color.var)==1 && color.var=="group.vars"){cl <- grp}
  if(length(line.var)==1 && line.var=="group.vars"){ln <- grp}
  if(length(shape.var)==1 && shape.var=="group.vars"){sh <- grp}
  
  z <- data.frame(vars=c("color.var", "line.var", "shape.var"),
                  vn = c("cl", "ln", "sh"), stringsAsFactors=FALSE)
  
  for (i in 1:nrow(z)){
    lz <- length(get(z$vars[i]))
    zz <- z$vars[i]
    zn <- z$vn[i]
    
    if (lz >1 && get(zz) != "group.vars"){   
      tmp <- paste("interaction(", paste(get(zz), collapse = ','), ")", sep="")
      assign(zn, get(tmp))   
    }else if (lz ==1 && get(zz) != "group.vars"){
      assign(zn, get(zz)) #what environment is this going to?
    }
  }
  
  
  if(x.val=="TPC"){ 
    x.lab="Trend as: Total Percent Change over Record"
  }else if (x.val=="APC"){ 
    x.lab="Trend as: Annual Percent Change"
  }else if (x.val=="beta"){
    if(length(unique(df$Units)) > 1){
      x.lab=paste("Trend as: Change in concentration (units may vary))", sep="") 
    }else x.lab=paste("Trend as: Change in concentration (",unique(df$Units),")", sep="") 
  }
  
  #SET THE THEME TO BW GLOBALLY: (all plots will be drawn with this)
  theme_set(theme_bw())
  #create default theme
  
  #set up basic plot
  p <- ggplot(data=df, aes_string(x=x.val, y="power", group= grp, color=cl)) + 
    geom_point( aes_string(shape=sh), size=3) + 
    geom_line( aes_string(linetype=ln))
  
  #group.vars <- "Scenario"
  #group.vars <- c("freqName", "NSimYrs")
  #facet by: 
  #p1 <- p + facet_wrap(as.formula(facet1), ncol=2)
  p1 <- p + facet_wrap(as.formula(facet1), ncol=NCOL, scales=SCALES)
  
  #examples of adding axis titles
  p2 <- p1 + ylab("Power (%)") + xlab(x.lab) +
    ggtitle(paste("Power Curves for ",unique(df[,eval(subset.par)])))
  
  #format lengend
  p3 <- p2 + guides(colour = guide_legend(override.aes = list(shape = NA)))
  
  #format axis 
  #set breaks
  if(x.val == "beta"){
    p4 <- p3 +scale_y_continuous(breaks=seq(0,1, by=0.1), labels=seq(0,100, by=10))
  }else {
    brks <- df$TPC
    
    p4 <- p3 +scale_y_continuous(breaks=seq(0,1, by=0.1), labels=seq(0,100, by=10)) +
      scale_x_continuous(breaks=brks)
  }
  
  print(p4)
}
#dev.off()

#ex of using function to define breaks
#scale_x_continuous(breaks = myBreaks,labels = percent_format()) 
#myBreaks <- function(x){
#  breaks <- c(min(x),median(x),max(x))
#  names(breaks) <- attr(breaks,"labels")
#  breaks
#}


