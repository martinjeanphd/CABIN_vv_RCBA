##############################################################
# Code to plot trend for 
##############################################################

dat <- all.DTDS

#SET THE THEME TO BW GLOBALLY: (all plots will be drawn with this)
theme_set(theme_bw())

#-----------------------------------------------
# LOWESS FACETTED BY PARAMETER
#-----------------------------------------------

p <- ggplot(data=dat, aes(x=dates.dec, y=datacol)) 

p1 <- p +geom_point() + geom_line(linetype="dashed")

p2 <- p1 +  geom_line(aes(y=DT.fit), color="red") +
  ylab("Concentration") + xlab("")

p3 <- p2 + facet_wrap(~Parameter, scales="free_y")


#----
p4 <- p + geom_line(aes(y=DT.fit), color="red") +
      ylab("Concentration") + xlab("")

p5 <- p4 + facet_wrap(~Parameter, scales="free_y")

#-----------------------------------------------
# SEASONAL COMPONENT FACETTED BY PARAMETER
#-----------------------------------------------

p <- ggplot(data=dat, aes(x=dates.dec, y=DT)) 

p1 <- p +geom_point() + geom_line(linetype="dashed")

p2 <- p1 +  geom_line(aes(y=DTDS.fit), color="red") +
  ylab("DT Residuals") + xlab("")

p3 <- p2 + facet_wrap(~Parameter, scales="free_y")

#----
p4 <- p + geom_line(aes(y=DTDS.fit), color="red") +
  ylab("DT Residuals") + xlab("")

p5 <- p4 + facet_wrap(~Parameter, scales="free_y")