# senth(x,y)  from Dennis Helsel.
#########################################################################
# Computes intercept,slope,S,tau,pvalue for Kendall/Sen/Theil line
# Requires package Kendall
# y = response variable
# x = explanatory variable
#########################################################################

senth<- function(x,y){
  n<-length(x)
  medx<-median(x)
  medy<-median(y)
  
  slope<-rep(c(0),n*(n-1)/2)
  
  k<-0
  
  for(j in 1:(n-1)){
    for(i in (j+1):n){
      k<-k+1;
      dx <-(x[c(i)]-x[c(j)]);
      dy <-(y[c(i)]-y[c(j)]);
      if(abs(dx) >0.0) slope[c(k)]<-dy/dx 
    }}
                      
  medslop<-median(slope)
  #print("results for the SEASON");#print(medslop);
  int<-medy-medslop*medx
  #print("intercept");#print(int)
  kenout<-Kendall(x,y)
 #summary(kenout)
 s<-mean(kenout$S)
 var<-mean(kenout$varS)
 tau<-mean(kenout$tau)
 pval<-mean(kenout$sl)
 vars<-mean(kenout$varS)
 denom<-kenout$D
  
 RESULTS1<-data.frame(S=s, DENOM = denom,VARS=vars,TAU=tau,PVAL=pval,INTERCEPT = int, MEDIANSLOPE = medslop)
 #print(RESULTS1)
 
 RESULTS <- data.frame(INTERCEPT = int, MEDIANSLOPE = medslop, PVAL=pval)
 #print (RESULTS)
 
# plot(y~x,main="Sen Theil is BLUE  Least Squares is RED dashed")
# abline(int,medslop,col = "blue",lwd = 3)
# abline(lsfit(x,y),lty = 3,col= "red",lwd = 3)

}

#########################################################################
# Computes Seasonal Kendall and Thiel-Sen line (y vs x by group)
# Requires package Kendall and kslopes
# y = response variable
# x = explanatory variable
# group
#########################################################################

sea.senth<-function(x,y,group){
  
  sall<- 0.0
  varall<-0
  xxx<-x
  yyy<-y
  nall<-length(x)  #changed from nall<-length(time) b/c no time variable
  denomall<-0
  xmedian<-median(x)
  ymedian<-median(y)
         
  yc<-split(y,group)
  xc<-split(x,group)
  zc<-split(group,group)
  #print(xyplot(y ~ x | group))
  
  k<- 0
  allslope<-rep(c(0),1)
  
  for(i in 1:length(yc)) {
    grp<-(zc[[i]])
    sea<-(grp[c(1)])
    #print("===============================")
    # print("results for season");#print(sea);
    #RESULTS <- data.frame(SEASON = sea)
    #print (RESULTS)
                             
    #xyplot(yc[[i]]~xc[[i]]|sea)
    #kslopes(xc[[i]],yc[[i]],sea)   #this will print out results for each season
    n<-length(xc[[i]])
    x<-xc[[i]]
    y<-yc[[i]]

    for (j in 1:(n-1)){
      for (jj in (j+1):n){
        k<-k+1
        dx <- (x[c(jj)] - x[c(j)])
        dy <- (y[c(jj)] - y[c(j)]);
        allslope[c(k)]<-dy/dx
      }}
      
    kenout<-Kendall(xc[[i]],yc[[i]])
                         
    #summary(kenout);
    nn<-length(xc[[i]])
    denomall<-denomall+nn*(nn-1)/2
    s<-mean(kenout$S)
    sall<- sall+s
    var<-mean(kenout$varS)
    varall<-varall+var
    tau<-mean(kenout$tau)
    pval<-mean(kenout$sl)
    vars<-mean(kenout$varS)
    denom<-kenout$D;
    RESULTS1<-data.frame(SEASON = sea,S=s, DENOM = denom,VARS=vars,TAU=tau,PVAL=pval)
    #print(RESULTS1);
    }
    #print("===================");
    #print("===================");
    #print("Results for pooled season slopes");
    #print("sall"); #print(sall);
    #print("denomall"); #print(denomall);
    tau_all<- (sall)/denomall
    #print("tauall");#print(tau_all);
    #print("varall");#print(varall);
    sdall<-sqrt(varall)
    #print ("sdall"); #print(sdall);
    zall<- (sall-sign(sall))/sdall
    #print("zall");#print(zall)
    pval_all <- 2*pnorm(-abs(zall))
    #print ("2-sided PVAL_ALL"); #print(pval_all)
    medslope<-median(allslope)
    #print("overall median slope");#print(medslope);
    intall<- ymedian - medslope*xmedian
    #print("overall intercept");#print(intall)
    RESULTS <- data.frame(SALL=sall,DENOM = denomall,VARALL=varall,Z_ALL =zall,
                          TAUALL =tau_all,PVAL =pval_all,INT = intall,SLOPE = medslope)
  
    return(RESULTS)
    #print(RESULTS)
    #plot(yyy~xxx, ylab = "RESPONSE",xlab ="Explanatory",main = " Overall Sen-Theil Line")
    #abline(intall,medslope,lwd=3,col = "blue")
}

