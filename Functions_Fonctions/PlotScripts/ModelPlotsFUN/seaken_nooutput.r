#seaken_noout                                     #seaken_noout

################################################################
# ED GILROY's Seaken function with all print output removed
#
# seaken(x,y,group)
# Seasonal Kendall test
# y vs x by group
# calls script kslopes
#################################################################

seaken_noout<-function(y,x,group){
  
#added so that could just call a single data frame with t.index, Data and Season
#x <- df$t.index
#y <- df$Data
#group <- df$Season

  ##original code
  sall<- 0.0
  varall<-0
  #xxx<-x
  #yyy<-y
  nall<-length(time)
  denomall<-0
  xmedian<-median(x)
  ymedian<-median(y)
  #print(xmedian)
  #print(ymedian)
  yc<-split(y,group)
   xc<-split (x,group)
   zc<-split (group,group)
  #print(xyplot(y ~ x | group))     #requires lattice installed
   k<- 0
  
  allslope<-rep(c(0),1)
  
  for(i in 1:length(yc)) {
    grp<-(zc[[i]]); sea<-(grp[c(1)]);
    #print("===============================")
     # print("results for season");#print(sea);
    #RESULTS <- data.frame(SEASON = sea)
    #print (RESULTS)
  
    #xyplot(yc[[i]]~xc[[i]]|sea)
    #kslopes(xc[[i]],yc[[i]],sea);
    n<-length(xc[[i]]);
    x<-xc[[i]];
    y<-yc[[i]];
    
    for (j in 1:(n-1)){
      for (jj in (j+1):n) {
        k<-k+1;
        dx <- (x[c(jj)] - x[c(j)]);
        dy <- (y[c(jj)] - y[c(j)]);
        allslope[c(k)]<-dy/dx
      }
    }
    
    kenout<-Kendall(xc[[i]],yc[[i]]);
    
  #summary(kenout);
  nn<-length(xc[[i]]);
  denomall<-denomall+nn*(nn-1)/2;
  s<-mean(kenout$S);sall<- sall+s;
  var<-mean(kenout$varS);varall<-varall+var;
  tau<-mean(kenout$tau);pval<-mean(kenout$sl);
  vars<-mean(kenout$varS);denom<-kenout$D;
  RESULTS1<-data.frame(SEASON = sea,S=s, DENOM = denom,VARS=vars,TAU=tau,PVAL=pval)
  #print(RESULTS1);
  }
  #print("===================");
  #print("===================");
  #print("Results for pooled season slopes");
  #print("sall"); #print(sall);
  #print("denomall"); #print(denomall);
  tau_all<- (sall)/denomall;
  #print("tauall");#print(tau_all);
  #print("varall");#print(varall);
  sdall<-sqrt(varall);
  #print ("sdall"); #print(sdall);
  zall<- (sall-sign(sall))/sdall;
  #print("zall");#print(zall)
  pval_all <- 2*pnorm(-abs(zall));
  #print ("2-sided PVAL_ALL"); #print(pval_all)
  medslope<-median(allslope);
  #print("overall median slope");#print(medslope);
  intall<- ymedian - medslope*xmedian;
  #print("overall intercept");#print(intall)
  RESULTS <- data.frame(SALL=sall,DENOM = denomall,VARALL=varall,Z_ALL =zall,
  TAUALL =tau_all,PVAL =pval_all,INT = intall,SLOPE = medslope);
  #print(RESULTS)
  #plot(yyy~xxx, ylab = "RESPONSE",xlab ="Explanatory",main = " Overall Sen-Theil Line")
  #abline(intall,medslope,lwd=3,col = "blue")
  
  return(RESULTS)
  }
  
  
