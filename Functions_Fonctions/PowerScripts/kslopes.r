#kslopes

kslopes<- function(x,y,grp=NULL){{n<-length(x);medx<-median(x);medy<-median(y)}
{slope<-rep(c(0),n*(n-1)/2)}
{k<-0;for(j in 1:(n-1)){{for(i in (j+1):n){k<-k+1;
     dx <-(x[c(i)]-x[c(j)]);
   dy <-(y[c(i)]-y[c(j)]);
    if(abs(dx) >0.0) slope[c(k)]<-dy/dx; }}}}

 {medslop<-median(slope)
     ;#print("results for the SEASON");#print(medslop);
    int<-medy-medslop*medx; #print("intercept");#print(int)
kenout<-Kendall(x,y);
#summary(kenout);
s<-mean(kenout$S);
var<-mean(kenout$varS);
tau<-mean(kenout$tau);pval<-mean(kenout$sl);
vars<-mean(kenout$varS);denom<-kenout$D;
RESULTS1<-data.frame(S=s, DENOM = denom,VARS=vars,TAU=tau,PVAL=pval,INTERCEPT = int, MEDIANSLOPE = medslop)
#print(RESULTS1)

  # RESULTS <- data.frame(INTERCEPT = int, MEDIANSLOPE = medslop)
#print (RESULTS)
}
  }

