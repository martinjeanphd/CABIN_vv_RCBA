#
#
# seaken(x,y,group) w. kslopes removed
# Seasonal Kendall test
# y vs x by group
# calls script kslopes
#
seaken_pwr<-function(y,x,group){

 # skall.time <- Sys.time()
  
  sall<- 0.0
  varall<-0
#   #xxx<-x
#   #yyy<-y
#   #nall<-length(time)
#   denomall<-0
#   xmedian<-median(x)
#   ymedian<-median(y)
#   
  yc<-split(y,group)
  xc<-split (x,group)
  zc<-split (group,group)
  
  k<- 0
#   allslope<-rep(c(0),1)
  
  #skgroup.time <- Sys.time()
  
  
  for(i in 1:length(yc)) {
    #grp<-(zc[[i]]);sea<-(grp[c(1)]);
    
    #kslopes(xc[[i]],yc[[i]],sea);
    
    xx<-xc[[i]]
    yy<-yc[[i]]
#     n<-length(xx)
#     
#    
#     for (j in 1:(n-1)){
#       for (jj in (j+1):n){
#         k<-k+1
#         dx <- (xx[jj] - xx[j])
#         dy <- (yy[jj] - yy[j])
#         allslope[k]<-dy/dx
#       }
#     }

    kenout<-Kendall(xx,yy)
  
    
#   nn<-length(xx)
#      denomall<-denomall+nn*(nn-1)/2
    s<-mean(kenout$S); sall<- sall+s
    var<-mean(kenout$varS); varall<-varall+var
#      tau<-mean(kenout$tau); 
#      pval<-mean(kenout$sl)
#      vars<-mean(kenout$varS)
#      denom<-kenout$D
    
    #RESULTS1<-data.frame(SEASON = sea,S=s, DENOM = denom,VARS=vars,TAU=tau,PVAL=pval)
    #print(RESULTS1);
    
  
  }                             
#    tau_all<- (sall)/denomall
  
  sdall<-sqrt(varall)
  
  zall<- (sall-sign(sall))/sdall
  
  pval_all <- 2*pnorm(-abs(zall))
  
  #medslope<-median(allslope)
  
  #intall<- ymedian - medslope*xmedian
  
  # RESULTS <- data.frame(SALL=sall,DENOM = denomall,VARALL=varall,Z_ALL =zall,
  #                         TAUALL =tau_all,PVAL =pval_all,INT = intall,SLOPE = medslope);
  #  print(RESULTS)
  
 # skall.end<- Sys.time()-skall.time; cat("seaken total time:"); print(skall.end)
  #cat("\n")
  
  return(pval_all)
}


