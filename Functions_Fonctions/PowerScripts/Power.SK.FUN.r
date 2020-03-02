#Power.SK.FUN.R                                                                

#################################################################################
#  Power.SK FUNCTION
#
#  CODE TO COMPUTER POWER FOR EACH LEVEL OF BETA FOR 1 SCENARIO
#################################################################################

#SimPar1= scenarios parameters for 1 scenario

Power.SK <- function(SimPar1, bootRES, scen.ind, sub.pools, k=NULL, model, DS.option, 
                     alpha, I,  SKseason.df){


  pwrSKtime <- Sys.time()
  
  scen.name <- as.character(SimPar1$scenID)
 
  cat("\n"); cat("Running Power Calculations for ", scen.name, "\n")
  
  #extract beta values (slopes) from the sim parameters
  betas <- as.numeric(SimPar1[,grep("^[beta]", names(SimPar1))])

  B <- length(betas)

 #convert bootRES to a matrix with innovations only 
 #(i.e. removed Sim.Data, Sim.Mth, t.index, Sim.sp, SceanrioID and Parameter columns)
 boot.res <- as.matrix(bootRES[,1:I])
 J <- ncol(boot.res)  #should=I
  
  
  #-----------------------------------------------------------  
  # EXTRACT t.index as time vector t
  #-----------------------------------------------------------  
 
  scen.ind$Sim.Yr <- year(scen.ind$Sim.Date)
  t <- round(scen.ind$t.index - (scen.ind$Sim.Yr[1]-1), digits=3)
  #because t begins in start_year (e.g. 2012) and not 1
 
  nr <- length(t)  #number of rows (should =nrow(bootres1)) 
 
  
  #-----------------------------------------------------------  
  # 
  #-----------------------------------------------------------  
  
  #Create df (added so we could analyze SK by season instead of month)
   SKseas.index <- SKseason.df[,names(SKseason.df)== "SKgroup"]
   month.index <- SKseason.df[,names(SKseason.df)== "Mth"]
   seas.tmp <- SKseas.index[match(scen.ind$Sim.Mth, month.index)]
   #seas <- as.factor(seas.tmp)

  tmp.sim <- data.frame(t, Mth=scen.ind$Sim.Mth, SKgroup=seas.tmp)
  
  #-----------------------------------------------------------  
  # initiate a vector to hold predicted simulated data values
  #-----------------------------------------------------------  

  simpred <- as.numeric(rep(NA, nr))
  
  #-----------------------------------------------------------  
  # For Fourier initiate a df for data, sin, cos values for simdata
  #-----------------------------------------------------------  
  
  #create the sin/cos matrix
  if (DS.option == "Fourier") {
     
     #add all sin/cos terms as columns to d.sim
     cos <- matrix(ncol=k, nrow=length(t))
     sin <- matrix(ncol=k, nrow=length(t))
     m=1
     
     while (m<=k){          #k=num.harmonics in original model
       cos[,m] <- cos(2*pi*m*t)
       sin[,m] <- sin(2*pi*m*t)
       m=m+1
     }
     
     colnames(cos) <- paste("cost",1:ncol(cos), sep="")
     colnames(sin) <- paste("sint",1:ncol(sin), sep="") 
 
 #initiate the d.sim df with k*2+1 columns (data.col, sin/cos matrix values) 
 # and t rows
  d.sim <- data.frame(data.col=as.numeric(matrix(nrow=nr)), cos, sin)
}
  
 
 
  #==========================================================================  
  # For each beta value, calculate SK for each I set of bootstrap residuals
  #==========================================================================  

  d.sim.beta <- list()
  
  simdata <- matrix(nrow=nr, ncol=I)
  
  #-----------------------------
  # FOR EACH LEVEL OF TREND:
  #----------------------------
  for (i in 1:B) {
    
    cat("     beta = ", betas[i])#, "\n")
       
    # ------
    # A) ADD TREND to bootstrapped residuals
    # ------
    
    simdata <- betas[i] * t + boot.res
    
    #----------------------------
    # FOR EACH SET OF BOOTSTRAPED RESIDUALS (should = I)
    #----------------------------
    betatime <- Sys.time()
    for (j in 1:J) {
        
      # ------
      # B) ADD SEASONALITY COMPONENT to trend + bootstrapped residuals
      # ------
 
      #********* FOURIER OPTION *********
      if (DS.option == "Fourier") {
        
          #the first column of d.sim = simdata (other column =sin/cos terms)
          #this specifies the first place to look for explanotry variables to be used for prediction
          d.sim[,1] <- simdata[,j]
        
          #evaluate the regression model on newdata (resampled residuals + trend)
          PRED = predict(model, newdata=d.sim)
        
          #simpred <- PRED + d.sim$data.col
           simpred <- PRED + d.sim[,1]
          
      #********* NO SEASONALITY OPTION *********    
      }else if (DS.option == "None") {
      
        simpred <- simdata[,j]
      
      #********* SUBTRACT SEASONAL MEDIANS OPTION *********
      }else if (DS.option == "Subtract.Medians") {
        
        
        SM.index <- model.DTDS$SM
        seas1.index <- as.numeric(model.DTDS$Season)
        SM.tmp <- SM.index[match(scen.ind$Sim.Mth, seas1.index)]
        
        simpred <- simdata[,j] + SM.tmp
      }
      
      # ------
      # E) Add d.sim df to list of d.sim.beta  
      # ------

      d.sim.beta[[(i-1)*J+j]] <- simpred
      
    }# end for J (I)
    cat("  "); print(Sys.time()-betatime)
}#end for i (beta)

   cat("d.sim.beta time:"); print(Sys.time()-pwrSKtime)
  #OUTPUT = d.sim.beta = list of df? simpred values for each set of boot.res residuals
  #assume I=10 (= 10 columns in boot.res) -->  10*(# beta levels) list items)
  #ex: for trend 1 (beta1) --> d.sim.beta values are in list (0*10+1+j) = 1-10
  #    for trend 2 --> d.sim.beta values are in df (1*10+j) = 11-20
  
  #-----------------------------------------------------------  
  # APPLY TO SEAKEN FUNCTION to each df in d.sim.beta
  #-----------------------------------------------------------
  cat("     Calculating SK Results for all beta", "\n")

  pvaltime <- Sys.time()
  
  #tmppval <- sapply(d.sim.beta, seaken_pwrSK, y=t, group=bootRES$Sim.Mth) 
  #tmppval <- laply(d.sim.beta, seaken_pwr, x=t, group=bootRES$Sim.Mth)    
  tmppval <- laply(d.sim.beta, seaken_pwr, x=t, group=tmp.sim$SKgroup)  
 
#   tmphold <- matrix(ncol=B ,nrow=J, byrow = FALSE)
#   for (i in 1:length(d.sim.beta)){
#     pp <- seaken_pwr(d.sim.beta[[i]], x=t,  group=bootRES$Sim.Mth)    
#     tmphold[i] <- pp
#   }
    
  
    #tmppval = vector of pvalue results from list of d.sim.beta df
    # 1 value in vector for each d.sim.beta df
   cat("tmpval.time all:"); print(Sys.time()-pvaltime)

  tmppval.mat <- matrix(tmppval, nrow=J)        #columns = level of beta
    #rows = I (number of set of bootstrap residuals)
    #column will represent each level of trend
  
  
 #create a matrix where 1 = SIGNIFICANT SK, 0= NS SK
  #reject if pvalue <= alpha ----> SIGNIFICANT TREND
  tmpreject.SK <- matrix(rep(NaN), nrow=nrow(tmppval.mat), ncol=ncol(tmppval.mat))
  
  for (g in 1:ncol(tmppval.mat)){
    tmpreject.SK[,g] <- ifelse(tmppval.mat[,g] <= alpha, 1, 0)
  }

  #calculate power = sum of each column (i.e. #significant trends)/ Total # Simulated Datasets
  power.SK <- apply(tmpreject.SK,2,sum)/J    #vector of power results - 1 value per trend level 

  cat("Total Power.SK time for scenario:  "); print(Sys.time()- pwrSKtime)
  
 return(power.SK)
}