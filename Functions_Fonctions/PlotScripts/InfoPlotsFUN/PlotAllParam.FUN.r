
#####################################################################
#  FUNCTION TO CALL A SPECIFIC PLOT FUNCTION FOR ALL VARIABLES IN A DATAFRAME
#  - input df should be dataframe for all variables (i.e. list of df --> ldply(df.list))
########################################################################

#AllParam.Plots(dat, plotname="TESTALLPARAMPLOT", n.per.page=1, my.func="AllFreq.Plots")

AllParam.Plots <- function(dat, savename, n.per.page, my.func, arglist=NULL){
#dat = dataframe for all variables (i.e. list of df --> ldply(df.list))
#arglist=list(x=1, y=2, z=3) 

  pdf(paste(savename, "_", unique(dat$StationID), ".pdf"),width=11,height=8.5, onefile=TRUE)   

nm <- unique(dat$Parameter)
LN  <- length(nm)                #number of parameter to be plotted 
k <- n.per.page                  #number of plots per page
j <- as.integer(ceiling(LN/k))   #number of pages/loops needed

#intiate parameters
m <- 1   
q <- 1        
r <- k

while(m<j+1) {
  #extract subset to use for page
  slice.nm <- nm[q:r]
  slice.nm <- as.vector(na.omit(slice.nm))
  slice.df <- dat[dat$Parameter %in% slice.nm,]
  
  #call function for subset  
   if(is.null(arglist)==TRUE) {
     get(my.func)(slice.df)
     } else {
#       get(my.func)(slice.df, al
     do.call(my.func, args=c(list(slice.df), arglist))                                                
    }
  
  m=m+1  
  q=q+k
  r=r+k
}  

dev.off()
}




#####################################################################
#  FUNCTION TO CALL A LIST OF PLOTS FOR ONE VARIABLE
#  - creates 1 pdf for each parameter 
#  - input df should be dataframe for all variables (i.e. list of df --> ldply(df.list))
########################################################################

OneParam.Plots <- function(dat, savename, fun.list){
  #dat = dataframe for all variables (i.e. list of df --> ldply(df.list))
  #arglist=list(x=1, y=2, z=3) 
 
  for (i in 1:length(unique(dat$Parameter))){
    
    cat("Running Plots for ",unique(dat$Parameter)[i], "\n")

    #extract subset to use for first pdf
    slice.df <- dat[dat$Parameter == unique(dat$Parameter)[i],]
    
    #create the pdf for the parameter
    pdf(paste(savename, "_", unique(   slice.df $Parameter), "_", unique(slice.df $StationID), ".pdf"),
        width=11,height=8.5, onefile=TRUE)   
  
    #call a list of functions to run
    for (j in 1:length(fun.list)){
      
      #select function and argument
      my.func <- fun.list[[j]]$myfun
      arglist <- fun.list[[j]]$arglist
      
      #run function
      if(is.null(arglist)==TRUE) {
        get(my.func)(slice.df)
      } else {
        # get(my.func)(slice.df, al
        do.call(my.func, args=c(list(slice.df), arglist))                                                
      }
    }#end for each functiond
  cat("\n")
  dev.off()
  } #end for each parameter
  
}#END FUNCTION
