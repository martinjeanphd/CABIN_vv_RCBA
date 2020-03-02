
#####################################################################
#  WRAPPER FUNCTIONS TO CALL SPECIFIC PLOT and PLOT defined number of 
#  plots per page in pdf document
########################################################################


#----------------------------------------------------------------------------
#  FUNCTION TO CALL A SPECIFIC PLOT FUNCTION FOR ALL VARIABLES IN A DATAFRAME
#  SPLIT PLOT BY PARAMETER (1 PARAMETER PER PAGE)
#  - input df should be PowerResults dataframe for all variables 
#----------------------------------------------------------------------------

PowerPlot.WRAP.facet <- function(dat, savename, savedir, pdf.subset, subset.par,
                           facet.par, n.facets.per.page, my.func, arglist=NULL){
  #dat = dataframe for all variables (i.e. list of df --> ldply(df.list))
  #arglist=list(x=1, y=2, z=3) 

if(is.null(pdf.subset)){ln.sub <- 1} else {ln.sub <-length(unique(dat[,eval(pdf.subset)]))}

for (i in 1:ln.sub){
  setwd(savedir)
  if(is.null(pdf.subset)){
    pdf(paste(savename, ".pdf", sep=""),width=11,height=8.5, onefile=TRUE)     
  }else{
  pdf(paste(unique(dat[,eval(pdf.subset)])[i],"_", savename, ".pdf", sep=""),width=11,height=8.5, onefile=TRUE)   
  }
  nm <- unique(dat[,eval(subset.par)])
  LN  <- length(nm)                #total number of subset.pars to be plotted 
  
  k <- 1                           #number of plots per page
  j <- as.integer(ceiling(LN/k))   #number of pages needed for each subset parameter
  
  x <- unique(dat[,eval(facet.par)])   #total number of unique facets parameters
  Lx <- length(x)
  jj <- as.integer(ceiling(Lx/n.facets.per.page))  #number of pages needed for all facets
  
  
  #intiate parameters
  m <- 1   
  q <- 1        
  r <- k
  
  while(m<j+1) {
    #extract subset to use for page
    slice.nm <- nm[q:r]
    slice.nm <- as.vector(na.omit(slice.nm))
    slice.df <-  dat[dat[,eval(subset.par)] %in% slice.nm,]
    
    #intiate parameters
    qq <- 1
    rr <- n.facets.per.page
    mm <- 1
    
    #extract facet subset to use for page
    while (mm < jj+1){
      slice.f <- x[qq:rr]
      slice.f <- as.vector(na.omit(slice.f))
      slice.df2 <- slice.df[slice.df[,eval(facet.par)] %in% slice.f,]
   
    
      #call function for subset  
      if(is.null(arglist)==TRUE) {
        get(my.func)(slice.df2)
      } else {
        do.call(my.func, args=c(list(slice.df2), arglist, subset.par))                                                
      }
      
      mm = mm+1
      qq = qq + n.facets.per.page
      rr = rr + n.facets.per.page
      
    }#end facet for
    
    m=m+1  
    q=q+k
    r=r+k
  }  
  
  dev.off()
}
}#end function
  
  
  #----------------------------------------------------------------------------
  #  FUNCTION TO CALL A SPECIFIC PLOT FUNCTION FOR ALL PARAMETERS (or other
  #  subset.par) IN A DATAFRAME
  #
  #  SPLITS PLOT BY subset.par (ie if subset.par=Parameter --> 1 PARAMETER PER PAGE)
  #  - input df should be PowerResults dataframe for all variables for 1 station
  #----------------------------------------------------------------------------
  
#   PowerPlot.WRAP <- function(dat, savename, savedir, subset.par = "Parameter", n.per.page, my.func, arglist=NULL){
#     #dat = dataframe for all variables (i.e. list of df --> ldply(df.list))
#     #arglist=list(x=1, y=2, z=3) 
#     
#     
#     setwd(savedir)
#     pdf(paste(savename, ".pdf"),width=11,height=8.5, onefile=TRUE)   
#     
#     nm <- unique(dat[,eval(subset.par)])
#     LN  <- length(nm)                #number of parameter to be plotted 
#     
#     k <- n.per.page                  #number of plots per page
#     j <- as.integer(ceiling(LN/k))   #number of pages needed
#     
#     #intiate parameters
#     m <- 1   
#     q <- 1        
#     r <- k
#     
#     while(m<j+1) {
#       #extract subset to use for page
#       slice.nm <- nm[q:r]
#       slice.nm <- as.vector(na.omit(slice.nm))
#       slice.df <-  dat[dat[,eval(subset.par)] %in% slice.nm,]
#       
#       
#       #call function for subset  
#       if(is.null(arglist)==TRUE) {
#         get(my.func)(slice.df)
#       } else {
#         do.call(my.func, args=c(list(slice.df), arglist))                                                
#       }
#       
#       m=m+1  
#       q=q+k
#       r=r+k
#     }  
#     
#     dev.off()
#   }
#   
