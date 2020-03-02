#input dataset = origdata (Data,obs,sen, Date, dates.dec, Season....)

###################################################################
#  DETREND a DATASET USING LOWESS
###################################################################

#DT.loess <- function(df,data.col="Data", span=0.65,degree=2, family="symmetric", flowYN=c("YES,"NO")){
DT.loess <- function(df,datcol, l.span,l.degree, l.family, flowYN){
  
  tmp <- df
  #names(tmp) <- gsub(grep(data.col, names(tmp), value=TRUE), "data.col", names(tmp))
  
  #ADD DT.col COLUMN which is identical to the col identified in data.col in function def 
  tmp$datacol <- tmp[,grep(paste("^",datcol,"$", sep=""), names(tmp), value=TRUE)]   
  
  if(flowYN=="NO") {
    G.loess <- loess(datacol~dates.dec, data=tmp, span=l.span, degree=l.degree, na.action=na.exclude, family=l.family)
    tmp$DTmethod = "Lowess"
  }else if(flowYN=="YES"){
    G.loess <- loess(datacol~Flow+dates.dec, data=tmp, span=l.span, degree=l.degree, na.action=na.exclude, family=l.family) 
    tmp$DTmethod = "Lowess + Flow"
  }
  tmp$DT.resid <- G.loess$residuals      #add DT to original dataframe --> this line won't work if MV?
  tmp$DT.fit <- G.loess$fitted
  
  out <- list(DT1=tmp, model.DT1 = G.loess)
    
  return(out)
}#end function



###################################################################
#  DETREND a DATASET USING SEASONAL KENDALL
###################################################################

DT.SK <- function(df,datcol){
  
  tmp <- df
 
  #ADD DT.col COLUMN which is identical to the col identified in data.col in function def 
  tmp$datacol <- tmp[,grep(paste("^",datcol,"$", sep=""), names(tmp), value=TRUE)]   
  
  s <- with(tmp, sea.senth(x=dates.dec,y=datacol,group=Season))
  slope <- s$SLOPE
  
  tmp$DTmethod = "SenSlope"
  
  tmp$DT.fit <- slope*tmp$dates.dec + s$INT
  tmp$DT.resid <- tmp$datacol - tmp$DT.fit
    
  out <- list(DT1=tmp, model.DT1 = s)
  
  return(out)
}#end function



###################################################################
#  DESEASONALIZE A DATASET USING FOURIER (LM on Sin/Cos)
###################################################################

# The above assumes you are using decimal time.  
# If you use months (1,2, … 12) as the time variable you would modify the sine and cosine terms as:
#  sintk = sin(2*pi*k*month/12)
#  costk = cos(2*pi*k*month/12)

#DS.fourier <- function (df,data.col="Data", k=6){
DS.fourier <- function (df,datcol, k){
  
  tmp <- df
  
  time=df$dates.dec 
  
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  tmp$datacol <- tmp[,grep(paste("^",datcol,"$", sep=""), names(tmp), value=TRUE)]   
  
  #create df with data.col (data to fit model to) and all sin & cos term values (e.g. cos(2*pi*k*time))
  df.tmp <- data.frame(datacol=tmp$datacol)
  
  m=1
  z=2
  while (m<=k){      		# change 1:6 to match your nc=k
    cos.tmp <- cos(2*pi*m*time)
    sin.tmp <- sin(2*pi*m*time)
    df.tmp <- cbind(df.tmp, cos.tmp, sin.tmp)
    names(df.tmp)[z] <- paste("cost",m, sep="")
    names(df.tmp)[z+1] <- paste("sint",m, sep="")
    m=m+1
    z=z+2
  }
  
  # Fit harmonics to data.col 
  xx1 <- paste(names(df.tmp[2:((2*k)+1)]), collapse="+")   #should this as grep?    
  xx.formula<- paste("datacol~",xx1, sep="")
  
  LM.3 <- lm(as.formula(xx.formula), data=df.tmp, na.action=na.exclude)
  
  
  tmp$DS.resid <- LM.3$residuals      #add DT to original dataframe --> this line won't work if MV
  tmp$DS.fit <- LM.3$fitted
  tmp$DSmethod = "Fourier"
  tmp$harmonics = k
  
  out <- list(DS1=tmp, model.DS1 = LM.3, DS1.formula=xx.formula)
  
  return(out)
}#end function


###################################################################
#  DESEASONALIZE A DATASET BY SUBTRACTING SEASONAL MEDIANS
###################################################################


DS.Medians <- function(df,datcol){
  
  tmp <- df
  
  #ADD data.col COLUMN which is identical to the col identified in data.col in function def 
  tmp$datacol <- tmp[,grep(paste("^",datcol,"$", sep=""), names(tmp), value=TRUE)]   
  
  #calculate seasonal medians
  SM<- by(tmp$datacol, tmp$Season, median, na.rm=T)
  seas.medians <- data.frame(cbind(SM = as.numeric(SM), Season = dimnames(SM)[[1]]), stringsAsFactors=FALSE)
  seas.medians$SM <- as.numeric(seas.medians$SM)
  
  tmp$SM2 <- seas.medians$SM[match(tmp$Season, seas.medians$Season)]
  tmp$DS.resid = tmp[,"datacol"] - tmp[,"SM2"]
  tmp$DS.fit <- tmp$SM2
  tmp$DSmethod = "SeasMed"
  tmp$harmonics = NA
  tmp <- tmp[,!names(tmp) %in% c("SM2")]
  
  #tmp$SM <- seas.medians$SM[match(tmp$Season, seas.medians$Season)] 
  
  out <- list(DS2=tmp, model.DS2 = seas.medians, DS1.formula=NULL)

  return(out)
}



###################################################################
#  DT AND DS USING NONPARAMETRIC MODEL (ISABELLAS CODE) 
###################################################################







