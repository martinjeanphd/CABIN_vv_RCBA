#create.t.scenarios                         #create.t.scenarios

#####################################################################################
#FUNCTION TO CREATE time index for simulated data record (to define pts simulated data record)
#input df requires (NSimYears, NSimSeasons, SampleMonths, SimDays, Nperseas)
#####################################################################################

create.t.scenarios <-function(df, start_date, sub.pools) {
  #df <- subset(s, scenID == "monthly_5yr_sp1")
  #require(lubridate)
  
  NYrs=df$NSimYears
 
  tmp <- df[,names(df) %in% c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug","Sep", "Oct","Nov","Dec")]
  SampleMonths <- data.frame(Mth=rep(1:12), YM=c(t(tmp)))
                     
  freq=df$freq.option
  
  sp.option=df$sp.option
  
  INT <- df$Interval
  
  
  
  #-----------------------
  #create the time.index
  #-----------------------
    start1 <- ymd(start_date)      #POSIXct[1:1], format: "2012-01-01"  ([1] "2012-01-01 UTC"
    
    end1 <- start1 + years(NYrs) -days(1)  # POSIXct[1:1], format:  "2016-12-31"   
    
    if (freq=="wk.int"){
        span <- weeks(INT)            #period class:   [1] "7d 0H 0M 0S"
      }else if (freq=="mth.int") {
        span <- months(INT)            #period class:   [1] "1m 0d 0H 0M 0S"
      }else if (freq=="yr.int") { 
        span <- years(INT)             #period class:   [1] "1y 0m 0d 0H 0M 0S"
    }#end ifs

  
    
    #REMOVE MONTHS NOT INCLUDED IN THE SampleMonths
    #create a vector with Months to include
    incl1 <- c(subset(SampleMonths, YM==1)$Mth)  
    
    #first month in SampleMonths
    m1 <- incl1[1]
    
    #first year
    y1 <- year(start1)
    
    #first day: 
    d1 <- df$sday
  
    #new start date
    newdate <- paste(y1, "-", m1, "-", d1, sep="")
    start2 <- ymd(newdate)
  
  
    t <- new_interval(start2, end1)
    tt <- t %/% span
    
    Sim.Date <- start2 + c(0:tt) * span

    
    #combine into dataframe
    #xx <- data.frame(Sim.Date, Sim.Day=day(Sim.Date), Sim.Mth=month(Sim.Date), Sim.Yr=year(Sim.Date))
    #xx$Sim.Date <- as.Date(Sim.Date)
    scen.I <- data.frame(Sim.Date = as.Date(Sim.Date), Sim.Mth=month(Sim.Date))
  
    scen.I <- subset(scen.I, Sim.Mth %in% incl1)

  #-----------------------
  #CONVERT Sim.Date to a fraction of its year
  # decimal_date from lubridate causes NA values for Jan 1st
  # uses date2decyear from WQ package (exported as source code - save in supporting functions folder as
  #  Decimal_date_function.R)
  #-----------------------  
  
    #setwd(formatfundir)
    #source("Decimal_date_function.R")
    #setwd(homedir)
  
    #xx$t.index <- decimal_date(xx$t.index)    # CAUSES NA in JAN1st values
  
    scen.I$t.index <- date2decyear(scen.I$Sim.Date)   #from WQ package

  #-----------------------  
  #ADD subpool column to xx
  #-----------------------  
  
#     sp.df <- sub.pools[,names(sub.pools) %in% c("Mth", sp.option) ]
#     #only include mths that will appear in scen.I
#     sp.df <- subset(sp.df, Mth %in% incl1)
    
    sp.index <- sub.pools[,names(sub.pools) == sp.option ]
    month.index <- sub.pools[,names(sub.pools) == "Mth" ]
    scen.I$Sim.sp <- sp.index[match(scen.I$Sim.Mth, month.index)] 
      
  scen.I$scenID <- as.character(unique(df$scenID))
  
  #-----------------------  
  #return scen.inded
  #----------------------- 
  
  return(scen.I)
  
}#end function

