#"Perform_SK_PowerSims.R"           

#################################################################################
#  "Perform_SK_PowerSims" FUNCTION
#
#  DEFINE THE CODE FOR RUNNING POWER ANALYSIS 
#   
#################################################################################

Perform_SK_PowerSims <- function(df, savedir, homedir, data.col, 
                                  SimParameters, Sim.Index, subpools, 
                                  DS.option, num.harmonics=NULL, 
                                  DT.option, span.value, flowYN, 
                                  trend.levels, I, alpha, SKseas, 
                                  LANG, SEP, DEC) {
  
  #===============================================================================
  # STEP 0: LOAD REQUIRED PACKAGES
  #===============================================================================
  
  #===============================================================================
  # STEP 1: SOURCE SUPPORTING CODE
  #==============================================================================
  
  #===============================================================================
  # STEP 2: CREATE OUTPUT DIRECTORIES
  #==============================================================================
  
    setwd(savedir)
    stndir <- paste(getwd(),"/", unique(df$StationID), sep="")
    if (!file.exists(stndir)){
      dir.create(stndir, showWarnings = FALSE)
    }
    
    #Creates a SK_PowerResults dir in the station directory
    SKresultsdir <- paste(stndir, "/", "SK_PowerResults", sep="")
    assign("SKresultsdir", SKresultsdir, envir = .GlobalEnv)
    if (file.exists(SKresultsdir)){
      cat ("SKResults directory already exists", "\n")
    } else {
      dir.create(SKresultsdir, showWarnings = FALSE)
    }

  ParameterName <- unique(df$Parameter)
  paramdir <- paste(SKresultsdir, "/", ParameterName , sep="")
  if (file.exists( paramdir)){
    cat("\n")
    cat(ParameterName, "directory already exists. Results may be overwritten.", "\n")
  } else {
    dir.create(paramdir, showWarnings = FALSE)
  }
  
  #Creates a tmp directory to store Power results as they are run 
  tmpRESdir <- paste(paramdir, "/", "TmpPwrRESULTS", sep="")
  if (!file.exists(tmpRESdir)){
    dir.create(tmpRESdir, showWarnings = FALSE)
  }
  
  #===============================================================================
  # STEP 3: Assign input datasets to origdata 
  #===============================================================================
    
    origdata <- df
  
    stnID <- unique(origdata$StationID)
  
  #------------------------------------------------------------------------------
  # ADD Data.Method column to identify if model is for log or original units
  #------------------------------------------------------------------------------
  
  origdata$Data.Method <- paste(data.col)
  
  #===============================================================================
  # STEP 4: MODEL TREND AND SEASONALITY FROM THE ORIGINAL DATASET
  #===============================================================================
  
  #ADD DT.col COLUMN which is identical to the col identified in data.col in function def 
  origdata$datacol <- origdata[,grep(paste("^",data.col,"$", sep=""), names(origdata), value=TRUE)]   
  
  #calculate seasonal medians of original input dataset (for data.col)
  #Note: seasons here represent the season for input dataset (not SKgroup)
  SM <- by(origdata$datacol, origdata$Season, median, na.rm=T)
  seas.medians <- data.frame(cbind(SM = as.numeric(SM), Season = dimnames(SM)[[1]]), stringsAsFactors=FALSE)
  seas.medians$SM <- as.numeric(seas.medians$SM)
  
  #add SM column to origdata (matches SM to season column)  
  origdata$SM <- seas.medians$SM[match(origdata$Season, seas.medians$Season)] 
  
  #calculate overall stratified seasonal median
  sm <- median(seas.medians$SM)
  
    #------------------------------------
    # DETREND AND DESEASONALIZE THE DATASET
    #
    # DTDSmodel.Input contains a list with: 
    #  - DTDS.data : origdata w/ new columns for DTmethod, DT, DT.fit, DS, DS.fit
    #                DSmethod, harmonics, DTDS, DTDSfit
    #  - model.DT : output parameter of DT model on original data
    #  - model.DS : output paramters of DS model on orig data
    #  - model.DTDS : output paramters of DS model on DT data
    #  - DS.forumula : formula used to DS the orig data
    #  - DTDS.formula: formula used to DS to DT data
    #------------------------------------
  
    
      DTDSmodel.Input <- Run.DTDS(data.df=origdata, dataCOL=data.col, span.value, flowYN, 
                                  DT.option, DS.option, num.harmonics)
    
      #SAVE THE DTDSmodel.Input to the paramdir
        setwd(paramdir)
        save(DTDSmodel.Input, file="DTDSmodel.Input.RData")  
        setwd(homedir)
      
      #extract all elements in DTDSmodel.Input list and assign as list item names
      for(i in 1:length(DTDSmodel.Input)) {
        assign(paste0(names(DTDSmodel.Input[i])), DTDSmodel.Input[[i]])
      } 
    
        #THIS CREATES THE FOLLOWING VARIABLES:
        #   - DS.formula
        #   - model.DT
        #   - model.DS
        #   - model.DTDS
        #   - DTDS.data
        #   - DTDS.formula
    
      rm(DTDSmodel.Input)
    
  
    #------------------------------------
    # MODEL RESIDUALS USING AND ARMA process
    #------------------------------------
    ###########MAY ADD LATER#######################
  
 
  #===============================================================================
  # STEP 5: DEFINING THE VALUES OF BETA (in units/year) to use for test
  #         and ADD TO SimParameters (1 new column beta_% for each trend.level)
  #===============================================================================
  
    #Calculate stratified medians for the 
    # why doesn't it work with the detrended dataset
  
    #ADD col COLUMN which is identical to the col identified in data.col in function def 
    #tmp <- origdata
    #tmp$datacol <- tmp[,grep(paste("^",data.col,"$", sep=""), names(tmp), value=TRUE)]   
  
     # seas.med <- as.numeric(by(tmp$datacol, tmp$Month, median, na.rm=T))
      #sm <- median(seas.med)
    
  #  rm(tmp)
    
  #add beta value to SimParameters dataframe
      cc <- data.frame(matrix(nrow=nrow(SimParameters), ncol=length(trend.levels)))
      for (i in 1:length(cc)){names(cc)[i] <- paste("beta_", trend.levels[i], sep="")}
      cc1 <- cbind(SimParameters, cc)
      
      
      if (data.col == "Data"){
        for (j in 1:nrow(SimParameters)){
          tmp <- add.beta.CONC(SimParameters[j,],trend.levels, sm)
          cc1[j,] <- tmp 
        }
      }else if (data.col == "Data.log") {
        for (j in 1: nrow(SimParameters)){
          tmp <- add.beta.APC(SimParameters[j,], trend.levels)
          cc1[j,] <- tmp 
        }
      }
      
      SimParameters <- cc1   #beta columns now added
  
  #===============================================================================
  # STEP 6: CREATE A SET OF RESIDUALS (FROM THE DTDS data) TO USE IN THE BOOTSTRAP
  #         RESAMPLING PROCEDURE 
  #===============================================================================
  
    #-------------------------------------------------------------------------------
    # A) EXTRACT THE SET OF RESIDUALS FROM WHICH TO BOOTSTRAP 
    #-------------------------------------------------------------------------------
    
      residuals <- DTDS.data
    
      #add a resid column
      residuals$resid <- DTDS.data$DTDS
  
    #-------------------------------------------------------------------------------
    #  B) Calls the add.subpool FUNCTION (in Bootstrap_Methods.R code)
    #     - adds 2 columns: 
    #          1) subpool: used to add column indicating which set(pool) of residuals each 'resid'   
    #             should be extracted to 
    #          2) spOPT: name of the spX option
    #-------------------------------------------------------------------------------
    
      # Melt the subpool dataframe so that spX columns are the .id variable and VALUE
      sp.melt <- melt(subpools, id.vars=c("Mth", "Month"), variable.name="sp.col")
      
      #extract season.df from Season column of original dataset
      s.df1 <- unique(origdata[, names(origdata) %in% c("Month", "Season")])
   
      
      # merge will add in NA in Season column if the Month does not exist in s.df1; 
      # and then will replace NA with zero
      Mth.df <- data.frame (Month=rep(1:12, by=1))
      s.df <- merge(x=Mth.df,y=s.df1, by = "Month", all.x=TRUE)
      s.df$Season[is.na(s.df$Season)] <- 0
      names(s.df) <- c("Mth", "Season")
      rm(s.df1, Mth.df)
  
      #CALL THE ADD.SUBPOOL FUNCTION
      resid.df <- ddply(sp.melt, .(sp.col), add.subpools, residuals, season.df=s.df)  
      #resid.df = dataframe of residuals w/ sp and spOPT (replicates the resid df for each subpool option)
      #to seperate on option will need to subset by spOPT column
     
    #-------------------------------------------------------------------------------
    #  C) Create a dataframe that can be used to subset for residuals pool data values
    #     (columns: spOPT, subpool, sp.data, value
    #    - sp.data = name of the column used (resid)
    #    - value = data (resid) value belonging to the subpool and spOPT
    #-------------------------------------------------------------------------------
  
      resid.vals <- create.resid.ls(resid.df)
      
  
  #===============================================================================
  # STEP 6: BOOTSTRAP (i.e. sample with replacement) the appropriate residuals to create 
  #         I series of bootstrapped innovations (FOR EACH SCENARIO)
  #===============================================================================
  
    cat("Creating Bootstrapped innovations", "\n")
    cat("----------------------------------------------", "\n")
    
    #change scen.index list into a dataframe
    scenIND <- ldply(scen.index)  
  
    boottime <- Sys.time()
  
    #initiate bootres.all variable
    bootres.all <- NULL
  
    for (i in 1:nrow(SimParameters)){
       set.seed(1111)
       boot1 <- Create.bootres(SimParameters[i,], scenIND, residVALS=resid.vals, I) 
       bootres.all <- rbind(bootres.all, boot1)
    }
    #df with I=1000 columns with resampled residuals + Sim.Date + Sim.Mth+t.index+Sim.sp+ ScenarioID
  

   cat("     "); print(Sys.time()-boottime)
  

    #append Parameter to bootres.all (for reference in .RData file)
    bootres.all$Parameter <- unique(origdata$Parameter)
    
    #save bootres.all to Param Directory
    setwd(paramdir)
    save(bootres.all, file=paste("bootres.all_", unique(bootres.all$Parameter), ".RData", sep=""))
  
  
  
  #===============================================================================
  # STEP 7: CREATES A DATAFRAME DEFINING SEASONS OF SK GROUPINGS
  #===============================================================================  
  
  # Creates dataframe with all Months
  Mth.df <- data.frame(Month=c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug","Sep", "Oct","Nov","Dec"), Mth=rep(1:12, by=1))
  
  #Automatically adds user defined seasons (from step A above) to Mth.df 
  # --> gives a df with Month(Jan-Dec), Mth (01-12), and Season (1 to number of seasons)
  season.df <- cbind(Mth.df, SKgroup=SKseas)
  rm(Mth.df)
  
  
  #===============================================================================
  # STEP 8: FOR EACH SCENARIO (i.e. each row in SimParameters)
  #    - calculate the Seasonal Kendall test for the different slope values, for 
  #      each set of bootstrap innovations (I columns in bootres.all)
  #    - Compute the power of the Seasonal Kendall test for different slope values  
  #===============================================================================
    
      
  #Create an empty dataframe
    results.SK <- data.frame(StationID=character(), Parameter=character(), Units=character(),
                             Scenario=character(), freqName=character(), 
                             freq.option=character(), Interval=numeric(), sday=numeric(), sp.option=character(), 
                             NSimYrs=numeric(), NumSeas =numeric(), sm=numeric(), alpha=numeric(), 
                             TPC = numeric(), beta=numeric(), power=numeric())
 
    if(data.col=="Data.log"){gsub("TPC", "APC", names(results.SK))}

  #Run Power.SK for each scenario
  
  lptime <- Sys.time()
  
  #FOR EACH SCENARIO (ie. row in SimParameters)
  for (i in 1:nrow(SimParameters)){
      
      bbtime <- Sys.time()  
      
      SimPar1 <- SimParameters[i,]
      scen.name <- as.character(SimPar1$scenID)
      bootres1 <- subset(bootres.all, ScenarioID==scen.name)
      
      Sim.Index.df <- ldply(Sim.Index)
      scen.ind1 <- subset(Sim.Index.df, scenID == scen.name)
      
      subpools1 <- subset(sp.melt, sp.col == SimPar1$sp.option)
      
      #Run Power.SK function on a single set of scenario parameters
      outPWR <- Power.SK(SimPar1, bootRES=bootres1, scen.ind=scen.ind1, sub.pools=subpools1, k=num.harmonics, 
               model=model.DTDS, DS.option, alpha, I, SKseason.df=season.df)
      
    
      
      
      #create a df with power results and additional scenario info
      out.df <- data.frame(StationID=stnID, Parameter=ParameterName, Units= unique(origdata$Units),
                           Scenario=scen.name, freqName= SimPar1$freqName, 
                           Internval=SimPar1$Interval, sday=SimPar1$sday, sp.option=SimPar1$sp.option,
                           NSimYrs=SimPar1$NSimYears, numSeas=length(unique(SKseas)), sm=sm, alpha=alpha,TPC = trend.levels, 
                           beta=as.numeric(SimPar1[,grep("^[beta]", names(SimPar1))]),
                           power=outPWR )
 
      if(data.col=="Data.log"){gsub("TPC", "APC", names(out.df))}
     
      #cat("     "); print(Sys.time()-bbtime)
     
      #save tmp results to xls document in Parameter folder
        setwd(tmpRESdir)
        cat("Results for ", scen.name, "exported to csv", "\n")
        write.table(out.df, file=paste("TmpPWR_", ParameterName, "_", scen.name, ".csv", sep=""), 
                    sep=SEP, dec=DEC, row.names = FALSE)
        
      #append to master results df
        results.SK <- rbind(results.SK, out.df)
      
        #overwrite results.SK.RData file to keep up to date record in tempRESdir in case of crash
        save(results.SK, file=paste("Tmp_PWR_", ParameterName, ".RData"))     
    }

  cat("ALL Scenarios SK TIME:"); print(Sys.time()- lptime)
  #})
  
  #save FINAL parameter results to paramdir
  setwd(paramdir)
  save(results.SK, file=paste(ParameterName, "_SKResults.RData"))
  
  #save results to excel document
  resultsSK.2xls(savedir=paramdir, results.df=results.SK, savename=paste(ParameterName, ".PWR_Results", sep=""), LANG=LANG)
  
  
  
  cat("\n"); cat("FINAL RESULTS for ",  ParameterName, "exported to excel", "\n")
 
  #cat("------------------------------------------------------------------", "\n")

  return(results.SK)
}