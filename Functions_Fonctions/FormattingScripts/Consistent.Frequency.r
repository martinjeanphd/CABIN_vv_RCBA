######################################################################################
#---------------------------------------------------------------------------------
# Consistent.Freq FUNCTION: 
#
#  FUNCTION TO reduce datasets to a consistent frequency
#---------------------------------------------------------------------------------
######################################################################################

Consistent.Freq <- function(df, NperSeas=NULL, 
                            sel.method=c("NperSeas.random", "OnePerMonth.byday", 
                                         "sample.by.weeks", "NperMonth.byday"), 
                            day.of.month = 15, W.INT=NULL, BUF=NULL) {
  
  #NOTE: Currently the OnePerMonth requires Monthly Seasons to be defined
  
  #=====================================================================================================
  # Step 1: Require necessary R packages
  #=====================================================================================================
  
  #require(lubridate) # if package unavailable, install it via the R command: 
  
  #=====================================================================================================
  # Step 2: Format dataset
  #=====================================================================================================
  
  G <- df
  
#   G$Day <- day(G$Date)
   G$Year <- year(G$Date)
   G$Month <- month(G$Date)

  cat("\n")
  cat("DATA ROWS REMOVED AND KEPT WHEN REDUCING TO CONSISTENT FREQUENCY: ", "\n")
  cat("=====================================================================", "\n")
  cat("\n")
  cat("----------------------------", "\n")
  cat(unique(G$Parameter), "\n")
  cat("----------------------------", "\n")
  cat("\n")
  cat("Selection method: ", sel.method, "\n")
  cat("\n")
  
  #//
  if (sel.method=="sample.by.weeks"){
    
    H <- sample.by.weeks(G, BUF, W.INT, addMV = FALSE)
    
  #// 
  }else if (sel.method=="OnePerMonth.byday"){
  
    NperSeas = 1
    H <- NULL
    #--------------------------------------------
    # SUBSET DATA FOR EACH YEAR (S1)
    #--------------------------------------------
    
    for (k in 1:length(unique(G$Year))) {
      
      cat("\n")
      cat("************YEAR: ",unique(G$Year)[k], " ************", "\n") 
      cat("\n")
      
      S1 <- subset(G, Year==unique(G$Year)[k])    #subset data by year  
      
      Wwhich <- which(S1[,"Month"] %in% (rle(S1$Month)$values)[which(rle(S1$Month)$lengths > NperSeas)])  
      Wdupl <- S1$Month[Wwhich]
      
      W <- S1$Month
      "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
      Wnondupl <- W %w/o% Wdupl                    #vector of Seasons with <= than NperSeas samples
      
      S2 <- S1[S1$Month %in% Wdupl,]            #df containing samples in months with >NperSeas samples only
      S2nondupl <- S1[S1$Month %in% Wnondupl,]  #df containing samples in months with <=NperSeas samples only
      
      if (nrow(S2)==0) {
        H <- rbind(H, S2nondupl)
        cat("No duplicate data in any months in ",unique(G$Year)[k],"\n")  
      
      }else {
        
        M <- unique(S2$Month)   # M= seasons w/ more than NperSeas values
        Htmp <- S2nondupl
        
        cat("Sample dates for months with more than one value in ",unique(G$Year)[k],"\n")
        cat("\n")
        print(S2)
        cat("\n")
        
        for (m in 1:length(M)) {        #for each month with more than NperSeas values
          S3 <- subset(S2, Month==M[m])              
          #R3 <- as.numeric(rownames(S3))
          S4 <- OnePerMonth.byday(S3, day.of.month) 
          
          Htmp <- rbind(Htmp, S4)
           
        }#end for
        
        Htmp <- Htmp[order(Htmp$Month),]
        
        H <- rbind(H, Htmp) 
      }#end else   
    
    }#end for each year
    
    
  #//
    }else if (sel.method== "NperSeas.random"){
      
      H <- NULL
      #--------------------------------------------
      # SUBSET DATA FOR EACH YEAR (S1)
      #--------------------------------------------
      
      for (k in 1:length(unique(G$Year))) {
        
        cat("\n")
        cat("************YEAR: ",unique(G$Year)[k], " ************", "\n") 
        cat("\n")
        
        S1 <- subset(G, Year==unique(G$Year)[k])    #subset data by year  
        
        Wwhich <- which(S1[,"Season"] %in% (rle(S1$Season)$values)[which(rle(S1$Season)$lengths > NperSeas)])  
        Wdupl <- S1$Season[Wwhich]
        
        W <- S1$Season
        "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
        Wnondupl <- W %w/o% Wdupl                    #vector of Seasons with <= than NperSeas samples
        
        S2 <- S1[S1$Season %in% Wdupl,]            #df containing samples in seasons with >NperSeas samples only
        S2nondupl <- S1[S1$Season %in% Wnondupl,]  #df containing samples in seasons with <=NperSeas samples only
        
        if (nrow(S2)==0) {
          H <- rbind(H, S2nondupl)
          cat("No duplicate data in any season in ",unique(G$Year)[k],"\n")  
          
        }else {
          
          M <- unique(S2$Season)   # M= seasons w/ more than NperSeas values
          Htmp <- S2nondupl
          
          for (m in 1:length(M)) {        #for each month with more than NperSeas values
            S3 <- subset(S2, Season==M[m])              
            #R3 <- as.numeric(rownames(S3))
            S4 <- NperSeas.random(S3, NperSeas) 
            
            Htmp <- rbind(Htmp, S4)
            
            cat("Sample dates for seasons with more than ", NperSeas, " values in ",unique(G$Year)[k],"\n")
            cat("\n")
            print(S2)
            cat("\n")
            
          }#end for
          
          Htmp <- Htmp[order(Htmp$Month),]
          
          H <- rbind(H, Htmp) 
        }#end else   
        
      }#end for each year
      
    
  }#end sel.method ifs

  
  return(H)
}#end function  

  
  



