#SplitDL.R                                                            #SplitDL.R

###################################################################
# FUNCTION TO SPLIT OUT '<' SYMBOLS and create a list of dataframes such that
#          each parameter is a list of 1 df with 3 columns: obs, cen and Date
#
# returns a list of dataframes (1 per variable) with obs, cen, date columns.
#
###################################################################

SplitDL <-function(df){
#Required input: dataframe with parameter(s), Date, Hour, StationID columns 

#=====================================================================================================
# Step 0: Require necessary R packages
#=====================================================================================================
  
  #install.packages("NADA")   # if package unavailable
  #require(NADA)
  
  #install.packages("plyr")
  #require(plyr)

#===============================================================================
#Step 1: Create 1 DF with Date, Hour, StationID, and another with all the parameters
#===============================================================================

  dataset.list <- df[,!names(df) %in% c("Date", "Hour", "StationID")]
  #dataset.dhs <- df[,c("Date", "Hour", "StationID")]  #these must be present to work
  #dataset.dhs <- df[,names(param.df) %in% c("Date", "Hour", "StationID")]  #this would fix the need 
                                                                                  #to have all columns, but 
                                                                                  #will need to check impact later in code
 


#===============================================================================
# Step 2: SPLIT OUT '<' SYMBOLS INTO SEPERATE COLUMN AS TF
#
#splitQual can be used to separate the symbol '<' and the values in a column. The command
#splitQual must be applied to each column of values individually; it cannot be applied to the whole
#data set.   EXAMPLE: conc.vector=splitQual(dataset$SELENIUM.TOTAL)
#
#The command splitQual returns a data set with two variables for each vector it is applied to: one
#for the observed values, which is named obs, or in this case conc.vector$obs; and the other one for
#the indicator variable for the censoring, which is named cen, or in this case conc.vector$cen.
#
#===============================================================================

  #dataset.cen = list of obs and cen vectors (1 parameter per list item)  
  #dataset.cen <- lapply(dataset.list,splitQual)   
  dataset.cen <- apply(dataset.list, 2, splitQual)   #apply to each column (2)

  # obs, cen vectors converted to dataframe for each variable in list
  dataset.cen.df <- llply(dataset.cen, as.data.frame)

  #add date back into dataframe for each variable
    # for (i in 1:length(dataset.cen.df)) {
    #    dataset.cen.df[[i]] <- cbind(dataset.cen.df[[i]],dataset.dhs$Date)   
    #    colnames(dataset.cen.df[[i]]) <- c("obs","cen","Date")
    #  }

  dataset.cen.df<- llply(dataset.cen.df, function(x) cbind(x,Date=df$Date))
  #str(dataset.cen.df)   #list of dataframes (1 per variable) with obs, cen, date columns.


#===============================================================================
# Return list of dataframes (1 per variable with obs, cen, Date columns)
#===============================================================================

return (dataset.cen.df)

}#end function
