# add.rm.columns.R

###################################################################
# FUNCTIONS to add or remove columns from a dataframe
#
# Author: Jennifer MacDonald Date:  15 March 2013 Version: 1.0
# Modified : Martin Jean Date:      2017-01-10 Version 1.2
#
# FONCTIONS pour ajouter ou supprimer des colonnes d'un dataframe
##################################################################


#==============================================================================
# FUNCTION to remove columns with only NA values (i.e. no data)
#
# Args:
#   df: a dataframe
#
# Returns:
#   a dataframe without columns with no data
#
#------------------------------------------------------------------------------
#
# FONCTION pour supprimer les colonnes ne contenant que des valeurs NA
# (sans données)
#
# Arguments:
#   df: un dataframe
#
# Retourne:
#   un dataframe sans colonnes ne contenant aucune données
#==============================================================================

rm.NAcol <- function(df) {
    # 1. Replacing empty cell by #NA -- Remplacer cellule vide par #NA
    df[df == ""] <- NA

    # 2. counting the NAs per column -- dénombrement des NA par colonne
    n.na <- colSums(is.na(df))

    # 3. df keeping only the columns which are not completely na --
    #    dataframe ne conservant que les colonnes avec données
    keep <- df[,colSums(is.na(df)) != nrow(df)]

    # 4. name of the columns which are completely na --
    #    nom des colonnes qui n'ont que des NA
    rm <- names(df)[!names(df) %in% names(keep)]

    if (Language == "Francais") {
        cat(catDiacritical("COLONNES SUPPRIMÉES :"))
    } else {
        cat("COLUMNS REMOVED:","\n")
    }

    cat("-----------------","\n")
    if (Language == "Francais"){
        if (length(rm) > 1) {
            cat(catDiacritical(paste(length(rm), " colonnes sans données supprimées", sep="")))
        } else {
            cat(catDiacritical(paste(length(rm), " colonne sans données supprimée", sep="")))
        }
    } else {
        if (length(rm) > 1) {
            cat(length(rm), " columns with Missing Values (i.e. NAs) were removed", "\n")
        } else {
            cat(length(rm), " column with Missing Values (i.e. NAs) were removed", "\n")
        }
    }
    if(length(rm) != 0) {
        for (k in 1: length(rm)){cat("- ", rm[k], "\n")}
    }
    cat("\n")

    return(keep)

} #end function


#==============================================================================
# FUNCTION to create a dataframe with rows where obs = NA.
#
# Args:
#   ds: a dataframe
#
# Returns:
#   a dataframe with lines with no data
#
#------------------------------------------------------------------------------
#
# FONCTION pour créer un dataframe contenant les lignes ne contenant que des
# valeurs NA (sans données).
#
# Arguments:
#   ds: un dataframe
#
# Retourne:
#   un dataframe avec lignes ne contenant aucune données
#==============================================================================

get.NArows <- function(ds) {

    # 1. Replacing empty cell by #NA
    #    Remplacer cellule vide par #NA
    ds[ds == ""] <- NA

    # 2. df keeping only the lines which are completely na
    #    dataframe ne conservant que les lignes sans données
    ds1 <- ds[rowSums(is.na(ds)) == ncol(ds),]

	return(ds1)
}


#==============================================================================
# FUNCTION to remove rows where obs = NA. After this, each df may be different
#          lengths
#
# Args:
#   ds: a dataframe
#
# Returns:
#   a dataframe without lines with no data
#
#------------------------------------------------------------------------------
#
# FONCTION pour supprimer les lignes ne contenant que des valeurs NA
# (sans données). Après cela, chaque dataframe pourrait être de longueurs
# différentes.
#
# Arguments:
#   ds: un dataframe
#
# Retourne:
#   un dataframe sans lignes ne contenant aucune données
#==============================================================================

rm.NArows <- function(ds) {

    # 1. Replacing empty cell by #NA -- Remplacer cellule vide par #NA
    ds[ds == ""] <- NA

    # 2. counting the NAs per line -- dénombrement des NA par ligne
    n.na <- rowSums(is.na(ds))

    # 3. df keeping only the lines which are not completely na --
    #    dataframe ne conservant que les lignes avec données
    ds1 <- ds[rowSums(is.na(ds)) != ncol(ds),]

    # 4. name of the columns which are completely na --
    #    nom des colonnes qui n'ont que des NA
    rm <- rownames(ds)[!rownames(ds) %in% rownames(ds1)]

    cat("\n")

    if (Language == "Francais") {
        cat(catDiacritical(paste("LIGNES SUPPRIMÉES :")))
    } else {
        cat("ROWS REMOVED:","\n")
    }

    cat("-----------------","\n")

    if (Language == "Francais") {
        if ((nrow(ds)-nrow(ds1)) > 1) {
            cat(catDiacritical(paste(nrow(ds)-nrow(ds1), " lignes sans donnée (NA) supprimées", sep="")))
        } else {
            cat(catDiacritical(paste(nrow(ds)-nrow(ds1), " ligne sans donnée (NA) supprimée", sep="")))
        }
    } else {
        if ((nrow(ds)-nrow(ds1)) > 1) {
            cat(nrow(ds)-nrow(ds1), " rows with Missing Values (i.e. NAs) were removed", "\n")
        } else {
            cat(nrow(ds)-nrow(ds1), " row with Missing Values (i.e. NAs) was removed", "\n")
        }
    }

    if(length(rm) != 0) {
        for (k in 1: length(rm)){cat("- ", rm[k], "\n")}
    }

    cat("\n", "\n", "\n")

    return(ds1)
}

#==================================================================
# FUNCTION to add Flow column to a df
#
# FONCTION pour ajouter une colonne de débit au dataframe
#==================================================================

add.flow <- function(df, all.datasets) {

    ff <- "Flow"  #input dataset needs to be written as Flow (case-sensitive)
    #cat("Flow Data:", "\n")
    #cat("-------------------", "\n")

    if (ff %in% names(all.datasets)) {
        Flow <- all.datasets$Flow
        #cat("Flow data available")
    } else {
        Flow <- as.numeric(NA)
        #cat("NO Flow data available - inserted NA in flow column")
    }
    #cat("\n")
    df <- cbind(df, Flow)
}

#==================================================================
# FUNCTION to add Year, Month and MonthName to input dataframe
#           (df needs to have Date column)
# Adds:
# - Year as YYYY
# - Month as 1,2,3, ...12 numeric
# - MonthName as Jan, Feb...ordered factor
#==================================================================

YearMonth <- function(df) {

    #require(lubridate)

    df$Year <- year(df$Date)
    df$Month <- month(df$Date)

    df$MonthName <- df$Month
    df$MonthName <- factor(df$MonthName, levels=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), ordered=TRUE)

    #do I need to refactor to remove levels with no data??
    #df$MonthName<-factor(df$MonthName)

    return(df)
}

#==================================================================
# FUNCTION to add Yr.index column
# (1=first year in record to #yrs in record)
#==================================================================

add.YrIndex <- function(df) {

    #require(lubridate)
    df$tmpYr <- year(df$Date)
    index <- unique(df$tmpYr)                               #vector of all years in record
    new.yr <- index - (index[1]-1)
    df$Yr.index <- new.yr[match(df$tmpYr,index)]  #add yr.index column by matching year to index values
    df <- df[, !names(df) %in% c("tmpYr")]
    return(df)
}#end function


#==================================================================
# FUNCTION to add Parameter and StationID as columns in list of dataframes
#==================================================================

add.ParStn <- function(df.list,stationID) {

    df.list <- llply(df.list, function(df){df$StationID <- stationID; df})

    #ADD Parameter column (used for splitting graphs later)
    #this methods as Parameter as a factor
    #   for (i in 1:length(df.list)){
    #     tmp1 <- cbind(df.list[[i]], Parameter=names(df.list)[i])
    #     df.list[[i]] <- tmp1
    #   }
    #  rm(tmp1)
    # OR this methods adds as chr(d):
    df.list <- mapply(`[<-`,  df.list, "Parameter", value = names( df.list), SIMPLIFY = FALSE)

    return(df.list)
}#end add.ParStn function

#==================================================================
# FUNCTION to add season column to a df
# (df requires a (numeric 0-12) Month column)
#==================================================================

# add.season <- function(df, season.df=NULL) {
#
#   #IF  SEASON.DF = NULL then USE DEFAULT OF MONTHLY SEASONS
#   if (is.null(season.df)){
#     season.df <- data.frame(Month=c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug","Sep", "Oct","Nov","Dec"),
#                             Mth=rep(1:12, by=1), Season=rep(1:12, by=1))
#   }
#
#   df$Season <- season.df$Season[match(df$Month, season.df$Mth)]
#
#   return(df)
# } # end add.season function


#==================================================================
# FUNCTION to add Parameter Group to a df
#==================================================================

add.ParGR <- function(df, param.df) {

    param.df$RNAME <-  gsub("-", ".",  param.df$RNAME )
    df$Group <- param.df$VARIABLE_GROUP[match(df$Parameter, param.df$RNAME)]

    #renames group names (remove spaces and replace with ".")
    df$Group <- gsub(" ", ".", df$Group)

    return(df)
}


#==================================================================
#FUNCTION to add Units to a df
#==================================================================

add.units <- function(df, param.df) {

    param.df$RNAME <-  gsub("-", ".",  param.df$RNAME )
    df$Units <- param.df$UNIT_ID[match(df$Parameter, param.df$RNAME)]

    return(df)
}


#==================================================================
# FUNCTION TO ADD DECIMAL DATE COLUMN TO THE DATAFRAMES
# #add a date column with date in decimal date format
#==================================================================
#source(Decimal_date_function.R)    #or from WQpackage

add.decDate <- function(ds) {

    ds$dates.dec <- date2decyear(ds$Date)
    return(ds)
}


#==============================================================================
# FUNCTION to truncate the time in the Date field
#
# Args:
#   ds: a dataframe
#
# Returns:
#   a dataframe with time in date column
#
#------------------------------------------------------------------------------
#
# FONCTION pour supprimer l'heure dans le champs Date
#
# Arguments:
#   ds: un dataframe
#
# Retourne:
#   un dataframe sans heure dans la colonne Date
#==============================================================================

rm.Time <- function(ds) {

    ds[["SampleDate"]] <- substring(ds[,"SampleDate"], 1, 10)
	ds$SampleDate <- as.Date(ds$SampleDate, format = dateformat)

    return(ds)
}

