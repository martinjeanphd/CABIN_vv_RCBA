#Requirements

#Required.Functions.FUN <- function {

#===============================================================================
# STEP 3: SOURCE SUPPORTING CODE
#===============================================================================

#Supporting code needed for importing and formatting data:
source("../../../Functions_Fonctions/Data.Import.Code.R", encoding="UTF-8")

source("../../../Functions_Fonctions/FormattingScripts/read.data_wDates.R")
source("../../../Functions_Fonctions/FormattingScripts/add.rm.columns.R")  #contains: rm.NAcol, rm.NArows, add.Var.GR, add.flow, add.ParStn, add.season, YearMonth, add.allcolumns
source("../../../Functions_Fonctions/FormattingScripts/SplitDL.R")
source("../../../Functions_Fonctions/FormattingScripts/ReplaceND.R")
source("../../../Functions_Fonctions/FormattingScripts/Cons.Freq.FUN.R")
source("../../../Functions_Fonctions/FormattingScripts/excelUtils3.R")
source("../../../Functions_Fonctions/FormattingScripts/Decimal_date.FUN.R")
source("../../../Functions_Fonctions/FormattingScripts/Consistent.Frequency.R")
source("../../../Functions_Fonctions/FormattingScripts/LOG.DataImport.FUN.R")
source("../../../Functions_Fonctions/FormattingScripts/Export.2xls.R")
source("../../../Functions_Fonctions/FormattingScripts/check.var.FUN.R")
source("../../../Functions_Fonctions/FormattingScripts/check.spcnt.FUN.R")

getColor2 <- function(dataset.SPA2) {
  sapply(dataset.SPA2$SampleDate, function(SampleDate) {
    if(SampleDate <= "2005-12-31") {
      "green"
    } else if(SampleDate <= "2006-12-31") {
      "yellow"
    } else if(SampleDate <= "2007-12-31") {
      "darkorange"
    } else if(SampleDate <= "2008-12-31") {
      "red"
    } else if(SampleDate <= "2009-12-31") {
      "pink"
    } else if(SampleDate <= "2010-12-31") {
      "blue"
    } else if(SampleDate <= "2011-12-31") {
      "darkgreen"
    } else if(SampleDate <= "2012-12-31") {
      "cyan"
    } else if(SampleDate <= "2013-12-31") {
      "deeppink"
    } else if(SampleDate <= "2014-12-31") {
      "lightblue"
    } else if(SampleDate <= "2015-12-31") {
      "lightseagreen"
    } else if(SampleDate <= "2016-12-31") {
      "darkviolet"
    } else if(SampleDate <= "2017-12-31") {
      "darkred"
    } else if(SampleDate <= "2018-12-31") {
      "cadetblue"
    } else if(SampleDate <= "2019-12-31") {
      "black"
    } else if(SampleDate <= "2020-12-31") {
      "gray"
    } else if(SampleDate <= "2021-12-31") {
      "white"
    } else {
      "brown"
    } })
} 


# #supporting code needed for graphs and tables:
source("../../../Functions_Fonctions/PlotScripts/InfoPlotsFUN/BoxplotsOutliners.r")


source("../../../Functions_Fonctions/ModuleFunctions/Module1.FUN.R")
#source("Module2.FUN.R")

source("../../../Functions_Fonctions/CABINScripts/Verification.data.FUN.R", encoding = "UTF-8")

#} #end Function

