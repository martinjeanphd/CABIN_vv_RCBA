#===============================================================================
# STEP 3: SOURCE SUPPORTING CODE
#===============================================================================

#Supporting code needed for importing and formatting data:
source("./Functions_Fonctions/Data.Import.Code.R", encoding="UTF-8")

source("./Functions_Fonctions/FormattingScripts/read.data_wDates.R")
source("./Functions_Fonctions/FormattingScripts/add.rm.columns.R")  #contains: rm.NAcol, rm.NArows, add.Var.GR, add.flow, add.ParStn, add.season, YearMonth, add.allcolumns
source("./Functions_Fonctions/FormattingScripts/SplitDL.R")
source("./Functions_Fonctions/FormattingScripts/ReplaceND.R")
source("./Functions_Fonctions/FormattingScripts/Cons.Freq.FUN.R")
source("./Functions_Fonctions/FormattingScripts/excelUtils3.R")
source("./Functions_Fonctions/FormattingScripts/Decimal_date.FUN.R")
source("./Functions_Fonctions/FormattingScripts/Consistent.Frequency.R")
source("./Functions_Fonctions/FormattingScripts/LOG.DataImport.FUN.R")
source("./Functions_Fonctions/FormattingScripts/Export.2xls.R")
source("./Functions_Fonctions/FormattingScripts/check.var.FUN.R")
source("./Functions_Fonctions/FormattingScripts/check.spcnt.FUN.R")


# #supporting code needed for graphs and tables:
source("./Functions_Fonctions/PlotScripts/InfoPlotsFUN/BoxplotsOutliners.r")


source("./Functions_Fonctions/ModuleFunctions/Module1.FUN.R")
#source("Module2.FUN.R")

source("./Functions_Fonctions/CABINScripts/Verification.data.FUN.R", encoding = "UTF-8")
