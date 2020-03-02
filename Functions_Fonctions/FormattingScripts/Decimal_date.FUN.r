#######################################################################################################
# FUNCTIONS TO CONVERT FROM DECIMAL YEAR TO DATE
#   - date2decyear: Converts object of class "Date" to decimal year assuming time of day is noon.
#   - decyear2date: Converts decimal year to object of class "Date".
#   - leapYear: TRUE if x is a leap year, FALSE otherwise.
#
#  Source: WQ-package
#  install.packages(wq)
#  library(wq)
######################################################################################################
leapYear <- function (x) 
{
  if (!is.numeric(x)) 
    stop("x must be numeric")
  x <- floor(x)
  x%%4 == 0 & (x%%100 != 0 | x%%400 == 0)
}

date2decyear <- function (w) 
{
  #old_ops <- options(digits = 8)
  #on.exit(options(old_ops))
  posx <- as.POSIXlt(w)
  yr <- 1900 + posx$year
  dy <- posx$yday + 0.5
  yr + dy/ifelse(leapYear(yr), 366, 365)
}

decyear2date <- function (x) 
{
  yr <- floor(x)
  len <- ifelse(leapYear(yr), 366, 365)
  julday <- floor((x - yr) * len)
  as.Date(julday, origin = as.Date(paste(yr, 1, 1, sep = "-")))
}
