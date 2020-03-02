################################################################################
### Borcard.FUN.R
###
### Updated by / Mise  jour par: Martin Jean et Évelyne Paquette-Boisclair
### Date: 2017-06-06
### Version: 1.0
###
###----------------------------------------------------------------------------
### DESCRIPTION IN ENGLISH
###
### DESCRIPTION EN FRANÇAIS
################################################################################

frequencyHistogram <- function(df) {

# Compute the number of sites where each taxon is present
# To sum by columns, the second argument of apply(), MARGIN, is set to 2
df.pres <- apply(df > 0, 2, sum, na.rm = TRUE)

# Sort the results in increasing order
sort(df.pres)

# Compute percentage frequencies
df.relf <- 100*df.pres/nrow(df)

# Round the sorted output to 1 digit
round(sort(df.relf), 1)

# Plot the histograms
#quartz(title="Frequency Histograms",8,5)
#windows(title="Frequency Histograms",8,5)

# Divide the window horizontally
par(mfrow=c(1,2))
hist(df.pres, main="taxon Occurrences", right=FALSE, las=1,
	xlab="Number of occurrences", ylab="Number of taxa",
	breaks=seq(0,max(df.pres+5),by=5), col="bisque")
hist(df.relf, main="taxon Relative Frequencies", right=FALSE, las=1,
	xlab="Frequency of occurrences (%)", ylab="Number of taxa",
	breaks=seq(0, 100, by=10), col="bisque")

}