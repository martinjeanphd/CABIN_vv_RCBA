#Table for diversity metrics

#Richness
N0 <- rowSums(dataset.BIO > 0, na.rm = TRUE)

#Shannon's entropy
#dataset.BIO[is.na(dataset.BIO)] <- 0
H <- diversity(dataset.BIO)

#Shannon's index
N1 <- exp(H)

#Simpson's index
N2 <- diversity(dataset.BIO, "inv")

#Pielou's evenness
J <- H/log(N0)

#Shannon's evenness (Hill's ratio)
E10 <- N1/N0

#Simpson's evenness (Hill's ratio)
E20 <- N2/N0

#Table for diversity
#(div <- data.frame(N0, H, N1, N2, E10, E20, J))
#dataset.BIO[dataset.BIO == 0] <- NA

div <- data.frame(N0, H, N1, N2, J, E10, E20, dataset.BIO2$Site_Date_Number)
#div <- rownames_to_column(div, var = "rownames")
#div <- left_join(div, dataset.NAM2[,c("SampleId", "Site_Date_Number")], by = "SampleId")
#div$rownames <- NULL

{div <- div[, c(8,1,2,3,4,5,6,7)]
  #div
  names(div)[1] <- "Site_Date_Visit"
  names(div)[2] <- "Richness"
  names(div)[3] <- "Shannon's entropy"
  names(div)[4] <- "Shannon's index"
  names(div)[5] <- "Simpson's index"
  names(div)[6] <- "Pielou's evenness"
  names(div)[7] <- "Shannon's evenness"
  names(div)[8] <- "Simpson's evenness"
}

pdf("Diversity.pdf", width = 17, height = 110)
#datatable(div, filter = "top", rownames=F, options = list(scrollX = TRUE ))
grid.table(div)
dev.off()
