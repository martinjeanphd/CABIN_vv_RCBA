{summary.BIO <- as.data.frame(t(do.call(cbind, lapply(dataset.BIO, summary))))
summary.BIO$Std.deviation <- apply(dataset.BIO, 2, sd, na.rm = T)
summary.BIO$Length <- colSums(!is.na(dataset.BIO))
summary.BIO$BinaryData <- sapply(dataset.BIO,function(x)length(unique(na.omit(x)))<=2)
summary.BIO$NA.values <- colSums(is.na(dataset.BIO))
summary.BIO$`NA's` <- NULL
summary.BIO <- as.data.frame(t(summary.BIO))
summary.BIO
}
pdf("")
datatable(summary.BIO, filter = "top", rownames=F, options = list(scrollX = TRUE ))
dev.off()