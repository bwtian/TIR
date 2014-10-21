
###############################################################################
## R code chunk: Import
###############################################################################
library("RefManageR")
library("xtable")
bibfile  <- "~/Dropbox/4refs/bib/paper1402-review.bib"
ref_bib  <- RefManageR::ReadBib(bibfile)
bib <- RefManageR::as.BibEntry(ref_bib)
bib_df  <- as.data.frame(bib)
  names(bib_df)  <- Hmisc::capitalize(names(bib_df)) # Make colname Capitzlized
  bib_df  <- bib_df[order(bib_df$Year, bib_df$Month),]
  bib_df$Authors  <- paste(gsub("([[:alpha:]]+).*","\\1",row.names(bib_df)), "et al.")
bib_tab  <- bib_df[,c("Authors","Year","Journal", "Bibtype")]
print(xtable(bib_tab))
