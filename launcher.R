library(knitr)

options(encoding='UTF-8')

setwd("/Users/Simona/Documents/GitHub/amo-statistika/")


amoNum <- 43
skolas <- c("Valmieras Valsts gimnazija", "Valmieras Viestura vidusskola")

count <- 0
skola <- skolas[1]
#  count <- count + 1
#  knit2pdf(input="amo43-report.Rnw", output = paste0("amo43-report", count, ".Rnw"))
knit2pdf("amo43-report.Rnw")


