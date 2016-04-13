library(knitr)
library(Cairo)
library(cairoDevice)
install.packages("ggplot2")


#options(encoding='UTF-8')

setwd("/Users/Simona/Documents/GitHub/amo-statistika/")

amoNum <- 43
skolas <- c("Valmieras Valsts \u01E7imn\u0101zija", "Valmieras Viestura vidusskola")
theDate <- format(Sys.time(), "%Y-%m-%d")

count <- 0
skola <- skolas[1]
knit2pdf("amo43-report.Rnw", encoding="UTF8")



