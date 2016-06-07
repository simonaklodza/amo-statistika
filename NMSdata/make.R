library(knitr)
library(Cairo)
library(cairoDevice)
library(ggplot2)

setwd("/Users/kapsitis/workspace/ddgatve-problems/NMSdata/data")
Sys.setenv(TEXINPUTS=getwd(),
           BIBINPUTS=getwd(),
           BSTINPUTS=getwd())

# parRegNum <- "4113901044"
# parSchoolNameAscii <- "Aizputes-vidusskola"

parRegNum <- "3713900756"
#parRegNum <- "4419902408"
parSchoolNameAscii <- "Rigas-64-vidusskola"
outputFile <- sprintf("amo43-parskats-%s-%s.2.pdf",parRegNum,parSchoolNameAscii)

parFigHist <- "AMO dal\u012Bbnieku skaits pa gadiem"
parFigHistGrade <- "AMO dal\u012Bbnieku proporcijas pa klas\u0113m"
parFigA <- "Aktivit\u0101te valsts re\u01E7ionos"
parFigB <- "Korel\u0101cija starp skolas uzdevumu un kopv\u0113rt\u0113jumu"
parFigGenderDifference <- "Meite\u0146u un z\u0113nu ieg\u016Bto punktu starp\u012Bba"
ParFigCompletelySolved <- "Piln\u012Bb\u0101 izr\u0113\u0137in\u0101to uzdevumu \u012Bpatsvars"
ParFigGradeLegends <- "Histogrammu apz\u012Bm\u0113jumi"
ParFigGradeA <- "Dati par 5. un 6. kl."
ParFigGradeB <- "Dati par 7. un 8. kl."
ParFigGradeC <- "Dati par 9. un 10. kl."
ParFigGradeD <- "Dati par 11. un 12. kl."

knit2pdf("amo43-report.Rnw", encoding = "UTF8")

