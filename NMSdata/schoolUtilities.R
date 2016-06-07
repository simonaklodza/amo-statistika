require(Unicode)
require(plyr)

#setwd("C:/Users/kapsitis/workspace/ddgatve-problems/NMSData/data")
#setwd(paste0(getwd(),"/data"))
thePath <- "amo2016.csv"
municipalityPath <- "skola-pasvaldiba.csv"
municipalityPath2 <- "adreses_1516.csv"
municipalityPath3 <- "lv-regioni.csv"

utf8ToIntVector <- function(arg) {
  return(as.vector(sapply(arg,utf8ToInt)))
}


#' @title getGender
#' @description Find gender by firstname
#' @details Given the firstname, return the gender of the person (Male/Female)
#' @aliases getGender
#' @author Kalvis Apsitis
#' @export getGender
#' @import Unicode
#' @param name A vector of names, represented in Unicode
#' @return Either "Male" or "Female"
#' @examples
#' getGender("Zane")
#' getGender(c("Suns", "Kaķis"))
getGender <- function(name) {
  # Drop leading, trailing spaces, 2nd names, etc.
  firstName <- sub(" *([^ ]+) *.*", "\\1",name)
  isMale <- (firstName == "Adam" |
               firstName == "Alexander" |
               firstName == "Aliaksandr" |
               firstName == "Andrey" |
               firstName == "Anton" |
               firstName == "Artem" |
               firstName == "Bruno" |
               grepl(".iro$", firstName) |
               firstName == "Dmitry" |
               firstName == "Dmytro" |
               firstName == "Guoyongyan" |
               firstName == "Hugo" |
               firstName == "Ivo" |
               firstName == "Lev" |
               firstName == "Nikita" |
               firstName == "Ņikita" |
               firstName == "Nikolay" |
               firstName =="Oleksandr" |
               firstName == "Oto" |
               firstName == "Raivo" |
               firstName == "Savva" |
               firstName == "Uko" |
               firstName == "Vladimir")
  #  isMale <- isMale & (!(firstName == "Nelli" |
  #               firstName == "Fani" |
  #               firstName == "Romi"))
  isMale <- isMale | grepl("[sš]$", firstName)
  nch <- nchar(firstName)
  lst <- substr(firstName,nch,nch)
  isMale <- isMale | (lst == "š") | (as.u_char(utf8ToIntVector(lst)) == "U+009A")
  result <- sapply(isMale, function(x) { if(x) { return("Male") } else {return("Female")} })
  return(result)
}



getSchoolType <- function(schNames) {
  vv <- sapply(schNames,function(schName) {
    result <- "NA"
    if (grepl("[Vv]idusskola", schName) |
        grepl("Valdorfa skola", schName) |
        grepl("[Ll]icejs", schName) |
        grepl("[Ss]tarptautisk.{1,2} skola", schName) |
        grepl("Doma kora skola", schName)) {
      result <- "vidusskola"
    } else if (grepl("[Ss].{1,2}kumskola", schName)) {
      result <- "sakumskola"
    } else if (grepl(".{1,2}imn.{1,2}zija", schName)) {
      result <- "gimnazija"
    } else if (grepl("[Pp]amatskola", schName) |
               grepl("[Aa]lternat.{1,2}v.{1,2} skola", schName) |
               grepl("[Ii]nternational Meridian School", schName)) {
      result <- "pamatskola"
    } else if (grepl("[Tt]ehnikums", schName)) {
      result <- "tehnikums"
    }
    return(result)
  })
  return(vv)
}

mDf <- read.table (file = municipalityPath,
                   header = TRUE, sep = ",", encoding = "UTF-8")
#mDf <- transform(mDf, School = as.character(School))
#mDf <- transform(mDf, Municipality = as.character(Municipality))
#mDf <- droplevels(mDf)


getSkolaNovadsMap <- function() {
  
  result <- list()
  for (i in 1:(nrow(mDf))) {
    result[[(mDf$School[i])]] <- mDf$Municipality[i]
  }
  return(result)
}

skolaNovadsMap <- getSkolaNovadsMap()


getNovadsSingle <- function(schName) {
  result <- "NA"
  
  if (!is.na(skolaNovadsMap[[schName]])) {
    result <- skolaNovadsMap[[schName]]
  }
  return(result)
}

getNovads <- function(ss) {
  vv <- sapply(ss, getNovadsSingle)
  return(vv)
}

getOlympicRatio <- function(schTypes) {
  vv <- sapply(schTypes,function(schType) {
    result <- 1
    if (schType == "gimnazija" | schType == "tehnikums") {
      result <- 1
    } else if (schType == "pamatskola") {
      result <- 5/9
    } else if (schType == "sakumskola") {
      result <- 1/3
    } else if (schType == "vidusskola") {
      result <- 2/3
    }
    return(result)
  })
  return(vv)
}

getEstEligible <- function(schType,izglitojamie) {
  result <- round(getOlympicRatio(schType)*izglitojamie)
  return(result)
}

getDisplaySkTips <- function(schType) {
  result <- "nezin\u0101ms"
  if (schType == "gimnazija") {
    result <- "\u01E7imn\u0101zija"
  } else if (schType == "tehnikums") {
    result <- "tehnikums"
  } else if (schType == "pamatskola") {
    result <- "pamatskola"
  } else if (schType == "s\u0101kumskola") {
    result <- "s\u0101kumskola"
  } else if (schType == "vidusskola") {
    result <- "vidusskola"
  }
  return(result)
}



getKlasuIntervals <- function(schType) {
  result <- "nezin\u0101ms"
  if (schType == "gimnazija") {
    result <- "7--12"
  } else if (schType == "tehnikums") {
    result <- "10--12"
  } else if (schType == "pamatskola") {
    result <- "1--9"
  } else if (schType == "s\u0101kumskola") {
    result <- "1--6"
  } else if (schType == "vidusskola") {
    result <- "1--12"
  }
  return(result)
}

getRegionsDisplay <- function(regCode) {
  result <- "nezin\u0101ms"
  if (regCode == "41") {
    result <- "Kurzemes re\u01E7ions (\u0101rpus L.p.)"
  } else if (regCode == "42") {
    result <- "Latgales re\u01E7ions (\u0101rpus L.p.)"
  } else if (regCode == "43") {
    result <- "Pier\u012Bgas re\u01E7ions (\u0101rpus L.p.)"
  } else if (regCode == "44") {
    result <- "Vidzemes re\u01E7ions (\u0101rpus L.p.)"
  } else if (regCode == "45") {
    result <- "Zemgales re\u01E7ions (\u0101rpus L.p.)"
  } else if (regCode == "10") {
    result <- "J\u0113kabpils"
  } else if (regCode == "25") {
    result <- "Valmiera"
  } else if (regCode == "27") {
    result <- "Daugavpils"
  } else if (regCode == "28") {
    result <- "Jelgava"
  } else if (regCode == "29") {
    result <- "J\u016Brmala"
  } else if (regCode == "30") {
    result <- "Liep\u0101ja"
  } else if (regCode == "31") {
    result <- "R\u0113zekne"
  } else if (regCode == "32") {
    result <- "Ventspils"
  } else if (regCode == "34") {
    result <- "R\u012Bgas Centra rajons"
  } else if (regCode == "35") {
    result <- "R\u012Bgas Kurzemes rajons"
  } else if (regCode == "36") {
    result <- "R\u012Bgas Latgales priek\u0161pils\u0113ta"
  } else if (regCode == "37") {
    result <- "R\u012Bgas Vidzemes priek\u0161pils\u0113ta"
  } else if (regCode == "38") {
    result <- "R\u012Bgas Zemgales priek\u0161pils\u0113ta"
  } else if (regCode == "39") {
    result <- "R\u012Bgas Zieme\u013Cu rajons"
  }
  return(paste0(result," (kods=",regCode,")"))
}

getUrbanizacijaDisplay <- function(parUrbanizacija) {
  result <- "nezin\u0101ms"
  if (parUrbanizacija == "riga") {
    result <- "1 -- R\u012Bga"
  } else if (parUrbanizacija == "lielpilseta") {
    result <- "2 -- lielpils\u0113ta \u0101rpus R\u012Bgas"
  } else if (parUrbanizacija == "pilseta") {
    result <- "3 -- pils\u0113ta"
  } else if (parUrbanizacija == "lauki") {
    result <- "4 -- lauki"
  }
  return(paste0(result," (1 -- R\u012Bga, 2 -- cita lielpils\u0113ta, 3 -- pils\u0113ta, 4 -- lauki)"))
}

getUrbanizacijaShortDisplay <- function(parUrbanizacija) {
  vv <- sapply(parUrbanizacija, function(pu) {
    result <- "nezin\u0101ms"
    if (pu == "riga") {
      result <- "R\u012Bga"
    } else if (pu == "lielpilseta") {
      result <- "Cita lielpils\u0113ta"
    } else if (pu == "pilseta") {
      result <- "Pils\u0113ta"
    } else if (pu == "lauki") {
      result <- "Lauki"
    }
    return(result)
  })
  return(vv)
}




getRegDfReduced <- function() {
  regDf <- read.table (file = municipalityPath3,
                       header = TRUE, sep = ",", encoding = "UTF-8")
  regDf <- mutate(regDf, RegDalEstimate = G05LV+G06LV+G07LV+G08LV+G09LV+G10LV+G11LV+G12LV+
                    G05RU+G06RU+G07RU+G08RU+G09RU+G10RU+G11RU+G12RU+
                    G05ZZ+G06ZZ+G07ZZ+G08ZZ+G09ZZ+G10ZZ+G11ZZ+G12ZZ)
  regDfReduced <- regDf[,c("RegCode","Regions19","RegDalEstimate")]
}


getKlaseFix <- function(klases) {
  vv <- sapply(klases, function(klase) {
    result <- 0
    if (klase < 5) {
      result <- 5
    } else {
      result <- klase
    }
    return(result)
  })
  return(vv)
}

## TODO: Create a table instead of a hard-coded data-structure
tablesMap <- list("T1","T1","T2","T3",
                  "T4","T5","T6","T6","T7",
                  "T8","T9","T10","T11",
                  "T12","T12","T13")
names(tablesMap) <-
  list("riga.sakumskola","riga.pamatskola","riga.vidusskola","riga.gimnazija",
       "lielpilseta.sakumskola","lielpilseta.pamatskola","lielpilseta.vidusskola",
       "lielpilseta.tehnikums","lielpilseta.gimnazija",
       "pilseta.sakumskola","pilseta.pamatskola","pilseta.vidusskola","pilseta.gimnazija",
       "lauki.sakumskola","lauki.pamatskola","lauki.vidusskola")


getUrbanTypeTable <- function(urbanizacijas, types) {
  iRange <- 1:length(urbanizacijas)
  vv <- sapply(iRange, function(i) {
    key <- paste0(urbanizacijas[i],".",types[i])
    return(tablesMap[[key]])
  })
  return(vv)
}


getAddressTable <- function() {
  addressDf <- read.table(file = municipalityPath2,header = TRUE,sep = ",",encoding = "UTF-8")
  addressDf <- mutate(addressDf, SkTips = getSchoolType(Nosaukums))
  addressDf$RTable <- getUrbanTypeTable(addressDf$Urbanizacija, addressDf$SkTips)
  return(addressDf)
}


getHistResults <- function(fName) {
  df <- read.table (file = fName, header = TRUE, sep = ",", encoding = "UTF-8")
  df2 <- mutate(df, Dzimums = getGender(Vards))
  df3 <- mutate(df2, KlaseFix = getKlaseFix(Klase))
  return(df3)
}




topicMap <- list()
########### grades:--1,2,3,4,5,6,7,8,9,10,11,12
topicMap[["A"]] <- c(0,0,0,0,3,3,4,3,4,3,3,3)
topicMap[["G"]] <- c(0,0,0,0,4,5,3,4,3,4,4,4)
topicMap[["C"]] <- c(0,0,0,0,5,4,5,5,5,5,5,5)
topicMap[["N"]] <- c(0,0,0,0,2,2,2,2,2,2,2,2)
topicMap[["S"]] <- c(0,0,0,0,1,1,1,1,1,1,1,1)



## topic is one letter (A,G,C,N,S)
## grade, u1, u2, u3, u4, u5 - are vectors of equal size
## Return vector with the grades for the given topic
getScoresByTopic <- function(topic, grade, u1, u2, u3, u4, u5) {
  iRange <- 1:length(grade)
  uzdForCurrentTopic <- topicMap[[topic]]
  vv <- sapply(iRange, function(i) {
    tNum <- uzdForCurrentTopic[grade[i]]
    #vect <- c(u1[i],u2[i],u3[i],u4[i],u5[i])
    result <- 0
    if (tNum == 1) {
      result <- u1[i]
    } else if (tNum == 2) {
      result <- u2[i]
    } else if (tNum == 3) {
      result <- u3[i]
    } else if (tNum == 4) {
      result <- u4[i]
    } else if (tNum == 5) {
      result <- u5[i]
    }
    # TODO: Amazing error!
    #     if (i <= 6) {
    #       print(paste0("i = ", i))
    #       print(paste0("  (",u1[i],",",u2[i],",",u3[i],",",u4[i],",",u5[i],"),tNum=",tNum,",",vect[tNum]))
    #     }
    #return(vect[tNum])
    return(result)
  })
  return(vv)
}

getEstRight <- function(est5To12, relError) {
  return(est5To12 == 24 | ( est5To12 != 0 & (relError >= -1/2 & relError <= 1/3)))
}

getEstUltimate <- function(estRight, est5To12, maxDalEstimate) {
  ifelse(estRight,est5To12,maxDalEstimate)
}



urbAndTipsMap <- list()
urbAndTipsMap[["riga.sakumskola"]] <- "T1"
urbAndTipsMap[["riga.pamatskola"]] <- "T1"
urbAndTipsMap[["riga.vidusskola"]] <- "T2"
urbAndTipsMap[["riga.tehnikums"]] <- "T2"
urbAndTipsMap[["riga.gimnazija"]] <- "T3"
urbAndTipsMap[["lielpilseta.sakumskola"]] <- "T4"
urbAndTipsMap[["lielpilseta.pamatskola"]] <- "T5"
urbAndTipsMap[["lielpilseta.vidusskola"]] <- "T6"
urbAndTipsMap[["lielpilseta.tehnikums"]] <- "T6"
urbAndTipsMap[["lielpilseta.gimnazija"]] <- "T7"
urbAndTipsMap[["pilseta.sakumskola"]] <- "T8"
urbAndTipsMap[["pilseta.pamatskola"]] <- "T9"
urbAndTipsMap[["pilseta.vidusskola"]] <- "T10"
urbAndTipsMap[["pilseta.tehnikums"]] <- "T10"
urbAndTipsMap[["pilseta.gimnazija"]] <- "T11"
urbAndTipsMap[["lauki.sakumskola"]] <- "T12"
urbAndTipsMap[["lauki.pamatskola"]] <- "T12"
urbAndTipsMap[["lauki.vidusskola"]] <- "T13"
urbAndTipsMap[["lauki.tehnikums"]] <- "T13"
urbAndTipsMap[["lauki.gimnazija"]] <- "T13"

getUrbAndTips <- function(parUrbanizacija, parSkolasTips) {
  iRange <- 1:length(parUrbanizacija)
  vv <- sapply(iRange, function(i) {
    theKey <- sprintf("%s.%s", as.character(parUrbanizacija[i]), as.character(parSkolasTips[i]))
    theValue <- urbAndTipsMap[[theKey]]
    return(theValue)
  })
  return(vv)
}




getAllResults <- function() {
  ### Review strange school names by displaying list with unique values
  #df1 <- df[order(df$Skola),]
  #unique(df1$Skola)
  
  df <- read.table (file = thePath, header = TRUE, sep = ",", encoding = "UTF-8")
  df2 <- mutate(df, Dzimums = getGender(Vards))
  df3 <- mutate(df2, SkTips = getSchoolType(Skola))
  
  ### Find out, if all school types are recognized
  #murr <- df3[df3$SkTips == "NA",]
  #unique(murr$Skola)
  
  mDf <- read.table (file = municipalityPath,
                     header = TRUE, sep = ",", encoding = "UTF-8")
  
  df4 <- merge(df3, mDf, by.x = "Skola", by.y = "School")
  ## df4[is.na(df4$Municipality),]
  
  skolasDf <- read.table(file = municipalityPath2,header = TRUE,sep = ",",encoding = "UTF-8")
  skolasDfReduced <- skolasDf[,c("RegNr","Regions","Nosaukums","Plusma","Izglitojamie","Urbanizacija")]
  df5 <- merge(df4, skolasDfReduced, by.x = "Skola", by.y = "Nosaukums",all.x=TRUE)
  
  df5[is.na(df5$Regions),]
  ### Find the mismatched school names
  #df5[is.na(df5$Izglitojamie),]
  
  df5[is.na(df5$Urbanizacija),]
  regDfReduced <- getRegDfReduced()
  
  df6 <- merge(df5, regDfReduced, by.x = "Regions", by.y = "Regions19")
  
  df7 <- mutate(df6, MaxDalEstimate = getEstEligible(SkTips,Izglitojamie))
  
  df7$U1[df7$U1=="--"] <- 0
  df7$U2[df7$U2=="--"] <- 0
  df7$U3[df7$U3=="--"] <- 0
  df7$U4[df7$U4=="--"] <- 0
  df7$U5[df7$U5=="--"] <- 0
  
  df7 <- mutate(df7, KlaseFix = getKlaseFix(Klase))
  df7$UA <- getScoresByTopic("A",df7$KlaseFix,df7$U1,df7$U2,df7$U3,df7$U4,df7$U5)
  df7$UG <- getScoresByTopic("G",df7$KlaseFix,df7$U1,df7$U2,df7$U3,df7$U4,df7$U5)
  df7$UC <- getScoresByTopic("C",df7$KlaseFix,df7$U1,df7$U2,df7$U3,df7$U4,df7$U5)
  df7$UN <- getScoresByTopic("N",df7$KlaseFix,df7$U1,df7$U2,df7$U3,df7$U4,df7$U5)
  df7$US <- getScoresByTopic("S",df7$KlaseFix,df7$U1,df7$U2,df7$U3,df7$U4,df7$U5)
  
  amoSkaitsDf <- read.table (file = "AMO_skaits.csv",
                             header = TRUE, sep = ",", encoding = "UTF-8")
  
  amoSkaitsDf <- amoSkaitsDf[,c("SkolaNew","Pieteikti","Est5To12","RakstVieta")]
  df8 <- merge(df7, amoSkaitsDf, by.x = "Skola", by.y = "SkolaNew")
  df8 <- mutate(df8, RelError = (Est5To12 - MaxDalEstimate)/MaxDalEstimate)
  df8 <- mutate(df8, EstRight = getEstRight(Est5To12, RelError))
  df8 <- mutate(df8, EstUltimate = getEstUltimate(EstRight, Est5To12, MaxDalEstimate))
  ttemp  <- aggregate(Izglitojamie ~ Skola, df8, length)
  #  rename(ttemp, c("Izglitojamie"="Daliba"))
  names(ttemp)[names(ttemp)=="Izglitojamie"] <- "Daliba"
  df9 <- merge(df8, ttemp, by.x = "Skola", by.y = "Skola")
  df9 <- mutate(df9, Intensity = Daliba/EstUltimate)
  
  df9$UrbAndTips <- getUrbAndTips(df9$Urbanizacija, df9$SkTips)
  
  return(df9)
}

allResults <- getAllResults()


getPunktiTotalForSkola <- function(regNrList) {
  vv <- sapply(regNrList,function(regNr) {
    result <- sum(allResults$Summa[allResults$RegNr == regNr])
    return(result)
  })
  return(vv)
}

getParticipationForSkola <- function(regNrList) {
  vv <- sapply(regNrList,function(regNr) {
    result <- sum(allResults$Summa[allResults$RegNr == regNr])
    return(result)
  })
  return(vv)
}


getMeitenesForSkola <- function(regNrList) {
  vv <- sapply(regNrList,function(regNr) {
    girls <- allResults[allResults$RegNr == regNr & allResults$Dzimums == "Female",]
    visi <- allResults[allResults$RegNr == regNr,]
    return(sprintf("%d no %d", nrow(girls),nrow(visi)))
  })
  return(vv)
}


getNoShowSkolas <- function(regCode) {
  return(13)
}



myAddressTable <- getAddressTable()

getShortName <- function(RegNrs) {
  vv <- sapply(RegNrs,function(regNr) {
    result <- "undefined"
    if (regNr == "4112901043") {
      result <- "Sikshnu pamatskola"
    } else if (regNr == "2716901232") {
      result <- "J.Pilsudska Daugavpils v. polu gimn."
    } else if (regNr == "3013900925") {
      result <- "J.Cakstes Liepajas 10. vidusskola"
    } else if (regNr == "3013900932") {
      result <- "DA Liepajas 5. vidusskola"
    } else if (regNr == "3612802850") {
      result <- "RIMS-Riga International meridian school"
    } else if (regNr == "3613801080") {
      result <- "ISMA vidusskola PREMJERS"
    } else if (regNr == "3613901263") {
      result <- "Rigas Lietuvieshu vidusskola"
    } else if (regNr == "3624900694") {
      result <- "Rigas Valda Avotina pamatskola"
    } else if (regNr == "3712900760") {
      result <- "Mezhciema pamatskola"
    } else if (regNr == "3713900754") {
      result <- "Dubnova Rigas Ebreju vidusskola"
    } else if (regNr == "3813800960") {
      result <- "Privata vidusskola Evrika"
    } else if (regNr == "3913803014") {
      result <- "RTU inzenierzinatnu vidusskola"
    } else if (regNr == "3913900818") {
      result <- "Rigas Rinuzhu vidusskola"
    } else if (regNr == "3913900821") {
      result <- "Pushkina licejs"
    } else if (regNr == "4312900232") {
      result <- "Tukuma E.B.Upisha 1. pamatskola"
    } else if (regNr == "4313800383") {
      result <- "Adazu Briva Valdorfa skola"
    } else if (regNr == "4313901008") {
      result <- "Salaspils 1. vidusskola"
    } else if (regNr == "4313901092") {
      result <- "Salaspils 2. vidusskola"
    } else if (regNr == "4313902768") {
      result <- "Garkalnes Makslu un visp. vidusskola"
    } else if (regNr == "4419900342") {
      result <- "DA Cesu Valsts gimnazija"
    } else if (regNr == "4419900900") {
      result <- "E.Glika Aluksnes Valsts gimnazija"
    } else {
      result <- as.character(myAddressTable$Nosaukums[myAddressTable$RegNr == regNr])
    }
    return(result)
  })
  return(vv)
}


# Get a vector of numbers 0 to 50, return vector of 1's (and if there are repeats, with 2's, 3's etc.)
theCounter <- function(myvect) {
  if (length(myvect) == 0) {
    return(numeric(0))
  } else {
    theTable <- rep(0,times=51)
    vv <- numeric(0)
    for (i in 1:length(myvect)) {
      vv <- c(vv, theTable[myvect[i]+1])
      theTable[(myvect[i]+1)] <- theTable[(myvect[i]+1)] + 1
    }
    return(vv)
  }
}


# What level the histogram ends for each class
classMaxes <- c(0,0,0,0,42,42,42,42,33,33,24,24)

getHistogram <- function(theDf, figTitle, classNum) {
  yUpperLimit <- classMaxes[classNum] + 2
  theDf$Urbanizacija <-
    factor(theDf$Urbanizacija,
           levels = c("lauki", "pilseta", "lielpilseta","riga"))
  
  result <- ggplot(theDf, aes(Summa, fill = Urbanizacija)) +
    geom_histogram(binwidth = 1) +
    theme(text = element_text(size=8),
          legend.position="none",
          plot.title = element_text(size=8),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_text(angle=90, margin = margin(1,3,1,1)),
          panel.background=element_blank(),
          panel.grid.minor.y = element_line(colour = "black", linetype = "dotted", size=0.2),
          panel.grid.major.y = element_line(colour = "black", linetype = "solid", size=0.2),
          panel.grid.minor.x = element_line(colour = "black", linetype = "dotted", size=0.2),
          panel.grid.major.x = element_line(colour = "black", linetype = "solid", size=0.2),
          plot.background=element_blank(),
          axis.text.x = element_text(angle = 0, vjust = 1.2, hjust=0, face=fontFaces)
    ) +
    ggtitle(figTitle) +
    #    scale_x_continuous(limits = c(0, yUpperLimit)) + 
    scale_y_continuous(breaks=c(0,10,20,30,40,50), limits = c(0, yUpperLimit), 
                       minor_breaks=seq(0,50,by=5)) +
    ylab("Skaits")
  
  return(result)
}







multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


