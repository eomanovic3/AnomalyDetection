# Master thesis work
# Omanovic Emina

install.packages('dplyr')
install.packages('stringr')
install.packages('caTools')
install.packages('anytime')
install.packages('e1071') 
library(e1071) 
library(dplyr)  
library(stringr)
library(caTools)
library(anytime)
library(caret)

# Učitati log20110211 set podataka
data = log20110211;

# Filtrirati podatke na osnovu zadataih kriterija u poglavlju 4.2.2.
filterData <- function (data) {
  browserLinux <- data[data$browser %in% c("lin"), ]
  browserNotNull <- browserLinux[!is.na(browserLinux$browser) , ]
  idxProperData <- browserNotNull[browserNotNull$idx == 0, ]
  sizeProperData <- idxProperData[!is.na(idxProperData$size) , ]
  findProperData <- sizeProperData[sizeProperData$find == 9, ]
  crawlerProperData <- findProperData[findProperData$crawler == 0, ]
  
  crawlerProperData$crawler <- NULL
  crawlerProperData$idx <- NULL
  crawlerProperData$norefer <- NULL
  crawlerProperData$noagent <- NULL
  crawlerProperData$zone <- NULL
  crawlerProperData$find <- NULL
  return(crawlerProperData)
}

# Detektovati robote koristeći se Loughran$McDonald pristupom
removeRobotDataUsingLMApproach<- function (data) {
  # Potrebno je transponovati podatke kako bi se moglo prolaziti petljom kroz redove
  transposedData <- as.data.frame(t(data))
  counter = 1
  isRobotColumnPerDay = c()
  for(record in transposedData) {
    transposedDataItem = as.data.frame(record)
    
    # IP adresa i datum kojim će se vršiti poređenje
    date = as.character(sapply(transposedDataItem, "[", 2))
    ip = as.character(transposedDataItem[1, 1])
    downloadFileCounterPerDay = 1
    for(i in transposedData) {
      transposedDataItemToBeCompared = as.data.frame(i)
      
      # IP adresa i datum s kojim će se vršiti poređenje
      dateToBeCompared = as.character(sapply(transposedDataItem, "[", 2))
      ipToBeCompared = as.character(transposedDataItemToBeCompared[1, 1])
      
      # Potrebno je brojati koliko puta je određena ip adresa preuzela neku datoteku 
      if(!is.na(date) && !is.na(dateToBeCompared) && !is.na(ip) && !is.na(ipToBeCompared)){
        if(date == dateToBeCompared && ip == ipToBeCompared){
          downloadFileCounterPerDay = downloadFileCounterPerDay + 1
        }
      }
    }
    
    # Ukoliko je ta ip adresa preuzela 50 i više datoteka na isti dan, klasifikuje se kao robot 
    if(downloadFileCounterPerDay >=50){
      isRobotColumnPerDay = cbind(isRobotColumnPerDay, 1)
    } else {
      isRobotColumnPerDay = cbind(isRobotColumnPerDay,  0)
    }
    counter = counter + 1
  }
  LM = t(isRobotColumnPerDay)
  data = cbind(data, LM)
  return(data)
}

# Filtirati podatke
dataWithFilters = filterData(data)

# Dodati kolonu LM
dataWithLMcolumn = removeRobotDataUsingLMApproach(dataWithFilters)

# Promijeniti kolonu vrijeme da ima epoch vrijednost
dataWithLMcolumn$time = as.integer(as.POSIXct(paste(dataWithLMcolumn$date, dataWithLMcolumn$time)))

# Pretvoriti LM kolonu u tip faktor, radi lakšeg rada sa SVM funkcijama
dataWithLMcolumn$LM = as.factor(dataWithLMcolumn$LM)

# Ukloniti sve nekompletne logove
dataWithLMcolumn = dataWithLMcolumn[complete.cases(dataWithLMcolumn), ]

# Pretvoriti cik i time u numeričke vrijednosti
dataWithLMcolumn$time = as.double(dataWithLMcolumn$time)
dataWithLMcolumn$cik = as.double(dataWithLMcolumn$cik)

# Finalne izmjene za krajnji set podataka koji će se koristiti
dataForSvm = dataWithLMcolumn
dataForSvm = dataWithLMcolumn[order(-dataWithLMcolumn$time),]