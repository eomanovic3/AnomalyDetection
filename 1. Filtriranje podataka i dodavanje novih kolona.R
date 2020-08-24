# Master thesis work
# Omanovic Emina

install.packages('dplyr')
install.packages('stringr')
install.packages('caTools')
install.packages('anytime')
install.packages('e1071') 
install.packages('plyr') 
install.packages('hash') 
library(hash) 
library(plyr) 
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
  browserNotNull <- data[!is.na(data$browser) , ]
  browserNotEmpty <- browserNotNull[browserNotNull$browser != "", ]
  idxProperData <- browserNotEmpty[browserNotEmpty$idx == 0, ]
  sizeProperData <- idxProperData[!is.na(idxProperData$size) , ]
  crawlerProperData <- sizeProperData[sizeProperData$crawler == 0, ]
  
  crawlerProperData$crawler <- NULL
  crawlerProperData$idx <- NULL
  crawlerProperData$norefer <- NULL
  crawlerProperData$noagent <- NULL
  crawlerProperData$zone <- NULL
  return(crawlerProperData)
}

# Detektovati robote koristeći se Loughran$McDonald pristupom
removeRobotDataUsingLMApproach<- function (data) {
  # Potrebno je transponovati podatke kako bi se moglo prolaziti petljom kroz redove
  transposedData <- as.data.frame(t(data))
  counter = 1
  isRobotColumnPerDay = c()
  allCountsForOneIp = c()
  for(record in transposedData) {
    transposedDataItem = as.data.frame(record)
    print(transposedDataItem)
    # IP adresa i datum kojim će se vršiti poređenje
    dateValue = as.character(sapply(transposedDataItem, "[", 2))
    ipValue = as.character(transposedDataItem[1, 1])
    downloadFileCounterPerDay = data %>% # take the diamonds data.fram and group it
      group_by(ip, date) %>% # 56 groups
      summarize(count = n()) %>% # add a count column
      filter(ip==ipValue, date==dateValue) %>%  # filter the row you want
      .$count
    # Ukoliko je ta ip adresa preuzela 50 i više datoteka na isti dan, klasifikuje se kao robot 
    if(length(downloadFileCounterPerDay) != 0){
      if(downloadFileCounterPerDay >=50){
        isRobotColumnPerDay = cbind(isRobotColumnPerDay, 1)
      } else {
        isRobotColumnPerDay = cbind(isRobotColumnPerDay,  0)
      }
      allCountsForOneIp= cbind(allCountsForOneIp,  downloadFileCounterPerDay)
    }
    counter = counter + 1
  }
  LM = t(isRobotColumnPerDay)
  NumberOfLogsPerIP = t(allCountsForOneIp)
  data = cbind(data, LM)
  data = cbind(data, NumberOfLogsPerIP)
  return(data)
}

dataWithFilters = filterData(data)
ee = (ddply(dataWithFilters,~ip,summarise,number_of_distinct_orders=length(unique(cik))))
eeT=hash(ee$ip, ee$number_of_distinct_orders)

res <- table(unlist(dataWithFilters$ip))
transposedData <- as.data.frame(t(dataWithFilters))

dataWithFilters$frequencyCounter <- lapply(transposedData, function(x) {
  return(res[x[[1]]][[1]])
})
dataNestoNesto = dataWithFilters
dataWithFilters$cikDownloaded <- lapply(transposedData, function(x) {
  return(eeT[x[[1]]][[x[[1]]]])
})
dataWithFilters$LM = ifelse(dataWithFilters$frequencyCounter >= 50, 1, 0)
# Dodati kolonu LM
#dataWithLMcolumnNewApp = removeRobotDataUsingLMApproach(dataWithFilters)
dataWithLMcolumn = as.data.frame(dataWithFilters)
# Promijeniti kolonu vrijeme da ima epoch vrijednost
dataWithLMcolumn$time = as.integer(as.POSIXct(paste(dataWithLMcolumn$date, dataWithLMcolumn$time)))

# Pretvoriti LM kolonu u tip faktor, radi lakšeg rada sa SVM funkcijama
dataWithLMcolumn$LM = as.factor(dataWithLMcolumn$LM)
dataWithLMcolumn$frequencyCounter = as.numeric(dataWithLMcolumn$frequencyCounter)
dataWithLMcolumn$cikDownloaded = as.numeric(dataWithLMcolumn$cikDownloaded)

# Ukloniti sve nekompletne logove
dataWithLMcolumn = dataWithLMcolumn[complete.cases(dataWithLMcolumn), ]

# Pretvoriti cik i time u numeričke vrijednosti
dataWithLMcolumn$time = as.double(dataWithLMcolumn$time)
dataWithLMcolumn$cik = as.double(dataWithLMcolumn$cik)

# Finalne izmjene za krajnji set podataka koji će se koristiti
dataForSvm = dataWithLMcolumn
dataForSvm = dataWithLMcolumn[order(-dataWithLMcolumn$time),]

write.csv(dataForSvm, '/Users/emina/Desktop/filtiraniPodaci.csv')
