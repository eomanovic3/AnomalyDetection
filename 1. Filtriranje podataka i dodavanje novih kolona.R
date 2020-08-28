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
detectRobotsUsingLMapproach<- function (data) {
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

#Filtrirati podatke na osnovu uvjeta u poglavlju 4.2.
dataWithFilters = filterData(data)

# Dodati kolonu sa vremenom u minutama
timeInMinutes = substr(as.character(dataWithFilters$time), 1, 5)
dataWithFilters = cbind(dataWithFilters, timeInMinutes)

# Izracunati broj unikatnih fajlova koji su preuzeti u minuti od strane ip adrese
groupByMinutesAndCik = dataWithFilters %>%
  group_by(ip, cik, timeInMinutes) %>% 
  summarize(count = n())

# Izracunati broj preuzimanja u minuti od strane ip adrese
groupByMinutes = dataWithFilters %>% # take the diamonds data.fram and group it
  group_by(ip, timeInMinutes) %>% # 56 groups
  summarize(count = n())

# Izracunati broj unikatnih fajlova po ip adresi
uniqueCikData = (ddply(dataWithFilters,~ip,summarise,number_of_distinct_orders=length(unique(cik))))

# Izracunati broj ip adresa koje su kreirale rekord u jednom danu
ipCounter <- table(unlist(dataWithFilters$ip))

# Napraviti key - value strukture sa ip adresama i potrebnim vrijednostima
dictionaryForCik=hash(uniqueCikData$ip, uniqueCikData$number_of_distinct_orders)
dictionaryForMinutes=hash(groupByMinutes$ip, groupByMinutes$count)
dictionaryForDifferentCiks=hash(groupByMinutesAndCik$ip, groupByMinutesAndCik$count)

# Transponovati set podataka, kako bi se moglo lakse manipulisati kolonama
transposedData <- as.data.frame(t(dataWithFilters))

# Dodati potrebne kolone za krajnju racunicu robot/covjek 
dataWithFilters$per_day_counter <- lapply(transposedData, function(x) {
  return(ipCounter[x[[1]]][[1]])
})
dataWithFilters$per_minute_counter <- lapply(transposedData, function(x) {
  return(dictionaryForMinutes[x[[1]]][[x[[1]]]])
})
dataWithFilters$per_cik_counter <- lapply(transposedData, function(x) {
  return(dictionaryForCik[x[[1]]][[x[[1]]]])
})
dataWithFilters$per_different_cik_counter <- lapply(transposedData, function(x) {
  return(dictionaryForDifferentCiks[x[[1]]][[x[[1]]]])
})

# Definisati za svaki pristup kolonu koja govori da li je rekord napravio covjek ili robot
dataWithFilters$LM = ifelse(dataWithFilters$per_day_counter >= 50, "robot", "human")
dataWithFilters$DRT = ifelse(dataWithFilters$per_minute_counter >= 5 && dataWithFilters$per_day_counter >=1000, "robot", "human")
dataWithFilters$Ryans = ifelse(dataWithFilters$per_different_cik_counter >= 3 && dataWithFilters$per_minute_counter >= 25 && dataWithFilters$per_day_counter >=500, "robot", "human")

# Uklanjanje kolone timeInMinutes i date te pretvaranje time kolone u sekunde
dataWithFilters$timeInMinutes = NULL
dataWithFilters$dateTime = paste(dataWithFilters$date, dataWithFilters$time)
dataWithFilters$epochTime = as.integer(as.POSIXct(dataWithFilters$dateTime))
dataWithFilters$time = dataWithFilters$epochTime
dataWithFilters$date = NULL
dataWithFilters$epochTime = NULL
dataWithFilters$dateTime = NULL

dataWithProperColumns = as.data.frame(dataWithFilters)

# Pretvoriti LM,DRT,Ryans kolonu u tip faktor, radi lakšeg rada sa SVM funkcijama
dataWithProperColumns$LM = as.factor(dataWithProperColumns$LM)
dataWithProperColumns$DRT = as.factor(dataWithProperColumns$LM)
dataWithProperColumns$Ryans = as.factor(dataWithProperColumns$LM)
dataWithProperColumns$per_different_cik_counter = as.numeric(dataWithProperColumns$per_different_cik_counter)
dataWithProperColumns$per_minute_counter = as.numeric(dataWithProperColumns$per_minute_counter)
dataWithProperColumns$per_day_counter = as.numeric(dataWithProperColumns$per_day_counter)
dataWithProperColumns$per_cik_counter = as.numeric(dataWithProperColumns$per_cik_counter)

# Ukloniti sve nekompletne logove
dataWithProperColumns = dataWithProperColumns[complete.cases(dataWithProperColumns), ]

# Pretvoriti cik i time u numeričke vrijednosti
dataWithProperColumns$time = as.double(dataWithProperColumns$time)
dataWithProperColumns$cik = as.double(dataWithProperColumns$cik)

# Finalne izmjene za krajnji set podataka koji će se koristiti
dataForSvm = dataWithProperColumns[order(-dataWithProperColumns$time),]
write.csv(dataForSvm, '/Users/emina/Desktop/podaciSaTriKolone.csv')

finalSvm =dataForSvm[dataForSvm$browser %in% c("lin"), ]
finalSvm <- finalSvm[finalSvm$find == 9, ]
finalSvm <- finalSvm[finalSvm$code == 200, ]

write.csv(finalSvm, '/Users/emina/Desktop/finalniSetZaFiltiranje.csv')
