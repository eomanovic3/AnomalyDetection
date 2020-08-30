# Master thesis work
# Omanovic Emina

install.packages('dplyr')
install.packages('stringr')
install.packages('caTools')
install.packages('anytime')
install.packages('e1071') 
install.packages('plyr') 
install.packages('hash') 
require(magrittr); require(tidyr)
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

#Filtrirati podatke na osnovu uvjeta u poglavlju 4.2.
dataWithFilters = filterData(data)

# Dodati kolonu sa vremenom u minutama
timeInMinutes = substr(as.character(dataWithFilters$time), 1, 5)
dataWithFilters = cbind(dataWithFilters, timeInMinutes)


# Izracunati broj ip adresa koje su kreirale rekord u jednom danu
ipCounter <- table(unlist(dataWithFilters$ip))

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

# Napraviti key - value strukture sa ip adresama i potrebnim vrijednostima
dictionaryForCik=hash(uniqueCikData$ip, uniqueCikData$number_of_distinct_orders)

# Potrebno je napraviti key za vise kolona za rjecnike vezane za minute i razlicite cik datoteka
makeProperKeysForMinutes = groupByMinutes %>% unite(ipKey, c("ip", "timeInMinutes"))
makeProperKeysForDifferentCiks = groupByMinutesAndCik %>% unite(ipKey, c("ip", "timeInMinutes", "cik"))
dictionaryForMinutes=data.frame(row.names=makeProperKeysForMinutes$ipKey , val=makeProperKeysForMinutes$count)
dictionaryForDifferentCiks=data.frame(row.names=makeProperKeysForDifferentCiks$ipKey , val=makeProperKeysForDifferentCiks$count)

# Transponovati set podataka, kako bi se moglo lakse manipulisati kolonama
transposedData <- as.data.frame(t(dataWithFilters))

# Dodati potrebne kolone za krajnju racunicu robot/covjek 
dataWithFilters$per_day_counter <- lapply(transposedData, function(x) {
  return(ipCounter[x[[1]]][[1]])
})

dataWithFilters$per_minute_counter <- lapply(transposedData, function(x) {
  properKey = (paste(x[[1]], x[[11]], sep="_"))
  print(properKey)
  return(dictionaryForMinutes[properKey, ])
})
dataWithFilters$per_cik_counter <- lapply(transposedData, function(x) {
  return(dictionaryForCik[x[[1]]][[x[[1]]]])
})

dataWithFilters$per_different_cik_counter <- lapply(transposedData, function(x) {
  properPartKey = (paste(x[[1]], x[[11]], sep="_"))
  numericCik=as.numeric(x[[4]])
  properKey = (paste(properPartKey, numericCik, sep="_"))
  print(properKey)
  return(dictionaryForDifferentCiks[properKey, ])
})
fff = dataWithFilters
write.csv(data.frame(lapply(fff, as.character), stringsAsFactors=FALSE), '/Users/emina/Desktop/fff.csv')

# Definisati za svaki pristup kolonu koja govori da li je rekord napravio covjek ili robot
dataWithFilters$LM = ifelse(dataWithFilters$per_day_counter >= 50, "robot", "human")
dataWithFilters$DRT = ifelse(dataWithFilters$per_minute_counter >= 5 & dataWithFilters$per_day_counter >= 1000, "robot", "human")
dataWithFilters$Ryans = ifelse(dataWithFilters$per_different_cik_counter >= 3 & 
                                 dataWithFilters$per_minute_counter >= 25 & 
                                 dataWithFilters$per_day_counter >=500, "robot", "human")

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
dataWithProperColumns$DRT = as.factor(dataWithProperColumns$DRT)
dataWithProperColumns$Ryans = as.factor(dataWithProperColumns$Ryans)
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
write.csv(data.frame(lapply(dataForSvm, as.character), stringsAsFactors=FALSE), '/Users/emina/Desktop/podaciSaNoveTriKolone.csv')

finalSvm =dataForSvm[dataForSvm$browser %in% c("lin"), ]
finalSvm <- finalSvm[finalSvm$find == 9, ]
finalSvm <- finalSvm[finalSvm$code == 200, ]
finalSvm$code = NULL
finalSvm$find = NULL
finalSvm$browser = NULL
write.csv(data.frame(lapply(finalSvm, as.character), stringsAsFactors=FALSE), '/Users/emina/Desktop/noviFinalniSetZaFiltiranje.csv')

newData4 <- finalSvm %>% group_by(LM) %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count)) 
newData5 <- finalSvm %>% group_by(DRT) %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count)) 
newData6 <- finalSvm %>% group_by(Ryans) %>% 
  summarize(count = n()) %>%  # count records by species
  mutate(pct = count/sum(count)) 

plot4 = ggplot(newData4, aes(LM, pct, fill = LM)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = .5))+
  scale_y_continuous(labels = scales::percent)
plot5 = ggplot(newData5, aes(DRT, pct, fill = DRT)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = .5))+
  scale_y_continuous(labels = scales::percent)

plot6 = ggplot(newData6, aes(Ryans, pct, fill = Ryans)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = .5))+
  scale_y_continuous(labels = scales::percent)
gridExtra::grid.arrange(plot4, plot5, plot6,
                        layout_matrix = matrix(c(1, 1, 2, 2, 3, 3), byrow = TRUE, ncol = 6),top= textGrob("Statistički prikaz pojave bota u setu podataka",gp=gpar(fontsize=17,font=3)))

table(dataForSvm$DRT)
table(dataForSvm$DRT)
table(dataForSvm$LM)
