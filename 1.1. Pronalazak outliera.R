#BOXPLOT
install.packages('sitools')
install.packages('tools')
install.packages('plyr')
library(ggplot2)
library(directlabels)
library(gridExtra)
library(grid)
library(ggplot2)
library(sitools)
library(ggrepel)
library(scales)
library(plyr)
dataForSvm = dataForRapidMiner

u01 = qplot(data = dataForSvm, y = cik, x = 1, geom = "boxplot", outlier.colour = "#E38942", ylab=NULL, main="cik") + 
  geom_text(stat = 'identity', position = 'identity', aes(label=ifelse(cik %in% boxplot.stats(cik)$out, as.character(cik), "")), hjust = 1.5, check_overlap = TRUE)+
  geom_boxplot(color="black", fill="#10afb4") +
  ggtitle("cik") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_boxplot(geom = 'errorbar')
u01

u02 = qplot(data = dataForSvm, y = size, x = 1, geom = "boxplot", outlier.colour = "#E38942", ylab=NULL, main="size") + 
  geom_text(stat = 'identity', position = 'identity', aes(label=ifelse(size %in% boxplot.stats(size)$out, as.character(size), "")), hjust = 1.5, check_overlap = TRUE)+
  geom_boxplot(color="black", fill="#10afb4") +
  ggtitle("size") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_boxplot(geom = 'errorbar')
u02

u03 = qplot(data = dataForSvm, y = frequencyCounter, x = 1, geom = "boxplot", outlier.colour = "#E38942", ylab=NULL, main="frequencyCounter") + 
  geom_text(stat = 'identity', position = 'identity', aes(label=ifelse(frequencyCounter %in% boxplot.stats(frequencyCounter)$out, as.character(frequencyCounter), "")), hjust = 1.5, check_overlap = TRUE)+
  geom_boxplot(color="black", fill="#10afb4") +
  ggtitle("frequencyCounter") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_boxplot(geom = 'errorbar')
u03


u04 = qplot(data = dataForSvm, y = cikDownloaded, x = 1, geom = "boxplot", outlier.colour = "#E38942", ylab=NULL, main="cikDownloaded") + 
  geom_text(stat = 'identity', position = 'identity', aes(label=ifelse(cikDownloaded %in% boxplot.stats(cikDownloaded)$out, as.character(cikDownloaded), "")), hjust = 1.5, check_overlap = TRUE)+
  geom_boxplot(color="black", fill="#10afb4") +
  ggtitle("cikDownloaded") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_boxplot(geom = 'errorbar')
u04


u05 = qplot(data = dataForSvm, y = time, x = 1, geom = "boxplot", outlier.colour = "#E38942", ylab=NULL, main="time") + 
  geom_text(stat = 'identity', position = 'identity', aes(label=ifelse(time %in% boxplot.stats(time)$out, as.character(time), "")), hjust = 1.5, check_overlap = TRUE)+
  geom_boxplot(color="black", fill="#10afb4") +
  ggtitle("time") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_boxplot(geom = 'errorbar')
u05

gridExtra::grid.arrange(u03, u04, u05, grid::nullGrob(), u02, u01, grid::nullGrob(),
                        layout_matrix = matrix(c(1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 6, 7), byrow = TRUE, ncol = 6),top= textGrob("Outlieri u setu podataka",gp=gpar(fontsize=17,font=3)))


# 1 1 2 2
# 3 3 4 4
boxplot(dataForSvm$frequencyCounter)
summary(dataForSvm$frequencyCounter)

boxplot(dataForSvm$cikDownloaded)
summary(dataForSvm$cikDownloaded)

boxplot(dataForSvm$size)
summary(dataForSvm$size)

boxplot(dataForSvm$cik)
summary(dataForSvm$cik)
dataForSvm$time = as.double(dataForSvm$time)
boxplot(dataForSvm$time)
summary(dataForSvm$time)

summary(dataForSvm$cik)
summary(dataForSvm$cik)
summary(dataForSvm$cik)
length(boxplot(dataForSvm$cik)$out)
length(boxplot(dataForSvm$size)$out)
length(boxplot(ipFrequency$freq)$out)
ccc = c(1,2,3,4,5000, 200000000)
length(boxplot(ccc)$out)

