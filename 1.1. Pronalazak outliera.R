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
dataForSvmBefore = dataForSvm
dataForSvm= finalSvm
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

u03 = qplot(data = dataForSvm, y = per_day_counter, x = 1, geom = "boxplot", outlier.colour = "#E38942", ylab=NULL, main="per_day_counter") + 
  geom_text(stat = 'identity', position = 'identity', aes(label=ifelse(per_day_counter %in% boxplot.stats(per_day_counter)$out, as.character(per_day_counter), "")), hjust = 1.5, check_overlap = TRUE)+
  geom_boxplot(color="black", fill="#10afb4") +
  ggtitle("Per day counter") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_boxplot(geom = 'errorbar')
u03


u04 = qplot(data = dataForSvm, y = per_minute_counter, x = 1, geom = "boxplot", outlier.colour = "#E38942", ylab=NULL, main="per_minute_counter") + 
  geom_text(stat = 'identity', position = 'identity', aes(label=ifelse(per_minute_counter %in% boxplot.stats(per_minute_counter)$out, as.character(per_minute_counter), "")), hjust = 1.5, check_overlap = TRUE)+
  geom_boxplot(color="black", fill="#10afb4") +
  ggtitle("Per minute counter") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_boxplot(geom = 'errorbar')
u04


u05 = qplot(data = dataForSvm, y = per_cik_counter, x = 1, geom = "boxplot", outlier.colour = "#E38942", ylab=NULL, main="per_cik_counter") + 
  geom_text(stat = 'identity', position = 'identity', aes(label=ifelse(per_cik_counter %in% boxplot.stats(per_cik_counter)$out, as.character(per_cik_counter), "")), hjust = 1.5, check_overlap = TRUE)+
  geom_boxplot(color="black", fill="#10afb4") +
  ggtitle("Per cik counter") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_boxplot(geom = 'errorbar')
u05


u06 = qplot(data = dataForSvm, y = per_different_cik_counter, x = 1, geom = "boxplot", outlier.colour = "#E38942", ylab=NULL, main="per_different_cik_counter") + 
  geom_text(stat = 'identity', position = 'identity', aes(label=ifelse(per_different_cik_counter %in% boxplot.stats(per_different_cik_counter)$out, as.character(per_different_cik_counter), "")), hjust = 1.5, check_overlap = TRUE)+
  geom_boxplot(color="black", fill="#10afb4") +
  ggtitle("Per different cik in minute counter") +
  theme(plot.title = element_text(hjust = 0.5))+
  stat_boxplot(geom = 'errorbar')
u06

gridExtra::grid.arrange(u04, u05, u06, u03, u02, u01,
                        layout_matrix = matrix(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6), byrow = TRUE, ncol = 6),top= textGrob("Outlieri u setu podataka",gp=gpar(fontsize=17,font=3)))


# 1 1 2 2 3 3
# 4 5 5 6 6 7
boxplot(dataForSvm$per_day_counter)
summary(dataForSvm$per_day_counter)

boxplot(dataForSvm$per_minute_counter)
summary(dataForSvm$per_minute_counter)

boxplot(dataForSvm$per_cik_counter)
summary(dataForSvm$per_cik_counter)

boxplot(dataForSvm$per_different_cik_counter)
summary(dataForSvm$per_different_cik_counter)

boxplot(dataForSvm$cik)
summary(dataForSvm$cik)

boxplot(dataForSvm$time)
summary(dataForSvm$time)

length(boxplot(dataForSvm$cik)$out)
length(boxplot(dataForSvm$per_minute_counter)$out)
ccc = c(1,2,3,4,5000, 200000000)
length(boxplot(ccc)$out)

