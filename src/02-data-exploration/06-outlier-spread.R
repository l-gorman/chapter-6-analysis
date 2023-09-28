library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
# library(ggridges)
library(ggExtra)
library(RColorBrewer)
library(scales)
library(viridis)
dir.create("outputs/02-data-exploration/distributions/outlier_removal")


modelling_data_set <- readr::read_csv("./data/02-prepared-data/indicator_data_full.csv")

modelling_data_set$livestock_tlu

tva_dist <- ggplot(modelling_data_set,aes(x=tva_per_mae_per_day_ppp))+
  geom_histogram(center=0.25,binwidth = 1/2,color="black", fill="dodgerblue4")+
  scale_x_log10(breaks=c(0.001,0.01,0.1,1,10,100,1000,10000, 100000),labels = c("E-3","E-2","0.1","1","10","100","1k","10k","100k"))+
  labs(x="TVA (PPP$/MAE/day)", y="Frequency", title="TVA")+
theme(plot.title = element_text(hjust=0.5))

ggsave("outputs/02-data-exploration/distributions/outlier_removal/tva.png",tva_dist, width=1500,height=1200,units = "px")


landcult_dist <- ggplot(modelling_data_set,aes(x=land_cultivated_ha))+
  geom_histogram(binwidth = 1/4,color="black", fill="dodgerblue4")+
  scale_x_log10(limits=c(0.001,10000), breaks=c(0.001,0.01,0.1,1,10,100,1000,10000, 100000),labels = c("E-3","E-2","0.1","1","10","100","1k","10k","100k"))+
  labs(x="Land Cultivated (ha)", y="Frequency", title="Land Cultivated")+
  theme(plot.title = element_text(hjust=0.5))
ggsave("outputs/02-data-exploration/distributions/outlier_removal/landcult_dist.png",landcult_dist, width=1500,height=1200,units = "px")


hh_size_mae_dist <- ggplot(modelling_data_set,aes(x=hh_size_mae))+
  geom_histogram(center=0.05,binwidth = 1/10,color="black", fill="dodgerblue4")+
  scale_x_log10(breaks=c(0.5,1,2,4,8,16,32,64, 128,256),labels = c("0.5","1","2","4","8","16","32","64","128","256"))+
  labs(x="Household Size (MAE)", y="Frequency", title="Household Size")+
  theme(plot.title = element_text(hjust=0.5))
ggsave("outputs/02-data-exploration/distributions/outlier_removal/hh_size_mae_dist.png",hh_size_mae_dist, width=1500,height=1200,units = "px")


livestock_tlu_dist <- ggplot(modelling_data_set,aes(x=livestock_tlu))+
  geom_histogram(center=0.05,binwidth = 1/4,color="black", fill="dodgerblue4")+
  scale_x_log10(limits=c(0.0001,10000),breaks=c(0.001,0.01,0.1,1,10,100,1000),labels = c("E-3","E-2","0.1","1","10","100","1000"))+
  labs(x="Livestock Holdings (TLU)", y="Frequency", title="Livestock Holdings")+
  theme(plot.title = element_text(hjust=0.5))

livestock_tlu_dist
ggsave("outputs/02-data-exploration/distributions/outlier_removal/livestock_tlu_dist.png",livestock_tlu_dist, width=1500,height=1200,units = "px")


