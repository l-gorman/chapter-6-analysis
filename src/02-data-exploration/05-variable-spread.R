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

modelling_data_set <- readr::read_csv("./data/02-prepared-data/rhomis-spatial-merged.csv")

numeric_summary <- function(col){
  
  if (median(col)<1){
    return(paste0(format(round(median(col),2),big.mark = ",")," (",format(round(IQR(col),2),big.mark = ",") ,")"))
  }
  
  if (median(col)<10){
    return(paste0(format(round(median(col),1),big.mark = ",")," (",format(round(IQR(col),1),big.mark = ",") ,")"))
  }
  
  if (median(col)>10){
    return(paste0(format(round(median(col)),big.mark = ",")," (",format(round(IQR(col)),big.mark = ",") ,")"))
  }
  
}

class_summary<- function(col){
  col <- gsub("pre_primary", "PP",col) 
  col <- gsub("primary", "P",col) 
  col <- gsub("secondary_or_higher", "S",col) 
  
  counts <- as.data.frame(table(col))
  counts <- counts[order(counts$Freq,decreasing = T),]
  
  temp <- paste0(counts$col, ": ",format(counts$Freq,big.mark = ","),"new_line")
  temp <- paste0(temp, collapse=", ")
  
  # temp <- format(temp, big.mark = ",")
   
  
  
  return(temp)
}

binary_summary <- function(col){
  
  # col <- modelling_data_set$external_labour
  # statement <- "Access to labour"
  
  num <- round(100*sum(col)/length(col))
  num <- paste0(num,"%")

  return(as.character(num))
}

kg_clas_country <- as.data.frame(table(modelling_data_set$kg_class_name,modelling_data_set$iso_country_code))


vars <- c("hh_size_mae",
          "education_cleaned",
          "livestock_tlu",
          "land_cultivated_ha",
          "off_farm_any",
          "market_orientation",
          "assisted_tillage",
          "external_labour",
          "debts_have",
          "use_fert",
          "number_income_sources",
          "livestock_inputs_any",
          "land_irrigated_any",
          "kitchen_garden",
          
          "tva_per_mae_per_day_ppp",
          "hdds_lean_season")

modelling_data_set %>% group_by(iso_code) %>% 
  summarise(
    adjusted_length_growing_period=mean(adjusted_length_growing_period,na.rm=T),
    household_size=mean(hh_size_mae,na.rm=T)
    
  )


table(modelling_data_set$iso_code,modelling_data_set$education_cleaned)




# col <- modelling_data_set$education_cleaned 

# df_list <- list()
# count <- 1
# for (i in unique(modelling_data_set$iso_code)){
# 
#   temp_df <- modelling_data_set[modelling_data_set$iso_code==i,]
# temp <- tibble::tribble(
#   ~Category,~`Sub-Category`,~variable, ~summary,
#   "Where You are","Village-Level  Variables","Growing Period",numeric_summary(temp_df$adjusted_length_growing_period),
#   "","","Travel Time",numeric_summary(temp_df$min_travel_time),
#   "","Country-Level Descriptors","HDI",numeric_summary(temp_df$gdl_shdi),
#   "What You Have","Demographics","Household Size",numeric_summary(temp_df$hh_size_mae),
#   "","","Education",class_summary(temp_df$education_cleaned),
#   "","Labour","Access to External Labour", binary_summary( temp_df$external_labour),
#   "","","Use assisted tillage", binary_summary( temp_df$assisted_tillage),
#   "","Resource Endowment","Livestock Holdings (TLU)",numeric_summary(temp_df$livestock_tlu),
#   "","","Land Cultivated",numeric_summary(temp_df$land_cultivated_ha),
#   "","Credit","Has Debts", binary_summary( temp_df$debts_have),
#   "","Inputs","Use Livestock Inputs", binary_summary( temp_df$livestock_inputs_any),
#   "","","Use Fertiliser", binary_summary( temp_df$use_fert),
#   "","","Irrigate Land",binary_summary( temp_df$land_irrigated_any),
#   "What You Do","Production Orientation","Has Off-farm Income",binary_summary( temp_df$off_farm_any),
#   "","","Number Income Sources",numeric_summary(temp_df$number_income_sources),
#   
#   "","","Has Kitchen Garden",binary_summary(temp_df$kitchen_garden),
#   "","","Market Orientation",numeric_summary(temp_df$market_orientation),
#   
#   "Performance Indicators","","TVA",numeric_summary(temp_df$tva_per_mae_per_day_ppp),
#   "","","HDDS",numeric_summary(temp_df$hdds_lean_season))
#   
#   
#   colnames(temp) <- gsub("summary",i, colnames(temp))
#   
#   if (count==1){
#     df_list[[count]] <- temp
#   }
#   
#   if (count>1){
#     
#     df_list[[count]] <- temp[i]
#     
#     
#   }
#   count <- count +1
# }
# 
# var_summary <- df_list %>% bind_cols()
# 
# 
# var_summary$Category <- NULL
# var_summary$`Sub-Category` <- NULL
# 
# var_summary <- as_tibble(cbind(nms = names(var_summary), t(var_summary)))


var_summary <- tibble::tribble(
  ~Category,~`Sub-Category`,~variable, ~summary,
  "Where You are","Village-Level  Variables","Growing Period",numeric_summary(modelling_data_set$adjusted_length_growing_period),
  "","","Travel Time",numeric_summary(modelling_data_set$min_travel_time),
  "","Country-Level Descriptors","HDI",numeric_summary(modelling_data_set$gdl_shdi),
  "What You Have","Demographics","Household Size",numeric_summary(modelling_data_set$hh_size_mae),
  "","","Education",class_summary(modelling_data_set$education_cleaned),
  "","Labour","Access to External Labour", binary_summary( modelling_data_set$external_labour),
  "","","Use assisted tillage", binary_summary( modelling_data_set$assisted_tillage),
  "","Resource Endowment","Livestock Holdings (TLU)",numeric_summary(modelling_data_set$livestock_tlu),
  "","","Land Cultivated",numeric_summary(modelling_data_set$land_cultivated_ha),
  "","Credit","Has Debts", binary_summary( modelling_data_set$debts_have),
  "","Inputs","Use Livestock Inputs", binary_summary( modelling_data_set$livestock_inputs_any),
  "","","Use Fertiliser", binary_summary( modelling_data_set$use_fert),
  "","","Irrigate Land",binary_summary( modelling_data_set$land_irrigated_any),
  "What You Do","Production Orientation","Has Off-farm Income",binary_summary( modelling_data_set$off_farm_any),
  "","","Number Income Sources",numeric_summary(modelling_data_set$number_income_sources),
  
  "","","Has Kitchen Garden",binary_summary(modelling_data_set$kitchen_garden),
  "","","Market Orientation",numeric_summary(modelling_data_set$market_orientation),
  
  "Performance Indicators","","TVA",numeric_summary(modelling_data_set$tva_per_mae_per_day_ppp),
  "","","HDDS",numeric_summary(modelling_data_set$hdds_lean_season))


write_csv(var_summary,"./outputs/02-data-exploration/distributions/variable_summary_all.csv")

median(modelling_data_set$tva_per_mae_per_day_ppp)

tva_distribution <- ggplot(modelling_data_set,aes(y=tva_per_mae_per_day_ppp, x=iso_code))+
  geom_point(alpha=0.5,position = position_jitter(seed = 1, width = 0.1))+
  
  geom_violin(alpha=0.6)+
  
  # ylim(0,5)+ 
  scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000,10000),labels = c("0.01","0.1","1","10","100","1,000","10,000"))+
  labs(x="Country", y="TVA (PPP$/MAE/day)", title="Distribution of TVA per Country")+
  theme(plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle=45, hjust=1))

dir.create("./outputs/02-data-exploration/distributions/")
ggsave("./outputs/02-data-exploration/distributions/TVA_dist.png", tva_distribution,width=1000, height=1250, units = "px")


hdds_distribution <- ggplot(modelling_data_set,aes(x=hdds_lean_season))+

  geom_histogram(binwidth=1,center=0.25,bins=10, fill="dodgerblue4", color="black")+
  
  # ylim(0,5)+ 
  # scale_y_log10(breaks=c(0.01,0.1,1,10,100,1000,10000),labels = c("0.01","0.1","1","10","100","1,000","10,000"))+
  # labs(x="Country", y="TVA (PPP$/MAE/day)", title="Distribution of TVA per Country")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(x="HDDS (Lean Season)", y="Frequency", title="Distribution of HDDS per Country")+
  
  facet_wrap(~iso_code)

ggsave("./outputs/02-data-exploration/distributions/HDDS_dist.png", hdds_distribution,width=1000, height=1250, units = "px")



# Indicator Heatmap

modelling_data_set <- readr::read_csv("./data/02-prepared-data/modelling_df.csv")


modelling_data_set$region <- NA
modelling_data_set$region <- modelling_data_set$iso_country_code()


modelling_data_set$education_primary <- 

vars <- c("tva","hdds",
          "hh_size" ,

          #Assets
          "livestock_tlu" ,
          "land_cultivated" ,
          "market_orientation",
          "debts_have",
          
          "off_farm_any",
          "kitchen_garden",
          "number_income_sources",
          "market_orientation",

          # Practices
          "assisted_tillage",
          "external_labour",
          "livestock_inputs_any",
          "land_irrigated_any",
          "use_fert",
          
          #------------------
          # Village Level
          "length_growing_period",
          "min_travel_time",
          # log_pop_dens ,
          #------------------
          #County Level
          "gdl_country_shdi")

modelling_data_set[num_vars]
scaled_df <- scale(modelling_data_set[num_vars]) %>% as_tibble(scaled_df)
scaled_df$Country <- modelling_data_set$iso_country_code
scaled_df$Region <- NA 
scaled_df$Region[scaled_df$Country%in%c("BF","GH","ML","NE","NG")] <- "West Africa"
scaled_df$Region[scaled_df$Country%in%c("ET","KE","UG","TZ","MW","ZM")]<- "East Africa"
scaled_df$Region[scaled_df$Country%in%c("BI","CD","RW")]<- "Central Africa"
scaled_df$Region[scaled_df$Country%in%c("KH","VN")] <- "South-East Asia"


variables_switch <- list(
  # "Education (Pre Primary)"="education_cleanedpre_primary",
  "Household Size"="hh_size",
  
  "Education (Primary)"="educationprimary",
  "Education (Secondary/Higher)"="educationsecondary_or_higher",
  
  "Livestock TLU"="livestock_tlu",
  "Land Cultivated"="land_cultivated",
  "Any Off Farm Income"= "off_farm_any",
  "Assisted Tillage"="assisted_tillage",
  "External Labour"="external_labour",
  # "Use Pesticide"="pesticide",
  "Have Debts"="debts_have",
  # "Received Aid"="aidreceived",
  "Use Livestock Inputs"="livestock_inputs_any",
  "Irrigate Land"="land_irrigated_any",
  "Use Fertiliser"="use_fert",
  
  
  "Market Orientation"="market_orientation",
  "Home Garden"="kitchen_garden",
  "Number of Income Sources"="number_income_sources",
  
  "Growing Period"="length_growing_period",
  "Minimum Travel Time"="min_travel_time",
  
  "Country HDI"="gdl_country_shdi",
  
  "tva"="TVA",
  "hdds"="HDDS"
)





summary_all <- scaled_df %>% group_by(Region,Country) %>% 
  summarise_all(mean)

summary_all <- summary_all %>% melt(id.vars = c("Region","Country"))

# summary_all$Country <- factor(summary_all$Country, ordered=T, levels=c("BF","GH","ML","NE","NG",
#                                                                    "BI","CD","RW",
#                                                                    "ET","KE","UG","TZ","MW","ZM",
#                                                                    "KH","VN"))

summary_all$Region <- factor(summary_all$Region, ordered=T, levels=c("West Africa",
                                                                 "Central Africa",
                                                                 "East Africa",
                                                                "South-East Asia"))

summary_all$variable <- as.character(summary_all$variable)
for (i in 1:length(variables_switch)){
  
  new_name <- as.character(variables_switch)[i]
var_name <- names(variables_switch)[i]
  
  
  summary_all$variable[summary_all$variable==var_name] <- new_name
  
}


heatmap <- ggplot(summary_all, aes(y=variable, x=Country, fill=value,label = Country))+
  geom_tile()+
  scale_fill_viridis_c() +
  labs(title="Distribution of Numeric Indicators")+
  facet_grid(~Region, scale="free_x",space="free_x")

ggsave("./outputs/02-data-exploration/distributions/per_country_heatmap.png",width=2000,height=1500,units="px")


