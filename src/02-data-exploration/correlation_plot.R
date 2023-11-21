library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)

library(GGally)
library(ggcorrplot)
library(corrplot)
# library(ggridges)
library(ggExtra)
library(RColorBrewer)
library(scales)
library(viridis)
library(psych)
dir.create("outputs/02-data-exploration/distributions/correlation_plot")

indicator_data <- readr::read_csv("./data/02-prepared-data/modelling_df.csv")


hist(indicator_data$adjusted_length_growing_period[indicator_data$id_form=="rw_oaf_2018"])
hist(indicator_data$adjusted_length_growing_period)

median(indicator_data$adjusted_length_growing_period[indicator_data$id_form=="rw_oaf_2018" ])
median(indicator_data$adjusted_length_growing_period)


hist(indicator_data$land_cultivated_ha[indicator_data$id_form=="cd_lgs_2019" & indicator_data$land_cultivated_ha<1])
hist(indicator_data$land_cultivated_ha)
indicator_data$land_cultivated_ha[indicator_data$id_form=="cd_lgs_2019" & indicator_data$land_cultivated_ha>100]
indicator_data$land_cultivated_ha[indicator_data$id_form=="cd_lgs_2019" ]
median(indicator_data$land_cultivated_ha[indicator_data$id_form=="cd_lgs_2019" ])
median(indicator_data$land_cultivated_ha)

length(indicator_data$land_cultivated_ha[indicator_data$id_form=="cd_lgs_2019" ])

indicator_data$adjusted_length_growing_period




variables <- list(
  "Household Size"="hh_size",
  
  "Education"="education",
  # "Education (Pre Primary)"="education_cleanedpre_primary",
  # "Education (Primary)"="educationprimary",
  # "Education (Secondary/Higher)"="educationsecondary_or_higher",
  
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
  
  "Country HDI"="gdl_country_shdi"
  
)






subset <- indicator_data[as.character(variables)]
subset$education <- factor(subset$education, levels =c("pre_primary","primary", "secondary_or_higher"), ordered=T)
colnames(subset) <- names(variables)
subset$Education <- as.numeric(subset$Education)

cor_sub <- round(cor(subset), 2)

ggcorrplot(cor_sub,method = "circle")


png("./outputs/02-data-exploration/distributions/correlation_plot/corr_plot.png",height=500, width=500, units="px")
corrplot(cor(subset))
dev.off()
# ggsave("./outputs/02-data-exploration/distributions/correlation_plot/corr_plot.png",plot, )
# corrplot.mixed(cor(subset),
#                upper = "circle",
#                lower = "number",
#                
#                # addgrid.col = "black",
#                # tl.col = "black"
#                )
corr_plot <- ggcorr(subset, label=T,hjust=1, nbreaks = 4, palette = "RdGy") 
# corr_plot <- corr_plot + theme(plot.margin = unit(c(2,2,2,2), "cm"))
pairs.panels(subset)

temp <- cor(subset)
temp[temp>0.4]
