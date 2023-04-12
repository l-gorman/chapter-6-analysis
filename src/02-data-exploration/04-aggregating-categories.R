library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(flextable)
library(rhomis)
library(XML)


indicator_data <- readr::read_csv("./data/02-prepared-data/rhomis-spatial-merged.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$village),]
indicator_data <- indicator_data[!is.na(indicator_data$iso_country_code),]
indicator_data <- indicator_data[indicator_data$iso_country_code%in%c(
  "EC",
  "SN"
  # "IN",
  # "KH",
  # "PE",
  # "VN"
  )==F,]




# rhomis_data <- readr::read_csv("./data/01-raw-data/rhomis-data/rhomis/processed_data.csv")
# indicator_data <- rhomis_data %>% merge(indicator_data, by="id_unique",all.x = F, all.y=T)
# rhomis_data <- NULL


# Identifying categorical Variables ---------------------------------------

education <- indicator_data %>% count(education_level)
education_conversion <- tribble(
  ~education_level, ~education_cleaned,
  "no_answer",NA,
  
  "no_school","pre_primary",
  "none","pre_primary",
  "enrolled_not_completed",   "pre_primary",
  "illiterate",   "pre_primary",
  
  "primary","primary",
  "primary_1","primary",
  "primary_2","primary",
  "adult_education",   "primary",
  "coranic",   "primary",
  "islamic_school","primary",
  "koranic_school","primary",
  "religious_school","primary",
  "literate","primary",
  
  
  "secondary","secondary",
  "secondary_1","secondary",
  "secondary_2","secondary",
  "lower_secondary","secondary",
  "upper_secondary","secondary",
  "vocational","secondary",
  "technical","secondary",
  
  "college",   "post_secondary",
  "postsecondary","post_secondary",
  
)

indicator_data <- indicator_data %>% base::merge(education_conversion,by="education_level",all.x=T,all.y=F) %>% as_tibble()
indicator_data$education_level <- factor(indicator_data$education_level, levels=education_conversion$education_level, 
                                         ordered = T)
indicator_data$education_cleaned <- factor(indicator_data$education_cleaned, levels=unique(education_conversion$education_cleaned), 
                                           ordered = T)

cleaned_aggregation <-indicator_data[c("education_level","education_cleaned")] %>% 
  count(education_level,education_cleaned)
cleaned_aggregation <- na.omit(cleaned_aggregation)
cleaned_aggregation <- cleaned_aggregation[c("education_level","n","education_cleaned")]
colnames(cleaned_aggregation) <- c("Original Value", "Count", "Cleaned Value")
totals <- cleaned_aggregation %>% group_by(`Cleaned Value`) %>% summarise(Total=sum(Count))
cleaned_aggregation <- cleaned_aggregation %>% merge(totals,by="Cleaned Value",all.x=T,all.y=F) %>% as_tibble()

cleaned_aggregation <- cleaned_aggregation[c("Original Value","Count","Cleaned Value", "Total")]
cleaned_aggregation <- cleaned_aggregation[order(cleaned_aggregation$`Original Value`, cleaned_aggregation$`Cleaned Value`),]
cleaned_aggregation$`Cleaned Value`[duplicated(cleaned_aggregation$`Cleaned Value`)] <- NA
cleaned_aggregation$Total[is.na(cleaned_aggregation$`Cleaned Value`)] <- NA


above_row_selectors <- which(!is.na(cleaned_aggregation$`Cleaned Value`))-1
above_row_selectors <- above_row_selectors[above_row_selectors!=0]
bold_row_selectors <- which(!is.na(cleaned_aggregation$`Cleaned Value`))

cleaned_aggregation <- cleaned_aggregation %>% flextable::flextable() %>% 
  bold( bold = TRUE, part="header") %>% 
  hline(i = above_row_selectors)  %>% 
  bold(i = bold_row_selectors,j = c("Cleaned Value", "Total"))  

dir.create("./outputs/02-data-exploration/category_merging")
save_as_image(cleaned_aggregation, "./outputs/02-data-exploration/category_merging/education_merging.png")




# AEZ_categories ----------------------------------------------------------


# Creating a table to convert bads to characters
xml_33_list <-  xmlParse('data/01-raw-data/external-data/aez/LR/aez/aez_v9v2red_5m_ENSEMBLE_rcp2p6_2020s.tif.aux.xml')
xml_33_list <- xmlToList(xml_33_list)
xml_33_list <- xml_33_list$PAMRasterBand$GDALRasterAttributeTable
xml_33_list <- xml_33_list[names(xml_33_list)=="Row"]

aez_33_class_conversions <- lapply(c(1:length(xml_33_list)), function(index){
  row <- xml_33_list[index]$Row
  names_of_row <- names(row)
  features <- unlist(as.list(as.character(row[names(row)=="F"])))
  features <- c(features,row$.attrs[["index"]])
  feature_names <- paste0("feature_",c(1:length(features)))
  row_df <- tibble::as_tibble(list(
    var=feature_names,
    value=features
  )) %>% pivot_wider(names_from = "var")
  
  result <- row_df[c("feature_2", "feature_8")]
  colnames(result) <- c("band", "name")
  
  return(result)
})  %>% dplyr::bind_rows()


indicator_data <- indicator_data %>%  merge(aez_33_class_conversions,by.x="AEZ_Classes_33",by.y="band",all.x = T,all.y=F)
indicator_data <- indicator_data %>% rename(AEZ_name=name.y)


summary_aez <- indicator_data %>% group_by(iso_country_code)%>% count(AEZ_name)

classes_to_remove <- c(
  "Dominantly built-up land", #
  "Cold, no permafrost; moist",
  "Cold, no permafrost; wet",
  "Desert/Arid climate",
  "Dominantly built-up land",
  "Dominantly water"
)

indicator_data <- indicator_data[indicator_data$AEZ_name %in%classes_to_remove==F,]

# Converting bands to character
# aez_classes <- tibble::as_tibble(
#   list(
#     id_form=indicator_data$id_form,
#     id_unique=indicator_data$id_unique,
#     band=indicator_data$AEZ_Classes_33
#   )
# )




aez_aggregation <- tribble(
  ~aez_class, ~aez_class_cleaned,
  "Tropics, lowland; semi-arid", "semi_arid_or_arid",
  "Tropics, lowland; sub-humid", "sub_humid",
  "Tropics, lowland; humid", "humid",
  "Tropics, highland; semi-arid", "semi_arid_or_arid",
  "Tropics, highland; sub-humid", "sub_humid",
  "Tropics, highland; humid", "humid",
  "Sub-tropics, warm; semi-arid", "semi_arid_or_arid",
  "Sub-tropics, warm; humid", "humid",
  "Sub-tropics, moderately cool; semi-arid", "semi_arid_or_arid",
  "Sub-tropics, moderately cool; sub-humid", "sub_humid",
  "Sub-tropics, cool; semi-arid", "semi_arid_or_arid",
  "Cold, no permafrost; moist", NA,
  "Cold, no permafrost; wet", NA,
  "Dominantly very steep terrain", "land_with_limitations",
  "Land with severe soil/terrain limitations", "land_with_limitations",
  "Land with ample irrigated soils", "irrigated_soils",
  "Dominantly hydromorphic soils", "hydromorphic_soils",
  "Desert/Arid climate",NA,
  "Dominantly built-up land", NA,
  "Dominantly water",NA
  
)


ggplot(indicator_data, aes(x=AEZ_name))+
  geom_histogram(stat = "count")+theme(
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5)
  )

indicator_data <- indicator_data %>% base::merge(aez_aggregation,by.x="AEZ_name",by.y="aez_class",all.x=T,all.y=F) %>% as_tibble()

cleaned_aggregation <-indicator_data[c("AEZ_name","aez_class_cleaned")] %>% 
  count(AEZ_name,aez_class_cleaned)
# cleaned_aggregation <- na.omit(cleaned_aggregation)
cleaned_aggregation <- cleaned_aggregation[c("AEZ_name","n","aez_class_cleaned")]
colnames(cleaned_aggregation) <- c("Original Value", "Count", "Cleaned Value")
totals <- cleaned_aggregation %>% group_by(`Cleaned Value`) %>% summarise(Total=sum(Count))
cleaned_aggregation <- cleaned_aggregation %>% merge(totals,by="Cleaned Value",all.x=T,all.y=F) %>% as_tibble()

cleaned_aggregation <- cleaned_aggregation[c("Original Value","Count","Cleaned Value", "Total")]
cleaned_aggregation <- cleaned_aggregation[order(cleaned_aggregation$`Original Value`, cleaned_aggregation$`Cleaned Value`),]
cleaned_aggregation$`Cleaned Value`[duplicated(cleaned_aggregation$`Cleaned Value`)] <- NA
cleaned_aggregation$Total[is.na(cleaned_aggregation$`Cleaned Value`)] <- NA


above_row_selectors <- which(!is.na(cleaned_aggregation$`Cleaned Value`))-1
above_row_selectors <- above_row_selectors[above_row_selectors!=0]
bold_row_selectors <- which(!is.na(cleaned_aggregation$`Cleaned Value`))

cleaned_aggregation <- cleaned_aggregation %>% flextable::flextable() %>% 
  bold( bold = TRUE, part="header") %>% 
  hline(i = above_row_selectors)  %>% 
  bold(i = bold_row_selectors,j = c("Cleaned Value", "Total"))  

dir.create("./outputs/02-data-exploration/category_merging")
save_as_image(cleaned_aggregation, "./outputs/02-data-exploration/category_merging/aez_merging.png")



# Market Travel Time ------------------------------------------------------
travel_time_cols <- grep("travel_time", colnames(indicator_data), value=T)
min_travel_time <-  apply( indicator_data[travel_time_cols], 1, min)
indicator_data$min_travel_time <- min_travel_time
indicator_data <- indicator_data[indicator_data$min_travel_time!=0,]
indicator_data$log_min_travel_time <- log(indicator_data$min_travel_time)


# food security -----------------------------------------------------------

unique(indicator_data$hfias_status)

indicator_data$hfias_status[indicator_data$hfias_status=="food_secure"] <- "not_fi"

indicator_data$combined_fs_score <- indicator_data$hfias_status
indicator_data$combined_fs_score[!is.na(indicator_data$fies_score) & indicator_data$fies_score%in% c(0,1)] <- 'not_fi'
indicator_data$combined_fs_score[!is.na(indicator_data$fies_score) & indicator_data$fies_score%in% c(2,3)] <- 'mildly_fi'
indicator_data$combined_fs_score[!is.na(indicator_data$fies_score) & indicator_data$fies_score%in% c(4,5)] <- 'moderately_fi'
indicator_data$combined_fs_score[!is.na(indicator_data$fies_score) & indicator_data$fies_score%in% c(6,7,8)] <- 'severely_fi'



ggplot(indicator_data,aes(x=hfias_status))+
  geom_histogram(stat="count")

ggplot(indicator_data,aes(x=fies_score))+
  geom_histogram(stat="count")

ggplot(indicator_data,aes(x=combined_fs_score))+
  geom_histogram(stat="count")

# gini diversity ----------------------------------------------------------
# Feed the future income diversity
# https://sitoolkit.com/the-five-domains/economic/income-diversification/income-diversification-index#:~:text=This%20measure%20is%20unitless%20and,income%20diversified%20the%20household%20is.
x <- c(1,2,3,4,3,2,1,4)

#https://en.wikipedia.org/wiki/Gini_coefficient


# Gini Coefficient reversed
# So 1 is more diverse, and 0 is less diverse
diversity_index <- function(x) {
  if(all(is.na(x))){
    return(NA)
  }
  
  x <- x[!is.na(x)]
  x <- x[x!=0]
  if(length(x)==0){
    return(NA)
    
  }
  
  
  sorted_values <- sort(x)
  n <- length(x)
  
  numerator <- 2*sum(sorted_values*c(1:n))
  denominator <- n*sum(sorted_values)
  
  constant <- (n+1)/n
  
  
  gini_coefficient <- (numerator/denominator)-constant
  reverse_gini <- 1-gini_coefficient
  
  weighted_diversity_index <- reverse_gini*n
  return(weighted_diversity_index)
}




diversity <- function(indicator_data){
  crop_price_and_value <- map_to_wide_format(indicator_data,
                                             name_column = "crop_name",
                                             column_prefixes = c("crop_harvest_kg_per_year","mean_crop_price_lcu_per_kg"),
                                             types=c("num","num"))
  # plain_crop_diversity <- rowSums(!is.na(crop_price_and_value[[1]]))
  
  
  crop_value <- crop_price_and_value[["crop_harvest_kg_per_year"]]*crop_price_and_value[["mean_crop_price_lcu_per_kg"]]
  
  livestock <- map_to_wide_format(indicator_data,
                                  name_column = "livestock_name",
                                  column_prefixes = c(
                                    "livestock_sale_income",
                                    
                                    "meat_kg_per_year",
                                    "mean_meat_price_per_kg",
                                    
                                    "milk_collected_litres_per_year",
                                    "milk_price_per_litre",
                                    
                                    "eggs_collected_kg_per_year",
                                    "eggs_price_per_kg",
                                    
                                    "bees_honey_kg_per_year",
                                    "bees_honey_price_per_kg"),
                                  types=c("num","num","num","num","num","num","num","num","num"))
  # plain_crop_diversity <- rowSums(!is.na(crop_price_and_value[[1]]))
  
  livestock_value <- livestock[["livestock_sale_income"]]
  meat_value <- livestock[["meat_kg_per_year"]]*livestock[["mean_meat_price_per_kg"]]
  milk_value <- livestock[["milk_collected_litres_per_year"]]*livestock[["milk_price_per_litre"]]
  eggs_value <- livestock[["eggs_collected_kg_per_year"]]*livestock[["eggs_price_per_kg"]]
  honey_value <- livestock[["bees_honey_kg_per_year"]]*livestock[["bees_honey_price_per_kg"]]
  
  
  off_farm_income <- map_to_wide_format(indicator_data,
                                        name_column = "offfarm_income_name",
                                        column_prefixes = c("offfarm_income_name"),
                                        types=c("chr"))[["offfarm_income_name"]]
  off_farm_income[!is.na(off_farm_income)] <- "1"
  off_farm_income <- off_farm_income %>% mutate_all(as.numeric) 
  off_farm_income <- off_farm_income/rowSums(off_farm_income,na.rm=T)
  off_farm_value <- off_farm_income*indicator_data$off_farm_income_lcu_per_year
  
  
  value_matrix <- bind_cols(crop_value,
                            livestock_value,
                            meat_value,
                            milk_value,
                            eggs_value,
                            honey_value,
                            off_farm_value)
  
  indicator_data$weighted_income_diversity <- apply(value_matrix,1,diversity_index)
 
  return( indicator_data)
}

indicator_data <- diversity(indicator_data)

# Livestock Orientation, Market Orientation ----------------------------------------

subset_columns <- c("total_income_lcu_per_year","value_farm_products_consumed_lcu_per_hh_per_year")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$tva_per_hh_per_year <- rowSums(indicator_data[subset_columns], na.rm=T)
indicator_data$tva_per_hh_per_year[na.rows] <- NA

# Removing any households with zero value
indicator_data <- indicator_data[!is.na(indicator_data$tva_per_hh_per_year) & indicator_data$tva_per_hh_per_year>0,]


subset_columns <- c("tva_per_hh_per_year","hh_size_mae","currency_conversion_lcu_to_ppp")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$tva_per_mae_per_day_ppp <- indicator_data$tva_per_hh_per_year/indicator_data$hh_size_mae/365/indicator_data$currency_conversion_lcu_to_ppp
indicator_data$tva_per_mae_per_day_ppp[na.rows] <- NA

subset_columns <- c("livestock_income_lcu_per_year","value_livestock_products_consumed_lcu_per_hh_per_year")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$livestock_value_per_hh_per_year <- rowSums(indicator_data[subset_columns], na.rm=T)
indicator_data$livestock_value_per_hh_per_year[na.rows] <- NA

subset_columns <- c("crop_income_lcu_per_year","livestock_income_lcu_per_year")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$value_farm_products_sold_per_hh_per_year <- rowSums(indicator_data[subset_columns], na.rm=T)
indicator_data$value_farm_products_sold_per_hh_per_year[na.rows] <- NA


unit_conv_tibble <- rhomis::proportion_conversions
unit_conv_tibble$id_rhomis_dataset <- "x"
id_vector <- rep("x", nrow(indicator_data))
off_farm_prop <- indicator_data["offfarm_income_proportion"]
off_farm_incomes_any <- indicator_data["offfarm_incomes_any"]
off_farm_prop <- switch_units(off_farm_prop, unit_tibble = unit_conv_tibble, id_vector = id_vector)
off_farm_prop[off_farm_incomes_any=="n"] <- 0
indicator_data$offfarm_income_proportion <- off_farm_prop[[1]]


subset_columns <- c("livestock_value_per_hh_per_year","tva_per_hh_per_year")
indicator_data$livestock_orientation <- indicator_data[["livestock_value_per_hh_per_year"]]/indicator_data[["tva_per_hh_per_year"]]


subset_columns <- c("value_farm_products_sold_per_hh_per_year","tva_per_hh_per_year")
indicator_data$market_orientation <- indicator_data[["value_farm_products_sold_per_hh_per_year"]]/indicator_data[["tva_per_hh_per_year"]]


#Recoding Food Security Status





vars <- c(
  "id_form",
  "id_unique",
  "gdlcode",
  "iso_country_code",
  "village",
  "hh_size_mae",
  "livestock_orientation",
  "market_orientation",
  "offfarm_income_proportion",
  
  "weighted_income_diversity",
  
  "livestock_tlu",
  "tva_per_mae_per_day_ppp",
  "adjusted_length_growing_period",
  "log_min_travel_time",
  
  "aez_class_cleaned",
  
  "gdl_lifexp",
  "gdl_shdi",
  
  
  "combined_fs_score"
 
  )


modelling_data_set <- indicator_data[vars]
modelling_data_set <- modelling_data_set[!is.na(modelling_data_set$tva_per_mae_per_day_ppp),]
modelling_data_set <- modelling_data_set[!is.na(modelling_data_set$gdlcode),]
modelling_data_set <- modelling_data_set[!is.na(modelling_data_set$weighted_income_diversity),]
modelling_data_set <- modelling_data_set[!is.na(modelling_data_set$market_orientation),]
modelling_data_set <- modelling_data_set[!is.na(modelling_data_set$livestock_tlu),]




readr::write_csv(modelling_data_set,"data/02-prepared-data/modelling_df.csv")








