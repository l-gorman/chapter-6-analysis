library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(flextable)
library(rhomis)


indicator_data <- readr::read_csv("./data/02-prepared-data/rhomis-spatial-merged.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$village),]
indicator_data <- indicator_data[!is.na(indicator_data$iso_country_code),]
indicator_data <- indicator_data[indicator_data$iso_country_code!="EC",]

indicator_data <- indicator_data[indicator_data$iso_country_code!="SN",]


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


# Livestock Orientation, Market Orientation ----------------------------------------

subset_columns <- c("total_income_lcu_per_year","value_farm_products_consumed_lcu_per_hh_per_year")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$tva_per_hh_per_year <- rowSums(indicator_data[subset_columns], na.rm=T)
indicator_data$tva_per_hh_per_year[na.rows] <- NA

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
indicator_data$offfarm_income_proportion <- off_farm_prop


subset_columns <- c("livestock_value_per_hh_per_year","tva_per_hh_per_year")
indicator_data$livestock_orientation <- indicator_data[["livestock_value_per_hh_per_year"]]/indicator_data[["tva_per_hh_per_year"]]


subset_columns <- c("value_farm_products_sold_per_hh_per_year","tva_per_hh_per_year")
indicator_data$market_orientation <- indicator_data[["value_farm_products_sold_per_hh_per_year"]]/indicator_data[["tva_per_hh_per_year"]]








