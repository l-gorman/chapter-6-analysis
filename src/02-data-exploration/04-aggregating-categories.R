library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(flextable)
library(rhomis)
library(XML)
library(GGally)

indicator_data <- readr::read_csv("./data/02-prepared-data/rhomis-spatial-merged.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$village),]
indicator_data <- indicator_data[!is.na(indicator_data$iso_country_code),]
# indicator_data <- indicator_data[indicator_data$iso_country_code%in%c(
#   "EC",
#   "SN"
#   # "IN",
#   # "KH",
#   # "PE",
#   # "VN"
# )==F,]




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
  
  
  "secondary","secondary_or_higher",
  "secondary_1","secondary_or_higher",
  "secondary_2","secondary_or_higher",
  "lower_secondary","secondary_or_higher",
  "upper_secondary","secondary_or_higher",
  "vocational","secondary_or_higher",
  "technical","secondary_or_higher",
  "college",   "secondary_or_higher",
  "postsecondary","secondary_or_higher"
  
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





# Market Travel Time ------------------------------------------------------
travel_time_cols <- grep("travel_time", colnames(indicator_data), value=T)
min_travel_time <-  apply( indicator_data[travel_time_cols], 1, min)
indicator_data$min_travel_time <- min_travel_time

# food security -----------------------------------------------------------


# gini diversity ----------------------------------------------------------
# Feed the future income diversity
# https://sitoolkit.com/the-five-domains/economic/income-diversification/income-diversification-index#:~:text=This%20measure%20is%20unitless%20and,income%20diversified%20the%20household%20is.
# https://en.wikipedia.org/wiki/Gini_coefficient


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

# Tva per hh per year (LCU)
subset_columns <- c("total_income_lcu_per_year","value_farm_products_consumed_lcu_per_hh_per_year")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$tva_per_hh_per_year <- rowSums(indicator_data[subset_columns], na.rm=T)
indicator_data$tva_per_hh_per_year[na.rows] <- NA

# Tva per mae per day in PPP
subset_columns <- c("tva_per_hh_per_year","hh_size_mae","currency_conversion_lcu_to_ppp")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$tva_per_mae_per_day_ppp <- indicator_data$tva_per_hh_per_year/indicator_data$hh_size_mae/365/indicator_data$currency_conversion_lcu_to_ppp
indicator_data$tva_per_mae_per_day_ppp[na.rows] <- NA

# Income per mae per day (ppp)
subset_columns <- c("total_income_lcu_per_year","hh_size_mae","currency_conversion_lcu_to_ppp")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$income_per_mae_per_day_ppp <- indicator_data$total_income_lcu_per_year/indicator_data$hh_size_mae/365/indicator_data$currency_conversion_lcu_to_ppp
indicator_data$income_per_mae_per_day_ppp[na.rows] <- NA


#Crop Income per mae per day
subset_columns <- c("crop_income_lcu_per_year","hh_size_mae","currency_conversion_lcu_to_ppp")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$crop_income_per_mae_per_day_ppp <- indicator_data$crop_income_lcu_per_year/indicator_data$hh_size_mae/365/indicator_data$currency_conversion_lcu_to_ppp
indicator_data$crop_income_per_mae_per_day_ppp[na.rows] <- NA

# Crop value total per hh per year (LCU)
subset_columns <- c("crop_income_lcu_per_year","value_crop_consumed_lcu_per_hh_per_year")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$crop_value_per_hh_per_year <- rowSums(indicator_data[subset_columns], na.rm=T)
indicator_data$crop_value_per_hh_per_year[na.rows] <- NA

# Livestock value per hh per year (LCU)
subset_columns <- c("livestock_income_lcu_per_year","value_livestock_products_consumed_lcu_per_hh_per_year")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$livestock_value_per_hh_per_year <- rowSums(indicator_data[subset_columns], na.rm=T)
indicator_data$livestock_value_per_hh_per_year[na.rows] <- NA

# Farm income per hh per year (LCU)
subset_columns <- c("crop_income_lcu_per_year","livestock_income_lcu_per_year")
na.rows <- rowSums(is.na(indicator_data[subset_columns]))==length(subset_columns)
indicator_data$value_farm_products_sold_per_hh_per_year <- rowSums(indicator_data[subset_columns], na.rm=T)
indicator_data$value_farm_products_sold_per_hh_per_year[na.rows] <- NA

#Livestock Orientation
subset_columns <- c("livestock_value_per_hh_per_year","tva_per_hh_per_year")
indicator_data$livestock_orientation <- indicator_data[["livestock_value_per_hh_per_year"]]/indicator_data[["tva_per_hh_per_year"]]

# Crop Orientation
subset_columns <- c("crop_value_per_hh_per_year","tva_per_hh_per_year")
indicator_data$crop_orientation <- indicator_data[["crop_value_per_hh_per_year"]]/indicator_data[["tva_per_hh_per_year"]]

# Off farm orientation
subset_columns <- c("off_farm_income_lcu_per_year","tva_per_hh_per_year")
indicator_data$off_farm_orientation <- indicator_data[["off_farm_income_lcu_per_year"]]/indicator_data[["tva_per_hh_per_year"]]



# Market orientation
subset_columns <- c("value_farm_products_sold_per_hh_per_year","tva_per_hh_per_year")
indicator_data$market_orientation <- indicator_data[["value_farm_products_sold_per_hh_per_year"]]/indicator_data[["tva_per_hh_per_year"]]


indicator_data$proportion_female_control <- indicator_data$proportion_of_value_controlled_female_youth+
  indicator_data$proportion_of_value_controlled_female_adult



vars <- c(
  "id_form",
  "id_unique",
  "gdlcode",
  "iso_country_code",
  "village",
  
  "hh_size_mae",
  "education_cleaned",
  "livestock_tlu", # centered transform
  "land_cultivated_ha",
  "proportion_female_control",
  
  "livestock_orientation", #logit transform
  "crop_orientation", #logit transform
  "off_farm_orientation", #logit transform
  "market_orientation", # logit transform
  
  "weighted_income_diversity", # centred transform
  
  "tva_per_mae_per_day_ppp", # centered transform

  # Village Level Variables
  "adjusted_length_growing_period", # centered transform
  "min_travel_time", # centered transform
  "kg_class_name",
  
  # Country Level Variables
  "gdl_lifexp",
  "gdl_shdi"
  
  
)
modelling_data_set <- indicator_data[vars]




variable_summary <- function(
    df,
    variable,
    description,
    level,
    criteria,
    criteria_description,
    actions,
    justification
    
){
  df[[variable]][is.infinite(df[[variable]])] <- NA
  
  summary <- list(
    Variable=variable,
    Min=min(df[[variable]],na.rm=T),
    Max=max(df[[variable]],na.rm=T),
    Mean=mean(df[[variable]],na.rm=T),
    SD=sd(df[[variable]],na.rm=T),
    Q.01=quantile(df[[variable]],probs=c(0.01),na.rm=T),
    Q.05=quantile(df[[variable]],probs=c(0.05),na.rm=T),
    Q.95=quantile(df[[variable]],probs=c(0.95),na.rm=T),
    Q.99=quantile(df[[variable]],probs=c(0.99),na.rm=T)
  ) %>% as_tibble()
  
  
  
  if(length(criteria)>1){
    summary_addition <- list(
      Variable=rep(NA,length(criteria)-1),
      Min=rep(NA,length(criteria)-1),
      Max=rep(NA,length(criteria)-1),
      Mean=rep(NA,length(criteria)-1),
      SD=rep(NA,length(criteria)-1),
      Q.01=rep(NA,length(criteria)-1),
      Q.05=rep(NA,length(criteria)-1),
      Q.95=rep(NA,length(criteria)-1),
      Q.99=rep(NA,length(criteria)-1)
    )%>% as_tibble()
    
    summary <- bind_rows(summary,summary_addition)
  }
  
  exclusion_list <- list()
  
  for (i in 1:length(criteria)){
    temp_function <- criteria[[i]]
    result <- temp_function(df[[variable]])
    number_of_values <- table(result)["TRUE"]
    if (is.na(number_of_values)){
      number_of_values <- 0
    }
    
    exclusion_list[[i]] <-list(
      Criteria=criteria_description[[i]],
      Action=actions[[i]],
      Justification=justification[[i]],
      Count=as.numeric(number_of_values)
    )
  }
  
  exclusion_list <- bind_rows(exclusion_list)
  
  summary <- bind_cols(summary,exclusion_list)
  
  return(summary)
  
  
}

hh_size_summary <-variable_summary(
  df=modelling_data_set,
  variable="hh_size_mae",
  description="Number of Household Members in MAE",
  level="Household Level",
  criteria=list(
    function(x){is.na(x)},
    function(x){x==0},
    function(x){x>30}),
  
  criteria_description=c("Null Value", "Zero Value","Above 30 MAE"),
  actions=c("Exclude", "Exclude", "Exclude"),
  justification=c(
    "Houeshold must have a size to be considered a household",
    "Household cannot be considered if it has no members",
    
    "Less than 1% of households above this amount. Likely that household of this size due to error"
  )
)


tlu_summary <-variable_summary(
  df=modelling_data_set,
  variable="livestock_tlu",
  description="",
  level="Household Level",
  criteria=list(
    function(x){is.na(x)},
    function(x){x>30}),
  
  criteria_description=c("Null Value", "Below 30 TLU"),
  actions=c("NAs will be converted to 0","Excluded"),
  justification=c(
    "",
    "Less than 1% of households above this amount. Likely that household of this size due to error"
  )
)


tva_summary <-variable_summary(
  df=modelling_data_set,
  variable="tva_per_mae_per_day_ppp",
  description="",
  level="Household Level",
  criteria=list(
    function(x){is.na(x)},
    function(x){x==0},
    function(x){x>10000}),
  
  criteria_description=c("Null Value", "Zero Value","Reported than 10,000 (PPP$/MAE/Day)"),
  actions=c("Excluded","Excluded","Excluded"),
  
  justification=c(
    "Value a key performance indicator",
    "Household has to have at least some farm produce",
    "Less than 1% of households above this amount. Likely that farming household not earning this much per person per day"
  )
)


growing_period_summary <-variable_summary(
  df=modelling_data_set,
  variable="adjusted_length_growing_period",
  description="",
  level="Village Level",
  criteria=list(
    function(x){is.na(x)},
    function(x){x>365}),
  
  criteria_description=c("Null Value", "Growing period of more than 365 days"),
  actions=c("Exclude", "Exclude"),
  
  justification=c(
    "Cannot have no growing period",
    "Cannot have growing period of more than 365 days"
  )
)

travel_time_summary <-variable_summary(
  df=modelling_data_set,
  variable="min_travel_time",
  description="",
  level="Village Level",
  criteria=list(
    function(x){is.na(x)},
    function(x){x>2880}),
  
  criteria_description=c("Null Value", "Travel time more than 48 hours"),
  actions=c("Exclude","Exclude"),
  
  justification=c(
    "NAs will removed",
    "Unlikely more than 48 hours travel time to closest town"
  )
)


hdi_summary <-variable_summary(
  df=modelling_data_set,
  variable="gdl_shdi",
  description="",
  level="Village Level",
  criteria=list(
    function(x){is.na(x)},
    function(x){x>1}),
  
  criteria_description=c("Null Value", "More than 1"),
  actions=c("Exclude", "Exclude"),
  
  justification=c(
    "Only 1 household",
    "Proportional measure, cannot be greater than 1"
  )
)



life_expect_summary <-variable_summary(
  df=modelling_data_set,
  variable="gdl_lifexp",
  description="",
  level="Village Level",
  criteria=list(
    function(x){is.na(x)},
    function(x){x>100}),
  
  criteria_description=c("Null Value", "Value more than 100 unrealistic"),
  actions=c("Exclude", "Exclude"),
  justification=c(
    "Only 1 household",
    "Life expectancy greater than"
  )
)

exclusion_summary <- bind_rows(
  hh_size_summary,
  tlu_summary,
  tva_summary,
  growing_period_summary,
  travel_time_summary,
  hdi_summary,
  life_expect_summary,
)

readr::write_csv(exclusion_summary,"./outputs/02-data-exploration/numeric_variable_exclusion_summary.csv")

above_row_selectors <- which(!is.na(exclusion_summary$Variable))-1
above_row_selectors <- above_row_selectors[above_row_selectors!=0]
# bold_row_selectors <- which(!is.na(cleaned_aggregation$`Cleaned Value`))

cleaned_aggregation <- exclusion_summary %>% flextable::flextable() %>% 
  bold( bold = TRUE, part="header") %>% 
  hline(i = above_row_selectors)  %>% 
  bold(j = c("Variable", "Count"))  


save_as_image(cleaned_aggregation, "./outputs/02-data-exploration/numeric_variable_exclusion_summary.png")


#-------------------------------------------------------------------------
# variable_summary -------------------------------------------------------
#-------------------------------------------------------------------------


variable_summary <- tribble(
  ~Category,~Variable, ~`Data Type`, ~Level, ~Description,~`Reason for Inclusion`,
  "Grouping Variables","id_form","categorical","Household-Level","","",
  "Grouping Variables","gdlcode","categorical","Household-Level","","",
  "Grouping Variables","iso_country_code","categorical","Household-Level","","",
  "Grouping Variables","village","categorical","Household-Level","","",
  
  "Demographics","hh_size_mae","continuous","Household-Level","","",
  "Demographics","education_cleaned","continuous","Household-Level","","",
  "Resource Endowment","livestock_tlu","continuous","Household-Level","","", # centered transform
  "Resource Endowment","land_cultivated_ha","continuous","Household-Level","","",
  
  "Production Orientation","livestock_orientation","proportion","Household-Level","","", #logit transform
  "Production Orientation","crop_orientation","proportion","Household-Level","","", #logit transform
  "Production Orientation","off_farm_orientation","proportion","Household-Level","","", #logit transform
  "Production Orientation","market_orientation","proportion","Household-Level","","", # logit transform
  "Production Orientation","weighted_income_diversity","continuous","Household-Level","","", # centred transform
  
  "Gender","proportion_female_control","proportion","Household-Level","","", # centred transform
  
  
  
  # Village Level Variables
  "Village Variables","adjusted_length_growing_period","continuous","Village Level","","", # centered transform
  "Village Variables","min_travel_time","continuous","Village Level","","", # centered transform
  "Village Variables","kg_class_name","categorical","Village Level","","",
  
  # Country Level Variables
  "County Descriptors","gdl_lifexp","continuous","County","","",
  "County Descriptors","gdl_shdi","proportion","County","","",
  
  "Performance Indicators","tva_per_mae_per_day_ppp","continuous","Household Level","","", # centered transform
  "Performance Indicators","combined_fs_score","ordinal","Household Level","",""
  
)

readr::write_csv(variable_summary,"./outputs/02-data-exploration/variable_summary.csv")

# Transformation ----------------------------------------------------------


log_add_half_min <- function(x){
  replacement <- min(x[x>0 & !is.na(x)])/2
  x[x==0] <- replacement
  
  return(log(x))
}

logit <- function(x){
  min_replacement <- min(x[x>0.000001 & x<0.999999 & !is.na(x)])/2
  max_replacement <- (1-max(x[x>0.000001 & x<0.999999 & !is.na(x)]))/2
  
  x[x<0.001] <- min_replacement
  x[x>0.999] <- 1-max_replacement
  
  return(log(x/(1 - x)))
}

normalisation <- function(x){
  return((x - mean(x)) / sd(x))
}


modelling_data_set$livestock_tlu[ is.na(modelling_data_set$livestock_tlu)] <- 0

modelling_data_set <- modelling_data_set[modelling_data_set$hh_size_mae>0 & !is.na(modelling_data_set$hh_size_mae),]

modelling_data_set <- modelling_data_set[modelling_data_set$tva_per_mae_per_day_ppp>0 & !is.na(modelling_data_set$tva_per_mae_per_day_ppp),]

modelling_data_set <- modelling_data_set[complete.cases(modelling_data_set),]


modelling_data_set <- modelling_data_set[
  modelling_data_set$hh_size_mae<30&
    modelling_data_set$livestock_tlu<30&
    modelling_data_set$tva_per_mae_per_day_ppp<10000&
    modelling_data_set$min_travel_time<2880
  
  ,]





# HHsize Standardisation (Log)

modelling_data_set$log_hh_size <- log_add_half_min(modelling_data_set$hh_size_mae)
modelling_data_set$log_hh_size <- normalisation(modelling_data_set$log_hh_size)

# Livestock TLU (Log)
modelling_data_set$log_livestock_tlu <- log_add_half_min(modelling_data_set$livestock_tlu)
modelling_data_set$log_livestock_tlu <- normalisation(modelling_data_set$log_livestock_tlu)

# Land Cultivated (Log)
modelling_data_set$log_land_cultivated <- log_add_half_min(modelling_data_set$land_cultivated_ha)
modelling_data_set$log_land_cultivated <- normalisation(modelling_data_set$log_land_cultivated)



# Livestock Orientation (Logit)
# modelling_data_set$logit_livestock_orientation <- logit(modelling_data_set$livestock_orientation)
# modelling_data_set$logit_livestock_orientation <- normalisation(modelling_data_set$logit_livestock_orientation)



# # Crop Orientation (Logit)
# modelling_data_set$logit_crop_orientation <- logit(modelling_data_set$crop_orientation)
# modelling_data_set$logit_crop_orientation <- normalisation(modelling_data_set$logit_crop_orientation)

# Off Farm Orientation (Logit)
modelling_data_set$logit_off_farm_orientation <- logit(modelling_data_set$off_farm_orientation)
modelling_data_set$logit_off_farm_orientation <- normalisation(modelling_data_set$logit_off_farm_orientation)

# Market Orientation (Logit)
modelling_data_set$logit_market_orientation <- logit(modelling_data_set$market_orientation)
modelling_data_set$logit_market_orientation <- normalisation(modelling_data_set$logit_market_orientation)

# Income diversity (Log)
modelling_data_set$log_income_diversity <- log_add_half_min(modelling_data_set$weighted_income_diversity)
modelling_data_set$log_income_diversity <- normalisation(modelling_data_set$log_income_diversity)

# Gender Control
modelling_data_set$logit_proportion_female_control <- logit(modelling_data_set$proportion_female_control)
modelling_data_set$logit_proportion_female_control <- normalisation(modelling_data_set$logit_proportion_female_control)


# TVA (Log)
modelling_data_set$log_tva <- log_add_half_min(modelling_data_set$tva_per_mae_per_day_ppp)
modelling_data_set$log_tva <- normalisation(modelling_data_set$log_tva)

# Length Growing Period (Norm)
modelling_data_set$norm_growing_period <- normalisation(modelling_data_set$adjusted_length_growing_period)


# Minimum travel time (Log)
modelling_data_set$log_min_travel_time <- log_add_half_min(modelling_data_set$min_travel_time)
modelling_data_set$log_min_travel_time <- normalisation(modelling_data_set$log_min_travel_time)


# GDL live-exp (Log)
modelling_data_set$norm_gdl_lifexp <- normalisation(modelling_data_set$gdl_lifexp)

# GDL HDI (Logit)
# modelling_data_set$logit_gdl_hdi <- logit(modelling_data_set$gdl_shdi)
# modelling_data_set$logit_gdl_hdi <- normalisation(modelling_data_set$logit_gdl_hdi)




vars <- c(
  "id_form",
  "id_unique",
  "gdlcode",
  "iso_country_code",
  "village",
  
  # "log_hh_size",
  "education_cleaned",
  "log_livestock_tlu", 
  "log_land_cultivated",
  
  "logit_proportion_female_control",
  
  # "logit_livestock_orientation", #logit transform
  # "logit_crop_orientation", #logit transform
  "logit_off_farm_orientation", #logit transform
  "logit_market_orientation", # logit transform
  
  "log_income_diversity", # centred transform
  
  "log_tva", # centered transform
  # "combined_fs_score",
  
  # Village Level Variables
  "norm_growing_period", # centered transform
  "log_min_travel_time", # centered transform
  "kg_class_name",
  
  # Country Level Variables
  "norm_gdl_lifexp"
  # "logit_gdl_hdi"
  
  
)

modelling_data_set <-modelling_data_set[vars]


write_csv(modelling_data_set,"./data/02-prepared-data/modelling_df.csv")


pair_plot <- modelling_data_set %>% 
  select_if( is.numeric) %>% 
  mutate_all(as.numeric) %>% 
  ggpairs(
    upper = list(continuous = "cor", combo = "box_no_facet"),
    lower = list(continuous = "density", combo = "dot_no_facet"),
    diag = list(continuous = "barDiag", combo = "box_no_facet")
  )

ggsave("./outputs/02-data-exploration/var_pairplot.png",plot = pair_plot,width = 4000,height=4000,units = "px")
















