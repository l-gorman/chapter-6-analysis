library(readr)
library(dplyr)
library(tidyr)
library(tibble)

modelling_data_set <- readr::read_csv("./data/02-prepared-data/rhomis-spatial-merged.csv")

modelling_data_set <- modelling_data_set[!is.na(modelling_data_set$x_gps_latitude) & !is.na(modelling_data_set$x_gps_longitude),]
modelling_data_set <- modelling_data_set[!is.na(modelling_data_set$village),]

modelling_data_set$iso_country_code[is.na(modelling_data_set$iso_country_code)] <- "ML"
modelling_data_set <- modelling_data_set[!is.na(modelling_data_set$iso_country_code),]
modelling_data_set$index <- 1:nrow(modelling_data_set)

# rhomis_data <- readr::read_csv("data/raw-data/rhomis/processed_data.csv", na=c("-999","NA", "n/a"))
# modelling_data_set <- readr::read_csv("data/raw-data/rhomis/modelling_data_set.csv", na=c("-999","NA", "n/a"))
# modelling_data_set$beneficiary <- rhomis_data$beneficiary
# 
# modelling_data_set <- modelling_data_set %>% merge(rhomis_data[c("id_unique","x_gps_latitude", "x_gps_longitude", "village")],by="id_unique")
# 
# # modelling_data_set <- modelling_data_set %>% merge(rhomis_data[c("id_unique","x_gps_latitude", "x_gps_longitude")],by="id_unique")
# 
# modelling_data_set <- modelling_data_set[!is.na(modelling_data_set$x_gps_latitude) & !is.na(modelling_data_set$x_gps_longitude),]
# modelling_data_set <- st_as_sf(modelling_data_set, coords = c("x_gps_longitude", "x_gps_latitude"), 
#                            crs = 4326, agr = "constant", remove = F)
# 

# Transformation ----------------------------------------------------------
ihs <- function(x) {
  y <- log(x + sqrt(x^2 + 1))
  return(y)
}

as_sqrt <- function(x){
  y <- asin(sqrt(x))
  return(y)
}

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

#------------------------



# final_modelling_df <- modelling_data_set[c("id_form",
#                                            "id_unique",
#                                            "iso_country_code",
#                                            "kg_class_name",
#                                            "gdlcode",
#                                            "village",
#                                            "external_labour",
#                                            "assisted_tillage",
#                                            "debts_have",
#                                            "livestock_inputs_any",
#                                            "land_irrigated_any",
#                                            "use_fert",
#                                            "kitchen_garden",
#                                            "off_farm_any"
#                                            
# )]





vars <- c(
  "id_form",
  "id_unique",
  "gdlcode",
  "iso_country_code",
  "village",
  
  # Country Level Variables
  "gdl_country_shdi",
  
  # Village Level Variables
  "adjusted_length_growing_period",
  "min_travel_time", 
  "kg_class_name",
  "population_density",
  
  "hh_size_mae",
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
  "hdds_lean_season"
  
  
)

final_modelling_df <-modelling_data_set[vars]



final_modelling_df$education <-modelling_data_set$education_cleaned

# HHsize Standardisation (Log)
final_modelling_df$hh_size <- log_add_half_min(modelling_data_set$hh_size_mae)
final_modelling_df$hh_size <- normalisation(final_modelling_df$hh_size)

# Livestock TLU (Log)
final_modelling_df$livestock_tlu <- log_add_half_min(modelling_data_set$livestock_tlu)
final_modelling_df$livestock_tlu <- normalisation(final_modelling_df$livestock_tlu)

# Land Cultivated (Log)
final_modelling_df$land_cultivated <- log_add_half_min(modelling_data_set$land_cultivated_ha)
final_modelling_df$land_cultivated <- normalisation(final_modelling_df$land_cultivated)


# hist(modelling_data_set$land_cultivated_ha)
# temp <- ihs(modelling_data_set$land_cultivated_ha)
# hist(temp)
# temp <- log_add_half_min(modelling_data_set$land_cultivated_ha)
# hist(temp)

# Livestock Orientation (Logit)
# modelling_data_set$logit_livestock_orientation <- logit(modelling_data_set$livestock_orientation)
# modelling_data_set$logit_livestock_orientation <- normalisation(modelling_data_set$logit_livestock_orientation)



# # Crop Orientation (Logit)
# modelling_data_set$logit_crop_orientation <- logit(modelling_data_set$crop_orientation)
# modelling_data_set$logit_crop_orientation <- normalisation(modelling_data_set$logit_crop_orientation)

# Off Farm Orientation (Logit)
# modelling_data_set$logit_off_farm_orientation <- logit(modelling_data_set$off_farm_orientation)
# modelling_data_set$logit_off_farm_orientation <- normalisation(modelling_data_set$logit_off_farm_orientation)

# Market Orientation (Logit)
final_modelling_df$market_orientation <- logit(modelling_data_set$market_orientation)
final_modelling_df$market_orientation <- normalisation(final_modelling_df$market_orientation)



# Income diversity (Log)
# modelling_data_set$log_income_diversity <- log_add_half_min(modelling_data_set$weighted_income_diversity)
# modelling_data_set$log_income_diversity <- normalisation(modelling_data_set$log_income_diversity)

# Gender Control
# modelling_data_set$logit_proportion_female_control <- logit(modelling_data_set$proportion_female_control)
# modelling_data_set$logit_proportion_female_control <- normalisation(modelling_data_set$logit_proportion_female_control)


# TVA (Log)
final_modelling_df$tva <- log_add_half_min(modelling_data_set$tva_per_mae_per_day_ppp)
# final_modelling_df$tva <- normalisation(final_modelling_df$tva)

income_values_original <-  modelling_data_set["tva_per_mae_per_day_ppp"]
write_csv(income_values_original,"data/02-prepared-data/income_data_only.csv")

# final_modelling_df$hdds <- normalisation(modelling_data_set$hdds_lean_season)
final_modelling_df$hdds <- log_add_half_min(modelling_data_set$hdds_lean_season)


# Length Growing Period (Norm)
final_modelling_df$length_growing_period <- normalisation(modelling_data_set$adjusted_length_growing_period)


# Minimum travel time (Log)
final_modelling_df$min_travel_time <- log_add_half_min(modelling_data_set$min_travel_time)
final_modelling_df$min_travel_time <- normalisation(final_modelling_df$min_travel_time)


# Population density
final_modelling_df$pop_dens <- log_add_half_min(modelling_data_set$population_density)
final_modelling_df$pop_dens <- normalisation(final_modelling_df$pop_dens)


# GDL live-exp (Log)

final_modelling_df$gdl_country_shdi <- log_add_half_min(modelling_data_set$gdl_country_shdi)
final_modelling_df$gdl_country_shdi <- normalisation(final_modelling_df$gdl_country_shdi)


# Number Income Source
final_modelling_df$number_income_sources <- log_add_half_min(modelling_data_set$number_income_sources)
final_modelling_df$number_income_sources <- normalisation(final_modelling_df$number_income_sources)


# GDL HDI (Logit)
# modelling_data_set$logit_gdl_hdi <- logit(modelling_data_set$gdl_shdi)
# modelling_data_set$logit_gdl_hdi <- normalisation(modelling_data_set$logit_gdl_hdi)




# Adding extra location variables -----------------------------------------

final_modelling_df$iso_country_code_gdlcode <- paste(final_modelling_df$iso_country_code, 
                                                     final_modelling_df$gdlcode, sep = "_")

final_modelling_df$iso_country_code_gdlcode_village <- paste(final_modelling_df$iso_country_code, 
                                                             final_modelling_df$gdlcode,
                                                             final_modelling_df$village, sep = "_")

final_modelling_df$iso_country_code_id_form <- paste(final_modelling_df$iso_country_code, 
                                                     final_modelling_df$id_form, sep = "_")


final_modelling_df$iso_country_code_village <- paste(final_modelling_df$iso_country_code, 
                                                     final_modelling_df$village, sep = "_")

final_modelling_df$gdlcode_village <- paste(final_modelling_df$gdlcode, 
                                                     final_modelling_df$village, sep = "_")


final_modelling_df$kg_class_name_village <- paste(final_modelling_df$kg_class_name, 
                                            final_modelling_df$village, sep = "_")





write_csv(final_modelling_df,"./data/02-prepared-data/modelling_df.csv")


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
  
  "Off Farm", "off_farm_any", "binary","Household-Level","","",
  # "Production Orientation","livestock_orientation","proportion","Household-Level","","", #logit transform
  # "Production Orientation","crop_orientation","proportion","Household-Level","","", #logit transform
  # "Production Orientation","off_farm_orientation","proportion","Household-Level","","", #logit transform
  # "Production Orientation","market_orientation","proportion","Household-Level","","", # logit transform
  # "Production Orientation","weighted_income_diversity","continuous","Household-Level","","", # centred transform
  
  # "Gender","proportion_female_control","proportion","Household-Level","","", # centred transform
  
  
  
  # Village Level Variables
  "Village Variables","adjusted_length_growing_period","continuous","Village Level","","", # centered transform
  "Village Variables","min_travel_time","continuous","Village Level","","", # centered transform
  "Village Variables","kg_class_name","categorical","Village Level","","",
  "Village Variables","population_density","numeric","Village Level","","",
  
  # County Level Variables
  "County Descriptors","gdl_shdi","proportion","County","","",
  
  # Country Level Variables
  "Country Descriptors","gdl_country_shdi","proportion","Country","","",
  
  "Performance Indicators","tva_per_mae_per_day_ppp","continuous","Household Level","","", # centered transform
  "Performance Indicators","hdds","ordinal","Household Level","","",
  
)

readr::write_csv(variable_summary,"./outputs/02-data-exploration/variable_summary.csv")
