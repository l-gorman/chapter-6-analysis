#' Preparing and Mergining Datasets
#' 
#' This script merges household survey (RHoMIS)
#' with datasets from the Global Data Lab (GDL)
#' 
#' Data from GAEZ can be explored and downloaded here:
#' 
#' https://gaez.fao.org/pages/data-viewer
#' 
#' With more notes on Accessing GAEZ data here:
#' 
#' https://gaez.fao.org/pages/data-access-download
#' 
#' AEZ metadata (for class conversion) found here:
#' 
#' https://gaez-data-portal-hqfao.hub.arcgis.com/pages/data-access-download
#' 
#' For more information, please see the README of this
#' repository
#' 
#' 
#' 
# -------------------------------------------------------------------------------------------------------------
# Loading Libraries -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# Data Cleaning and reformatting
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(jsonlite)
library(XML)
library(fastDummies)
library(magrittr)
library(readxl)
# Spatial Packages
library(sf)
library(stars)
library(terra)
library(raster)
library(sp)
library(geojsonsf)
library(rgdal)



# Defining Functions ------------------------------------------------------

#' Convert AEZ Classes
#' 
#' A function for converting AEZ classes from 
#' numbers to the name of the class. 
#'
#' @param aez_df A dataframe containing a column of AEZ classifications
#' @param aez_colname The column containing AEZ classifications
#' @param aez_conversion_tbl A conversion table which converts from numbers to classes
#'
#' @return
#' @export
#'
#' @examples
convert_aez_classes <- function(aez_df,
                                aez_colname, 
                                aez_conversion_tbl){
  

  
  aez_df$index <- c(1:nrow(aez_df))
  
  aez_conversion_tbl$band <- as.integer(aez_conversion_tbl$band)
  aez_df[[aez_colname]] <- as.integer(aez_df[[aez_colname]])
  
  
  
  result <- aez_df %>% merge(aez_conversion_tbl, 
                             by.x=aez_colname, 
                             by.y="band",
                             all.x=T,
                             all.y=F)
  
  result <- result[order(result$index),]
  
  result$name <- result$name %>% 
    tolower() %>% 
    gsub("/", " or ", .) %>% 
    gsub(",", "", .) %>% 
    gsub(";", "", .) %>% 
    gsub(" ", "_", .) %>% 
    gsub("-", "_", .) 
  
  
  
  result <- fastDummies::dummy_cols(result, "name", remove_selected_columns=T)
  colnames(result) <- colnames(result) %>% 
    gsub("name_",paste0(aez_colname,"_"),.)
  
  aez_df$index <- NULL
  
  return(result)
}


#--------------------------------------------------------------------------
# Read in Survey Data -----------------------------------------------------
#--------------------------------------------------------------------------

# Reading in RHoMIS survey data, and rhomis indicator data
rhomis_data <- readr::read_csv("data/02-prepared-data/filtered_rhomis_data.csv", na=c("-999","NA", "n/a"))
# indicator_data <- readr::read_csv("data/01-raw-data/rhomis-data/rhomis/indicator_data.csv", na=c("-999","NA", "n/a"))

# rhomis_data <- indicator_data %>% merge(rhomis_data,by="id_unique")

# indicator_data <- NULL

# Removing any data with missing GPS coordinates
rhomis_data <- rhomis_data[!is.na(rhomis_data$x_gps_latitude) & !is.na(rhomis_data$x_gps_longitude),]

# Converting the dataset to a "spatial dataframe" to allow linkage 
rhomis_data <- st_as_sf(rhomis_data, coords = c("x_gps_longitude","x_gps_latitude"), 
                           crs = 4326, agr = "constant", remove = F)

# Adding an index to ensure consistent row order before and after merging (see below)
rhomis_data$index <- c(1:nrow(rhomis_data))


#--------------------------------------------------------------------------
# Link Subnational HDI Index ----------------------------------------------
#--------------------------------------------------------------------------

gdl_shp <- sf::read_sf("./data/01-raw-data/external-data/global-data-lab/GDL Shapefiles V6.1/")

gdl_code <- readxl::read_excel("./data/01-raw-data/external-data/global-data-lab/GDL Codes V6.1.xlsx")
gdl_info <- readr::read_csv("./data/01-raw-data/external-data/global-data-lab/SHDI-SGDI-Total 7.0.csv")
country_codes <- readr::read_csv("./data/01-raw-data/external-data/country_conversions.csv")

gdl_info_country <- gdl_info[gdl_info$level=="National",]
gdl_info_country["GDLCODE"] <- NULL
colnames(gdl_info_country) <- paste0("gdl_country_",colnames(gdl_info_country))


gdl_info_country <- gdl_info_country %>% merge(country_codes, 
                                                              by.x="gdl_country_iso_code", 
                                                              by.y="alpha-3")


three_letter_codes <- country_codes[country_codes[["alpha-2"]]%in% rhomis_data$iso_country_code  ,][["alpha-3"]]

gdl_shp <- gdl_shp[gdl_shp$iso_code %in%three_letter_codes,]
gdl_code <- gdl_code[gdl_code$ISO_Code %in%three_letter_codes,]


joined_df_rhomis <- st_join(x=rhomis_data,
                            y=gdl_shp,
                            left=T)

# joined_df_rhomis <- joined_df_rhomis %>% rename(year=year.x)
rhomis_data <- NULL
subset_cols <- !grepl("gdl", colnames(gdl_info),ignore.case = T)
colnames(gdl_info)[subset_cols] <- paste0("gdl_",colnames(gdl_info)[subset_cols])

joined_df_rhomis$year_temp <- joined_df_rhomis$year
joined_df_rhomis$year_temp[joined_df_rhomis$year_temp>2021] <- 2021

joined_df_rhomis <- joined_df_rhomis %>% merge(gdl_info, by.x=c("gdlcode","year_temp"), by.y=c("GDLCODE","gdl_year"),all.x=T,all.y=F)

joined_df_rhomis <- joined_df_rhomis %>% merge(gdl_info_country, by.x=c("iso_country_code","year_temp"), by.y=c("alpha-2","gdl_country_year"),all.x=T,all.y=F)
joined_df_rhomis$year_temp <- NULL


#--------------------------------------------------------------------------
# Joining Raset information -----------------------------------------------
#--------------------------------------------------------------------------

koppen_geiger_classification <- raster::raster(x = "data/01-raw-data/external-data/Beck_KG_V1/Beck_KG_V1_present_0p083.tif")
conversions <- tibble::tribble(
  ~kg_class_number,~kg_class_code,~kg_class_name,
  1,"Af","Tropical, rainforest",
  2,"Am","Tropical, monsoon",
  3,"Aw","Tropical, savannah",
  4,"BWh","Arid, desert, hot",
  5,"BWk","Arid, desert, cold",
  6,"BSh","Arid, steppe, hot",
  7,"BSk","Arid, steppe, cold",
  8,"Csa","Temperate, dry summer, hot summer",
  9,"Csb","Temperate, dry summer, warm summer",
  10,"Csc","Temperate, dry summer, cold summer",
  11,"Cwa","Temperate, dry winter, hot summer",
  12,"Cwb","Temperate, dry winter, warm summer",
  13,"Cwc","Temperate, dry winter, cold summer",
  14,"Cfa","Temperate, no dry season, hot summer",
  15,"Cfb","Temperate, no dry season, warm summer",
  16,"Cfc","Temperate, no dry season, cold summer",
  17,"Dsa","Cold, dry summer, hot summer",
  18,"Dsb","Cold, dry summer, warm summer",
  19,"Dsc","Cold, dry summer, cold summer",
  20,"Dsd","Cold, dry summer, very cold winter",
  21,"Dwa","Cold, dry winter, hot summer",
  22,"Dwb","Cold, dry winter, warm summer",
  23,"Dwc","Cold, dry winter, cold summer",
  24,"Dwd","Cold, dry winter, very cold winter",
  25,"Dfa","Cold, no dry season, hot summer",
  26,"Dfb","Cold, no dry season, warm summer",
  27,"Dfc","Cold, no dry season, cold summer",
  28,"Dfd","Cold, no dry season, very cold winter",
  29,"ET","Polar, tundra",
  30,"EF","Polar, frost"
)


adjusted_length_growing_period  <- raster("data/01-raw-data/external-data/aez/gaez_v4_57_class/adjusted_length_growing_period.tif")
adjusted_length_growing_period <- projectRaster(adjusted_length_growing_period,koppen_geiger_classification)


# travel_time_5k_to_10k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_9.tif")
# travel_time_5k_to_10k <- projectRaster(travel_time_5k_to_10k,koppen_geiger_classification)
# 
# travel_time_10k_to_20k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_8.tif")
# travel_time_10k_to_20k <- projectRaster(travel_time_10k_to_20k,koppen_geiger_classification)

travel_time_20k_to_50k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_7.tif")
travel_time_20k_to_50k <- projectRaster(travel_time_20k_to_50k,koppen_geiger_classification)

travel_time_50k_to_100k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_6.tif")
travel_time_50k_to_100k <- projectRaster(travel_time_50k_to_100k,koppen_geiger_classification)

travel_time_100k_to_200k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_5.tif")
travel_time_100k_to_200k <- projectRaster(travel_time_100k_to_200k,koppen_geiger_classification)

travel_time_200k_to_500k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_4.tif")
travel_time_200k_to_500k <- projectRaster(travel_time_200k_to_500k,koppen_geiger_classification)

travel_time_500k_to_1M <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_3.tif")
travel_time_500k_to_1M <- projectRaster(travel_time_500k_to_1M,koppen_geiger_classification)

travel_time_1M_to_5M <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_2.tif")
travel_time_1M_to_5M <- projectRaster(travel_time_1M_to_5M,koppen_geiger_classification)

travel_time_5M_to_50M <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_1.tif")
travel_time_5M_to_50M <- projectRaster(travel_time_5M_to_50M,koppen_geiger_classification)


gridded_population <- raster("data/01-raw-data/external-data/gpw-v4-population-density-rev11_2015_2pt5_min_tif/gpw_v4_population_density_rev11_2015_2pt5_min.tif")
gridded_population <- projectRaster(gridded_population,koppen_geiger_classification)



r_stack <- raster::stack(koppen_geiger_classification,
                         adjusted_length_growing_period,
                         
                         travel_time_20k_to_50k,
                         travel_time_50k_to_100k,
                         travel_time_100k_to_200k,
                         travel_time_200k_to_500k,
                         travel_time_500k_to_1M,
                         travel_time_1M_to_5M,
                         travel_time_5M_to_50M,
                         gridded_population)


names(r_stack) <- c("koppen_geiger_classification",
                    "adjusted_length_growing_period",
                    
                    "travel_time_20k_to_50k",
                    "travel_time_50k_to_100k",
                    "travel_time_100k_to_200k",
                    "travel_time_200k_to_500k",
                    "travel_time_500k_to_1M",
                    "travel_time_1M_to_5M",
                    "travel_time_5M_to_50M",
                    "population_density")     


rasValue_rhomis=raster::extract(r_stack, joined_df_rhomis[c("x_gps_longitude","x_gps_latitude")]) %>% tibble::as_tibble()

rasValue_rhomis$koppen_geiger_classification <- as.integer(rasValue_rhomis$koppen_geiger_classification)
rasValue_rhomis$index <- c(1:nrow(rasValue_rhomis))
rasValue_rhomis <- rasValue_rhomis %>% merge(conversions,by.x="koppen_geiger_classification",by.y="kg_class_number",all.x=T,all.y=F)
rasValue_rhomis <- rasValue_rhomis[order(rasValue_rhomis$index),]

# colnames(rasValue_rhomis) <- gsub("X33_classes", "AEZ_Classes_33", colnames(rasValue_rhomis))
# rasValue_rhomis$AEZ_Classes_33 <- as.integer(rasValue_rhomis$AEZ_Classes_33)
# rasValue_rhomis <- convert_aez_classes(rasValue_rhomis,
#                                        "AEZ_Classes_33",
#                                        aez_33_class_conversions)


# joined_df_rhomis %>% group_by(gdlcode,village) %>% 
#   summarise(number_per_village=n()) %>% ungroup() %>% 
#   group_by(gdlcode) %>% 
#   summarise(number_of_villages=n())
# 




joined_df_rhomis <- cbind(joined_df_rhomis,rasValue_rhomis)




cols_to_remove <- grep("\\.y",colnames(joined_df_rhomis), value=T)
joined_df_rhomis <- joined_df_rhomis[colnames(joined_df_rhomis) %in% cols_to_remove==F]
colnames(joined_df_rhomis) <- gsub("\\.x","",colnames(joined_df_rhomis))


travel_time_cols <- grep("travel_time", colnames(joined_df_rhomis), value=T)
travel_time_cols <- as_tibble(joined_df_rhomis[travel_time_cols])
travel_time_cols$geometry <- NULL
min_travel_time <-  apply(travel_time_cols, 1, min)
joined_df_rhomis$min_travel_time <- min_travel_time




# write_to_file -----------------------------------------------------------


readr::write_csv(joined_df_rhomis,"./data/02-prepared-data/rhomis-spatial-merged.csv")




