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
rhomis_data <- readr::read_csv("data/01-raw-data/rhomis-data/rhomis/processed_data.csv", na=c("-999","NA", "n/a"))
indicator_data <- readr::read_csv("data/01-raw-data/rhomis-data/rhomis/indicator_data.csv", na=c("-999","NA", "n/a"))

rhomis_data <- indicator_data %>% merge(rhomis_data,by="id_unique")

indicator_data <- NULL

# Removing any data with missing GPS coordinates
rhomis_data <- rhomis_data[!is.na(rhomis_data$x_gps_latitude) & !is.na(rhomis_data$x_gps_longitude),]

# Converting the dataset to a "spatial dataframe" to allow linkage 
rhomis_data <- st_as_sf(rhomis_data, coords = c("x_gps_longitude", "x_gps_latitude"), 
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


three_letter_codes <- country_codes[country_codes[["alpha-2"]]%in%rhomis_data$iso_country_code,][["alpha-3"]]

gdl_shp <- gdl_shp[gdl_shp$iso_code %in%three_letter_codes,]
gdl_code <- gdl_code[gdl_code$ISO_Code %in%three_letter_codes,]


joined_df_rhomis <- st_join(x=rhomis_data,
                            y=gdl_shp,
                            left=T)

rhomis_data <- NULL
subset_cols <- !grepl("gdl", colnames(gdl_info),ignore.case = T)
colnames(gdl_info)[subset_cols] <- paste0("gdl_",colnames(gdl_info)[subset_cols])
joined_df_rhomis <- joined_df_rhomis %>% merge(gdl_info, by.x=c("gdlcode","year.x"), by.y=c("GDLCODE","gdl_year"),all.x=T,all.y=F)

joined_df_rhomis <- joined_df_rhomis %>% merge(gdl_info_country, by.x=c("iso_country_code.x","year.x"), by.y=c("alpha-2","gdl_country_year"),all.x=T,all.y=F)


#--------------------------------------------------------------------------
# Joining Raset information -----------------------------------------------
#--------------------------------------------------------------------------


# Agro-Eco Zone Data (GAEZ)
aez_33_classes <- raster::raster(x = "data/01-raw-data/external-data/gaez/33_classes.tif")
rasterToPoints(aez_33_classes)

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


adjusted_length_growing_period  <- raster("data/01-raw-data/external-data/aez/gaez_v4_57_class/adjusted_length_growing_period.tif")
adjusted_length_growing_period <- projectRaster(adjusted_length_growing_period,aez_33_classes)


travel_time_5k_to_10k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_9.tif")
travel_time_5k_to_10k <- projectRaster(travel_time_5k_to_10k,aez_33_classes)

travel_time_10k_to_20k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_8.tif")
travel_time_10k_to_20k <- projectRaster(travel_time_10k_to_20k,aez_33_classes)

travel_time_20k_to_50k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_7.tif")
travel_time_20k_to_50k <- projectRaster(travel_time_20k_to_50k,aez_33_classes)

travel_time_50k_to_100k <- raster("data/01-raw-data/external-data/travel-time/travel_time_to_cities_6.tif")
travel_time_50k_to_100k <- projectRaster(travel_time_50k_to_100k,aez_33_classes)


r_stack <- raster::stack(aez_33_classes,
                         adjusted_length_growing_period,
                         travel_time_5k_to_10k,
                         travel_time_10k_to_20k,
                         travel_time_20k_to_50k,
                         travel_time_50k_to_100k)
names(r_stack) <- c("X33_classes",
                    "adjusted_length_growing_period",
                    "travel_time_5k_to_10k",
                    "travel_time_10k_to_20k",
                    "travel_time_20k_to_50k",
                    "travel_time_50k_to_100k")     

rasValue_rhomis=raster::extract(r_stack, joined_df_rhomis[c("x_gps_longitude","x_gps_latitude")]) %>% tibble::as_tibble()
colnames(rasValue_rhomis) <- gsub("X33_classes", "AEZ_Classes_33", colnames(rasValue_rhomis))
rasValue_rhomis$AEZ_Classes_33 <- as.integer(rasValue_rhomis$AEZ_Classes_33)
rasValue_rhomis <- convert_aez_classes(rasValue_rhomis,
                                       "AEZ_Classes_33",
                                       aez_33_class_conversions)


# joined_df_rhomis %>% group_by(gdlcode,village) %>% 
#   summarise(number_per_village=n()) %>% ungroup() %>% 
#   group_by(gdlcode) %>% 
#   summarise(number_of_villages=n())
# 


joined_df_rhomis <- cbind(joined_df_rhomis,rasValue_rhomis)


cols_to_remove <- grep("\\.y",colnames(joined_df_rhomis), value=T)
joined_df_rhomis <- joined_df_rhomis[colnames(joined_df_rhomis) %in% cols_to_remove==F]
colnames(joined_df_rhomis) <- gsub("\\.x","",colnames(joined_df_rhomis))





# write_to_file -----------------------------------------------------------


readr::write_csv(joined_df_rhomis,"./data/02-prepared-data/rhomis-spatial-merged.csv")




