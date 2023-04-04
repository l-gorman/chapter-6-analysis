#' Preparing and Mergining Datasets
#' 
#' This script merges household survey (LSMS)
#' with datasets from Google Earth Engine (GEE)
#' and the GAEZ v4 Data Portal 
#' 
#' The data from GEE was generated using scripts 
#' from the following repository:
#' 
#' https://github.com/l-gorman/earth-engine-farm-size-analysis
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
#' Instructions on Parralel: https://www.blasbenito.com/post/02_parallelizing_loops_with_r/
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



convert_aez_classes <- function(aez_df,
                                aez_colname, 
                                aez_conversion_tbl){
  
  # aez_df <- rasValue
  # aez_colname <- "AEZ_Classes_57"
  # aez_conversion_tbl <- aez_57_class_conversions
  
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



convert_aez_column_name <- function(aez_df,
                                    aez_colname_pattern, 
                                    aez_conversion_tbl){
  
  # aez_df <- fao_level_2
  # aez_colname_pattern <- "level_2_aez_33_classes_"
  # aez_conversion_tbl <- aez_33_class_conversions
  
  aez_conversion_tbl$name <- aez_conversion_tbl$name %>% 
    tolower() %>% 
    gsub("/", " or ", .) %>% 
    gsub(",", "", .) %>% 
    gsub(";", "", .) %>% 
    gsub(" ", "_", .) %>% 
    gsub("-", "_", .) 
  
  
  columns_to_convert <- grep(aez_colname_pattern, colnames(aez_df@data), value=T)
  column_indices <- grep(aez_colname_pattern, colnames(aez_df@data))
  new_columns <- c()
  for (column in columns_to_convert){
    original_column <- column
    original_column_suffix <- gsub(aez_colname_pattern, "", original_column)
    if (!is.na(as.numeric(original_column_suffix))){
      new_column_suffix <- aez_conversion_tbl$name[aez_conversion_tbl$band==as.numeric(original_column_suffix)]
      new_column <- paste0(aez_colname_pattern,new_column_suffix)
      new_columns <- c(new_columns,new_column)
    }
    else{
      new_columns <- c(new_columns,column)
    }
    
  }
  
  colnames(aez_df@data)[column_indices] <- new_columns
  
  return(aez_df)
}




# Read in Survey Data -----------------------------------------------------

rhomis_data <- readr::read_csv("data/raw-data/rhomis/processed_data.csv", na=c("-999","NA", "n/a"))
indicator_data <- readr::read_csv("data/raw-data/rhomis/indicator_data.csv", na=c("-999","NA", "n/a"))
indicator_data$beneficiary <- rhomis_data$beneficiary

indicator_data <- indicator_data %>% merge(rhomis_data[c("id_unique","x_gps_latitude", "x_gps_longitude", "village")],by="id_unique")

# indicator_data <- indicator_data %>% merge(rhomis_data[c("id_unique","x_gps_latitude", "x_gps_longitude")],by="id_unique")

indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- st_as_sf(indicator_data, coords = c("x_gps_longitude", "x_gps_latitude"), 
                           crs = 4326, agr = "constant", remove = F)

indicator_data$index <- c(1:nrow(indicator_data))

# rhomis_data <- NULL

# global data lab shapefile ---------------------------------------------------


gdl_shp <- sf::read_sf("./data/raw-data/global-data-lab/GDL Shapefiles V6.1/")

gdl_code <- readxl::read_excel("./data/raw-data/global-data-lab/GDL Codes V6.1.xlsx")
gdl_info <- readr::read_csv("./data/raw-data/global-data-lab/SHDI-SGDI-Total 7.0.csv")
country_codes <- readr::read_csv("./data/raw-data/country_conversions.csv")

gdl_info_country <- gdl_info[gdl_info$level=="National",]
gdl_info_country["GDLCODE"] <- NULL
colnames(gdl_info_country) <- paste0("gdl_country_",colnames(gdl_info_country))


gdl_info_country <- gdl_info_country %>% merge(country_codes, 
                                                              by.x="gdl_country_iso_code", 
                                                              by.y="alpha-3")


three_letter_codes <- country_codes[country_codes[["alpha-2"]]%in%indicator_data$iso_country_code,][["alpha-3"]]

gdl_shp <- gdl_shp[gdl_shp$iso_code %in%three_letter_codes,]
gdl_code <- gdl_code[gdl_code$ISO_Code %in%three_letter_codes,]


joined_df_rhomis <- st_join(x=indicator_data,
                            y=gdl_shp,
                            left=T)


subset_cols <- !grepl("gdl", colnames(gdl_info),ignore.case = T)
colnames(gdl_info)[subset_cols] <- paste0("gdl_",colnames(gdl_info)[subset_cols])
joined_df_rhomis <- joined_df_rhomis %>% merge(gdl_info, by.x=c("gdlcode","year"), by.y=c("GDLCODE","gdl_year"),all.x=T,all.y=F)

joined_df_rhomis <- joined_df_rhomis %>% merge(gdl_info_country, by.x=c("iso_country_code","year"), by.y=c("alpha-2","gdl_country_year"),all.x=T,all.y=F)


# Joining Raster Information ----------------------------------------------

# Agro-Eco Zone Data (GAEZ)
aez_33_classes <- raster::raster(x = "data/raw-data/gaez/33_classes.tif")
rasterToPoints(aez_33_classes)

xml_33_list <-  xmlParse('data/raw-data/aez/LR/aez/aez_v9v2red_5m_ENSEMBLE_rcp2p6_2020s.tif.aux.xml')
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


adjusted_length_growing_period  <- raster("data/raw-data/aez/gaez_v4_57_class/adjusted_length_growing_period.tif")
adjusted_length_growing_period <- projectRaster(adjusted_length_growing_period,aez_33_classes)

travel_time_5k_to_10k <- raster("data/raw-data/travel-time/travel_time_to_cities_9.tif")
travel_time_5k_to_10k <- projectRaster(travel_time_5k_to_10k,aez_33_classes)

travel_time_10k_to_20k <- raster("data/raw-data/travel-time/travel_time_to_cities_8.tif")
travel_time_10k_to_20k <- projectRaster(travel_time_10k_to_20k,aez_33_classes)

travel_time_20k_to_50k <- raster("data/raw-data/travel-time/travel_time_to_cities_7.tif")
travel_time_20k_to_50k <- projectRaster(travel_time_20k_to_50k,aez_33_classes)

travel_time_50k_to_100k <- raster("data/raw-data/travel-time/travel_time_to_cities_6.tif")
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

rasValue_rhomis=raster::extract(r_stack, indicator_data[c("x_gps_longitude","x_gps_latitude")]) %>% tibble::as_tibble()
colnames(rasValue_rhomis) <- gsub("X33_classes", "AEZ_Classes_33", colnames(rasValue_rhomis))
rasValue_rhomis$AEZ_Classes_33 <- as.integer(rasValue_rhomis$AEZ_Classes_33)
rasValue_rhomis <- convert_aez_classes(rasValue_rhomis,
                                       "AEZ_Classes_33",
                                       aez_33_class_conversions)


joined_df_rhomis %>% group_by(gdlcode,village) %>% 
  summarise(number_per_village=n()) %>% ungroup() %>% 
  group_by(gdlcode) %>% 
  summarise(number_of_villages=n())



joined_df_rhomis <- cbind(joined_df_rhomis,rasValue_rhomis)




# Extracting Projects of Interest -----------------------------------------

projects_to_include <- c(
  "BF_CW2_2015",
  "ML_CW1_2015",
  "TZ_CFM_2015",
  "BF_SIL_2016",
  "ET_SRL_2016",
  "KE_SRL_2016",
  "TZ_SRL_2016",
  "KE_CM1_2016",
  "KE_CM2_2016",
  "CD_CLP_2017",
  "KE_VCD_2017",
  "ET_TA1_2017",
  "ML_TA3_2017",
  # "UG_CWU_2017",
  
  "CD_FRT_2017",
  "KE_SCN_2017",
  "ZM_SCN_2017",
  # "CR_IND_2017",
  
  "BF_GLD_2018",
  "ML_GLD_2018",
  "NE_GLD_2018",
  "BF_TA4_2018",
  "GH_TA7_2018",
  
  "CI_ARC_2018",
  "GH_ARC_2018",
  # "KH_SIL_2018",
  # "EC_CIP_2018",
  # "SL_IFD_2018",
  # "PS_FAO_2018",
  # "SN_FTF_2018",
  "BI_CLP_2018",
  "ZM_GIZ_2018",
  # "PE_MKP_2018",
  "TZ_CRA_2018",
  # "BO_AID_2018",
  "KE_GLT_2019",
  "NG_GLT_2019",
  "ET_GTC_2019",
  "KE_GTC_2019",
  # "IN_GEF_2018",
  "BF_ADN_2019",
  "BI_SNV_2019",
  "KE_LGS_2019",
  "MA_GDI_2019",
  "CD_LGS_2019",
  "ET_LGS_2020"
  
)

projects_with_control_groups <- c(
  "GH_TA2_2017",
  # "IN_BIO_2018",
  "RW_OAF_2018",
  "ET_ARI_2018",
  # "PS_FAO_2018",
  "ET_TA9_2019", # VTE group membership
  "GH_T10_2019", # VTE group membership
  "NE_TA8_2019",
  "GH_ADN_2019",
  "NE_T11_2019",# VTE group membership
  "MW_FAW_2019",
  "ZM_FAW_2019",
  # "KM_DHA_2019",
  "ET_CAF_2020"
  # "VN_CSI_2020",
  # "KH_CSI_2020"
)

# Remove KM, Peru, SL, SN, UG

# Removing Datasets in bias
# crit_1 <- (rhomis_data$id_form %in% tolower(projects_to_include) | rhomis_data$id_form %in% tolower(projects_with_control_groups) )
# table(crit_1)
# crit_2 <- !is.na(rhomis_data$x_gps_latitude)&
#   !is.na(rhomis_data$x_gps_longitude) &
#   !is.na(rhomis_data$village)
# table(crit_1&crit_2)
# 
# crit_3 <-  (is.na(rhomis_data$beneficiary) | rhomis_data$beneficiary %in% c("control","n","core","no_participation","non_beneficiary"))
# table(crit_1&crit_2&crit_3)
# 
#  crit_4 <- !is.na(rhomis_data$landcultivated) 
#  table(crit_1&crit_2&crit_3&crit_4)
 
 crit_1 <- (joined_df_rhomis$id_form %in% tolower(projects_to_include) | joined_df_rhomis$id_form %in% tolower(projects_with_control_groups) )
 table(crit_1)
 crit_2 <- !is.na(joined_df_rhomis$x_gps_latitude)&
   !is.na(joined_df_rhomis$x_gps_longitude) &
   !is.na(joined_df_rhomis$village)
 table(crit_1&crit_2)
 
 crit_3 <-  (is.na(joined_df_rhomis$beneficiary) | joined_df_rhomis$beneficiary %in% c("control","n","core","no_participation","non_beneficiary"))
 table(crit_1&crit_2&crit_3)
 
 crit_4 <- !is.na(joined_df_rhomis$landcultivated) 
 table(crit_1&crit_2&crit_3&crit_4)



subset <- crit_1&crit_2&crit_3

joined_df_rhomis <- joined_df_rhomis[subset,]



# write_to_file -----------------------------------------------------------


readr::write_csv(joined_df_rhomis,"./prepared-data/rhomis-gaez-gdl.csv")

readr::write_csv(joined_df_rhomis,"./prepared-data/gdl.csv")

gdl_shp <- sf::read_sf("./data/raw-data/global-data-lab/GDL Shapefiles V6.1/")
st_write(gdl_shp, "prepared-data/gdl_data.shp")



