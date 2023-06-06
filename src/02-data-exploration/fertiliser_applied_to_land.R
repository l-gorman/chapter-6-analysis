
other_1_index <- grepl("other1", indicator_data$fertiliser_crops)
indicator_data$fertiliser_crops[other_1_index] <- paste0(indicator_data$fertiliser_crops[other_1_index]," ", indicator_data$crops_other1[other_1_index])

other_2_index <- grepl("other2", indicator_data$fertiliser_crops)
indicator_data$fertiliser_crops[other_2_index] <- paste0(indicator_data$fertiliser_crops[other_2_index]," ", indicator_data$crops_other2[other_2_index])

other_3_index <- grepl("other3", indicator_data$fertiliser_crops)
indicator_data$fertiliser_crops[other_3_index] <- paste0(indicator_data$fertiliser_crops[other_3_index]," ", indicator_data$crops_other3[other_3_index])





fertiliser_crops <- rhomis::split_string_categories_to_dummy(indicator_data$fertiliser_crops, " ")
crop_name_cleaner_temp <- crop_name_cleaner
crop_name_cleaner_temp$id_rhomis_dataset <- NULL
crop_name_cleaner_temp <- crop_name_cleaner_temp[duplicated(crop_name_cleaner_temp)==F,]

fertiliser_crops <- switch_column_names_and_merge_categories(fertiliser_crops,
                                                             conversion_tibble = crop_name_cleaner_temp,
                                                             by_project = F)
fertiliser_crops$other1 <- NULL
fertiliser_crops$other <- NULL
fertiliser_crops$other2 <- NULL
fertiliser_crops$other3 <- NULL
fertiliser_crops$na <- NULL 
number_crops <- rowSums(fertiliser_crops,na.rm = T)


# Crop land ---------------------------------------------------------------


number_of_crop_loops <- find_number_of_loops(indicator_data, "crop_land_area")

# Identifying all of the looped columns we will need for our calculation
crop_land_area_columns <- paste0("crop_land_area_", c(1:number_of_crop_loops))
crop_land_prop_area_columns <- paste0("crop_land_area_proportions_numeric_", c(1:number_of_crop_loops))
crop_land_area_ha_columns <- paste0("crop_land_area_ha_", c(1:number_of_crop_loops))

crop_land_data <- indicator_data[crop_land_area_columns]

crop_land_proportions_numeric <- sapply(crop_land_area_columns, function(col_name){
  temp_df <-crop_land_data[col_name]
  temp_df$index <- c(1:nrow(temp_df))
  
  temp_df <- temp_df %>% merge(rhomis::proportion_conversions,by.x = c(col_name),by.y=c("survey_value"), all.x = T, all.y = F)
  temp_df <- temp_df[order(temp_df$index),]
  temp_df[["conversion"]]
}, simplify = F) %>% dplyr::bind_cols()


colnames(crop_land_proportions_numeric) <- crop_land_prop_area_columns

indicator_data <- add_column_after_specific_column(data = indicator_data,
                                                   new_data = crop_land_proportions_numeric,
                                                   new_column_name = "crop_land_area_proportions_numeric",
                                                   old_column_name = "crop_land_area",
                                                   loop_structure = T
)


crop_land_proportions_numeric <- map_to_wide_format(data = full_data,
                                                    name_column = "crop_name",
                                                    column_prefixes = "crop_land_area_proportions_numeric",types = "num")[[1]]


crops_to_examine <- colnames(fertiliser_crops)[colnames(fertiliser_crops) %in% colnames(crop_land_proportions_numeric)]
fertiliser_crops <- fertiliser_crops[crops_to_examine]
crop_land_proportions_numeric <- crop_land_proportions_numeric[crops_to_examine]

prop_land_fertilised <- fertiliser_crops*crop_land_proportions_numeric
prop_land_fertilised <- rowSums(prop_land_fertilised,na.rm=T)
prop_land_fertilised[prop_land_fertilised>1] <- 1 
