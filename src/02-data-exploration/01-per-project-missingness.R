library(magrittr)
library(readr)
library(tibble)
library(dplyr)


rhomis_data <- readr::read_csv("./data/02-prepared-data/rhomis-spatial-merged.csv")


columns <- list("id_unique"="character",
                "id_hh"="character",
                "id_rhomis_dataset"="character",
                # "id_form"="character",
                "id_proj"="character",
                "gps_lat"="numeric",
                "gps_lon"="numeric",
                "iso_country_code"="character",
                "year"="numeric",
                "currency_conversion_lcu_to_ppp"="numeric",
                "currency_conversion_factor_year"="numeric",
                "survey_length_minutes"="numeric",
                "land_cultivated_ha"="numeric",
                "land_owned_ha"="numeric",
                "livestock_tlu"="numeric",
                "hh_size_members"="numeric",
                "hh_size_mae"="numeric",
                "household_type"="character",
                "head_education_level"="character",
                "worst_food_security_month"="character",
                "best_food_security_month"="character",
                "nr_months_food_shortage"="numeric",
                "fies_score"="numeric",
                "hdds_good_season"="numeric",
                "hdds_good_season_bought"="numeric",
                "hdds_good_season_farm"="numeric",
                "hdds_bad_season"="numeric",
                "hdds_bad_season_bought"="numeric",
                "hdds_bad_season_farm"="numeric",
                "hdds_last_month"="numeric",
                "hdds_last_month_bought"="numeric",
                "hdds_last_month_farm"="numeric",
                "crop_income_lcu_per_year"="numeric",
                "livestock_income_lcu_per_year"="numeric",
                "total_income_lcu_per_year"="numeric",
                "off_farm_income_lcu_per_year"="numeric",
                "value_crop_consumed_lcu_per_hh_per_year"="numeric",
                "value_livestock_products_consumed_lcu_per_hh_per_year"="numeric",
                "value_farm_products_consumed_lcu_per_hh_per_year"="numeric",
                "crop_consumed_calories_kcal_per_hh_per_year"="numeric",
                "livestock_consumed_calories_kcal_per_hh_per_year"="numeric",
                "farm_products_consumed_calories_kcal_per_hh_per_year"="numeric",
                "proportion_of_value_controlled_female_youth"="numeric",
                "proportion_of_value_controlled_female_adult"="numeric",
                "proportion_of_value_controlled_male_youth"="numeric",
                "proportion_of_value_controlled_male_adult"="numeric",
                "hfias_status"="character",
                "hdds_last_24hr"="numeric"
)


result <- sapply(colnames(rhomis_data), function(column){
  
  if(!is.null(columns[[column]]))
  {
    num_char <- columns[[column]]
    
    
    if (num_char=="character"){
      rhomis_data[[column]] <- as.character(rhomis_data[[column]])
      result <- rhomis_data[c("id_form", column)] %>% 
        group_by(id_form) %>% 
        summarise(
          prop_nas= sum(is.na(.data[[column]]), na.rm=T)/length(.data[[column]])
        )
      
      colnames(result) <- paste0(column,"_", colnames(result))
      colnames(result)[1]<- "id_form"
      
      return(result)
    }
    
    if (num_char=="numeric"){
      rhomis_data[[column]] <- as.numeric(rhomis_data[[column]])
      
      result <- rhomis_data[c("id_form", column)] %>% 
        group_by(id_form) %>% 
        summarise(q5= quantile(.data[[column]],probs=0.05, na.rm=T),
                  median=median(.data[[column]], na.rm=T),
                  mean=mean(.data[[column]], na.rm=T),
                  q95= quantile(.data[[column]],probs=0.95, na.rm=T),
                  prop_nas= sum(is.na(.data[[column]]), na.rm=T)/length(.data[[column]]),
                  prop_zero= sum(.data[[column]]==0, na.rm=T)/length(.data[[column]]),
                  prop_na_or_zero= sum(is.na(.data[[column]]) | .data[[column]]==0, na.rm=T)/length(.data[[column]])
        )
      
      colnames(result) <- paste0(column,"_", colnames(result))
      colnames(result)[1]<- "id_form"
      
      return(result)
    }
  }
  
  
})


result <- result %>% bind_cols()
colnames(result) <- gsub("id_form...1","id_form",colnames(result))
result <- result[!grepl("id_form...",colnames(result))]
result <- result[!grepl("id_form[[:digit:]]",colnames(result))]


number_of_hhs <- indicator_sheet %>% 
  group_by(id_form) %>% summarise(number_of_surveys = n())

final_result <- merge(number_of_hhs, result, by="id_form")
final_result <- final_result[order(final_result$number_of_surveys,decreasing = T),]



dir.create("data_quality_check")
readr::write_csv(final_result, "merged/data_quality_check/med_na_24_02_2023.csv")
