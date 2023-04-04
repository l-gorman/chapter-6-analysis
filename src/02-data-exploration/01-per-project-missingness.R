library(magrittr)
library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(ggExtra)
library(cowplot)
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


number_of_hhs <- rhomis_data %>% 
  group_by(id_form) %>% summarise(number_of_surveys = n())

final_result <- merge(number_of_hhs, result, by="id_form")
final_result <- final_result[order(final_result$number_of_surveys,decreasing = T),]



dir.create("outputs")
dir.create("outputs/01-data-quality-check")

readr::write_csv(final_result, "outputs/01-data-quality-check/indicator_quality_check_all.csv")




prop_na_cols <- grep("prop_nas",colnames(final_result),value=T)
plotting_df <- final_result[c("id_form","number_of_surveys",prop_na_cols)] %>% gather(key = "variable", value = "prop_na",-id_form, -number_of_surveys)
plotting_df$variable  <- gsub("_prop_nas","",plotting_df$variable)



plot <- ggplot(plotting_df, aes(y=variable, x=id_form, fill = prop_na, size = prop_na)) +
  geom_point(shape = 21, stroke = 0) +
  geom_hline(yintercept = seq(.5, length(unique(plotting_df$variable))-0.5, 1), size = .2)+
  geom_vline(xintercept = seq(.5, length(unique(plotting_df$id_form))-0.5, 1), size = .2)+
  
  theme_minimal()+
  scale_radius(range = c(0, 2))+
  scale_x_discrete(position = "top") +
  
  scale_fill_gradient(low = "green", high = "red", breaks = c(0, .5, 1), labels = c("Great", "OK", "Bad"), limits = c(0, 1)) +
  theme(axis.text.x = element_text(hjust=1,angle=270,vjust=-1),
    legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
                             label.position = "bottom",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) 
ggsave("outputs/01-data-quality-check/missing_indicators.png",plot,width = 4000,height=2000,units = "px",limitsize = FALSE)


plotting_df$id_form <- factor(plotting_df$id_form, levels = sort(unique(plotting_df$id_form)), ordered = T)
plotting_df$numeric_id <- as.numeric(plotting_df$id_form)

plot <- ggplot(plotting_df, aes(y=variable, x=id_form, fill = prop_na, size = prop_na)) +
  # Initial Point plot
  geom_point(shape = 21, stroke = 0) +
  # Grid lines
  geom_hline(yintercept = seq(.5, length(unique(plotting_df$variable))-0.5, 1), size = .2)+
  geom_vline(xintercept = seq(.5, length(unique(plotting_df$id_form))-0.5, 1), size = .2)+
  
  # Highlighting projects
  geom_vline(xintercept = 4, size = 1, color="green",alpha=0.5)+
  
  theme_minimal()+
  
  # Setting the range of point sizes
  scale_radius(range = c(0, 1.5))+
  # Putting axis ticks at bottom
  scale_x_discrete(position = "bottom") +
  
  # Setting Scale Colours for 
  scale_fill_gradient(low = "green", high = "red", breaks = c(0, .5, 1), labels = c("Low\nProp", "Med\nProp", "High\nProp"), limits = c(0, 1)) +
  theme(axis.text.x = element_text(hjust=1,angle=90,vjust=0.5),
        legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8)) +
  guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
                             label.position = "bottom",
                             title.position = "right", 
                             order = 1),
         fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2)) 


xhist <- 
  axis_canvas(plot, axis = "x") + 
  geom_bar(data = plotting_df, aes(x=numeric_id,y=number_of_surveys), stat = "identity")


# ggplot(data = plotting_df, aes(x=numberic_id,y = number_of_surveys))+
#   geom_bar(stat = "identity")


plot %>%
  insert_xaxis_grob(xhist, grid::unit(1,"in"), position = "top") %>%
  ggdraw()
  # ggMarginal(data=plotting_df,x="number_of_surveys",y=0)


 



