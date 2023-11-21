library(readr)
library(tidyr)
library(tibble)
library(magrittr)
library(dplyr)
library(tidybayes)
library(brms)
library(ggplot2)
library(bayesplot)
library(hexbin)
library(flextable)


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


get_variables_all <- function(base_path){
  # base_path <- "./outputs/11_09_2023/outputs/overall_models/sensitivity_analysis/variance_component/hdds/"
  model <- loadRData(paste0(base_path,"model_0.rda"))
  all_vars <- get_variables(model)
  return(all_vars)
  
}


effects_summary <- function(draws,
                            variable_names){
  # all_vars <- get_variables(model)[as.character(variable_names)]
  
  # draws_df <- as_draws_df(model)
  
  draws.66 <- draws %>% 
    gather() %>% 
    group_by(key) %>% 
    summarise(
      Estimate=mean(value),
      min=quantile(value,probs=c(0.17)),
      max=quantile(value,probs=c(0.83)),
      level="0.66 Level",
    )
  
  draws.66 <- draws.66[draws.66$key %in% c(".chain",".draw", ".iteration")==F,]
  
  draws.95 <-draws %>% 
    gather() %>% 
    group_by(key) %>% 
    summarise(
      Estimate=mean(value),
      min=quantile(value,probs=c(0.025)),
      max=quantile(value,probs=c(0.975)),
      level="0.95 Level",
    )
  draws.95 <- draws.95[draws.95$key %in% c(".chain",".draw", ".iteration")==F,]
  
  
  draw_summary <- rbind(draws.66,draws.95)
  
  # fixed_effects <- c(paste0("b_",variables))
  # fixed_effects <- setNames(fixed_effects,variables) %>% as.list()
  
  
  
  # fixed_effects_summary <- draw_summary %>% filter(key %in% as.character(fixed_effects))
  
  # variable_names
  clean_names <- names(variable_names)[match(draw_summary$key,as.character(variable_names))]
  draw_summary$key <- clean_names
  
  # clean_names <- names(variables)[match(fixed_effects_summary$key,as.character(variables))]
  # fixed_effects_summary$key <- clean_names
  
  draw_summary$key <- factor(draw_summary$key,
                             levels=names(variable_names),
                             ordered = T)
  
  return(draw_summary)
}


all_fold_summary <- function(base_path,
                          variable_names,
                          number_of_iterations=49,
                          form_names
){
  
  model_summary_list <- list()
  
  for (model_index in 1:(number_of_iterations+1)){
    file_name <- paste0(base_path,"model_",model_index-1,".rda")
    if (file.exists(file_name)){
      model <- loadRData(file_name)
      draws <- as_draws_df(model,variable = as.character(variable_names))
      model_summary <- effects_summary(draws,variable_names)
      
      model_summary$index<- as.character(model_index-1)
      if (model_index-1==0){
        model_summary$form <- "All Projects"
      }
      if (model_index-1>0){
        model_summary$form <-form_names[model_index-1]
      }
      
      model_summary_list[[model_index]] <- model_summary
      
    }else{
      print(paste("Missing file: ", file_name))
      model_summary_list[[model_index]]<-tibble::tibble(
        key=factor(levels = names(variable_names),ordered = T),
        Estimate=numeric(),
        min=numeric(),
        max=numeric(),
        level=character()
      )
    }
    
  }
  
  model_summaries_all <- dplyr::bind_rows(model_summary_list,)
  # model_summaries_all$index[model_summaries_all$index=="0"] <- "All Projects"
  
  model_summaries_all$form <- factor(model_summaries_all$form, levels=c(form_names,"All Projects"))
  return(model_summaries_all)
  
}


quick_estimates_plot <- function(draw_summary, title,  number_of_iterations=49){
  
  # draw_summary$index <- factor(draw_summary$index, levels = c( c(number_of_iterations:1), "All Projects"), ordered = T)  

  plot <- ggplot(draw_summary, aes(y = form,x=Estimate,shape="Estimate"))+
    geom_point(show.legend = T,size=3)+
    geom_segment(aes(y=form,yend=form,x=min,xend=max,linewidth=level))+
    scale_discrete_manual("linewidth", values = c("0.95 Level"=0.75, "0.66 Level"=1.5))+
    labs(x="Estimate", y="Fold", title=title)+
    guides(linewidth = guide_legend(title="",
                                    nrow = 2, 
                                    byrow = TRUE, 
                                    override.aes = list(shape = c(NA), linetype = c("solid", "solid"))),
           shape=guide_legend(title="")) +
    theme(plot.title = element_text(hjust=0.5))
  return(plot)
}


# Getting form information ------------------------------------------------


indicator_data <- readr::read_csv("./data/02-prepared-data/modelling_df.csv")
forms <- unique(indicator_data$id_form)

# HDDS Location Only Model Summaries ----------------------------------------------------

output_path <- "./outputs/overall_model_results/sensitivity_analysis/variance_components/hdds/"
base_input_path <- "./outputs/11_09_2023/outputs/overall_models/sensitivity_analysis/variance_component/hdds/"

dir.create(output_path,recursive = T)


variable_names <- list(
  "Standard Deviation in Country Effects"="sd_iso_country_code__Intercept",
  "Standard Deviation in Village Effects"="sd_iso_country_code_village__Intercept",
  "Unexplained Variation"="sigma"
)

model_summary_all <- all_fold_summary(base_path=base_input_path,
                 variable_names=variable_names,
                 number_of_iterations=49,
                 form_names=forms
)


for (variable in names(variable_names)){
  
  # variable <- "Standard Deviation in Country Effects"
  
  temp_plot <- quick_estimates_plot(model_summary_all[model_summary_all$key==variable,],
                       title = paste0("Sensitivity Analysis for HDDS\nVariance Components Model\n(",variable,")"),number_of_iterations=49)
  
  ggsave(paste0(output_path,variable,".png"),temp_plot,width=2000, height=1700, units="px")
  
}


# TVA Location Only Model Summaries ----------------------------------------------------

output_path <- "./outputs/overall_model_results/sensitivity_analysis/variance_components/tva/"
base_input_path <- "./outputs/11_09_2023/outputs/overall_models/sensitivity_analysis/variance_component/tva/"

dir.create(output_path,recursive = T)


variable_names <- list(
  "Standard Deviation in Country Effects"="sd_iso_country_code__Intercept",
  "Standard Deviation in Village Effects"="sd_iso_country_code_village__Intercept",
  "Unexplained Variation"="sigma"
)

model_summary_all <- all_fold_summary(base_path=base_input_path,
                                      variable_names=variable_names,
                                      number_of_iterations=49,
                                      form_names=forms
                                      
)


for (variable in names(variable_names)){
  
  # variable <- "Standard Deviation in Country Effects"
  
  temp_plot <- quick_estimates_plot(model_summary_all[model_summary_all$key==variable,],
                                    title = paste0("Sensitivity Analysis for TVA\nVariance Components Model\n(",variable,")"),number_of_iterations=49)
  
  ggsave(paste0(output_path,variable,".png"),temp_plot,width=2000, height=1700, units="px")
  
}



# Mixed Effects HDDS ------------------------------------------------------



output_path <- "./outputs/overall_model_results/sensitivity_analysis/mixed_effects/hdds/"
base_input_path <- "./outputs/11_09_2023/outputs/overall_models/sensitivity_analysis/mixed_effects/hdds/"
all_vars <- get_variables_all(base_input_path)
all_vars[1:30]
dir.create(output_path,recursive = T)


variable_names <- list(
  "Standard Deviation in Country Effects"="sd_iso_country_code__Intercept",
  "Standard Deviation in Village Effects"="sd_iso_country_code_village__Intercept",
  "Unexplained Variation"="sigma",
  
  "Number of Income Sources"="b_number_income_sources",
  "Land Cultivated"="b_land_cultivated",
  "Primary Education"="b_educationprimary",
  "Secondary Education or Higher"="b_educationsecondary_or_higher",
  "Use Fertiliser"="b_use_fert",
  "Use External Labour"="b_external_labour",
  "Kitchen Garden"="b_kitchen_garden",
  "Irrigate Land"="b_land_irrigated_any",
  "Minimum Travel Time"="b_min_travel_time",
  "Assisted Tillage"="b_assisted_tillage",
  "Off Farm Income"="b_off_farm_any",
  "Market Orientation"="b_market_orientation",
  "Length of Growing Period"="b_length_growing_period",
  "Livestock TLU"="b_livestock_tlu"
  
  # "Household Size"="b_hh_size"
)

model_summary_all <- all_fold_summary(base_path=base_input_path,
                                      variable_names=variable_names,
                                      number_of_iterations=49,
                                      form_names=forms
)


for (variable in names(variable_names)){
  
  # variable <- "Standard Deviation in Country Effects"
  
  temp_plot <- quick_estimates_plot(model_summary_all[model_summary_all$key==variable,],
                                    title = paste0("Sensitivity Analysis for HDDS\nMixed Effects Model\n(",variable,")"),number_of_iterations=49)
  
  ggsave(paste0(output_path,variable,".png"),temp_plot,width=2000, height=1700, units="px")
  
}

# Mixed Effects TVA -------------------------------------------------------


output_path <- "./outputs/overall_model_results/sensitivity_analysis/mixed_effects/tva/"
base_input_path <- "./outputs/11_09_2023/outputs/overall_models/sensitivity_analysis/mixed_effects/tva/"
all_vars <- get_variables_all(base_input_path)
all_vars[1:30]
dir.create(output_path,recursive = T)


variable_names <- list(
  "Standard Deviation in Country Effects"="sd_iso_country_code__Intercept",
  "Standard Deviation in Village Effects"="sd_iso_country_code_village__Intercept",
  "Unexplained Variation"="sigma",
  
  "Number of Income Sources"="b_number_income_sources",
  "Household Size"="b_hh_size",
  "Market Orientation"="b_market_orientation",
  "Land Cultivated"="b_land_cultivated",
  "Use Fertiliser"="b_use_fert",
  "Use External Labour"="b_external_labour",
  "Primary Education"="b_educationprimary",
  "Secondary Education or Higher"="b_educationsecondary_or_higher",
  "Assisted Tillage"="b_assisted_tillage",
  "Kitchen Garden"="b_kitchen_garden",
  "Irrigate Land"="b_land_irrigated_any",
  "Livestock Inputs Any"="b_livestock_inputs_any"
  
)

model_summary_all <- all_fold_summary(base_path=base_input_path,
                                      variable_names=variable_names,
                                      number_of_iterations=49,
                                      form_names=forms
)


for (variable in names(variable_names)){
  
  # variable <- "Standard Deviation in Country Effects"
  
  temp_plot <- quick_estimates_plot(model_summary_all[model_summary_all$key==variable,],
                                    title = paste0("Sensitivity Analysis for TVA\nMixed Effects Model\n(",variable,")"),number_of_iterations=49)
  
  ggsave(paste0(output_path,variable,".png"),temp_plot,width=2000, height=1700, units="px")
  
}


