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
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


vpc <- function(model, params){
  
  draws_df <-  as_draws_df(model)[params]
  
  vpcs <- list()
  for (param in params){
    other_params <- params[params!=param]
    
    vpcs[[param]] <- draws_df[[param]]^2/rowSums(draws_df[params]^2)
  }
  vpcs <- vpcs %>% as_tibble()
  
  return(vpcs)
}

summarise_estimates <- function(draws_df, params_list){
  # draws_df$Total <- rowSums(draws_df)
  draws.66 <- draws_df %>% 
    gather() %>% 
    group_by(key) %>% 
    summarise(
      Estimate=mean(value),
      min=quantile(value,probs=c(0.17)),
      max=quantile(value,probs=c(0.83)),
      level="0.66 Level",
    )
  
  draws.95 <-draws_df %>% 
    gather() %>% 
    group_by(key) %>% 
    summarise(
      Estimate=mean(value),
      min=quantile(value,probs=c(0.025)),
      max=quantile(value,probs=c(0.975)),
      level="0.95 Level",
    )
  
  draw_summary <- rbind(draws.66,draws.95)
  clean_names <- names(params_list)[match(draw_summary$key,as.character(params_list))]
  
  # clean_names <- c("Total",names(params_list))[match(draw_summary$key,c("Total",as.character(params_list)))]
  draw_summary$key <- clean_names
  
  return(draw_summary)
  
}


estimates_plot <- function(draws_df,
                           params_list,
                           title,
                           sort=F
){

  draw_summary <-summarise_estimates(draws_df,
                                     params_list)
  
  draw_summary$key <- factor(draw_summary$key,
                             levels=names(params_list),
                             ordered = T)
  draw_summary$level <- factor(draw_summary$level, levels=c("0.66 Level","0.95 Level"),ordered = T)
  
  if (sort==T){
    factor_order <- draw_summary$key[order(draw_summary$Estimate[draw_summary$level=="0.66 Level"])]
    draw_summary$key <- factor(draw_summary$key, levels=factor_order,ordered = T)
  }
  
  
  plot <- ggplot(draw_summary, aes(y = key,x=Estimate,shape="Estimate"))+
    geom_point(show.legend = T,size=3)+
    geom_segment(aes(y=key,yend=key,x=min,xend=max,linewidth=level))+
    scale_discrete_manual("linewidth", values = c("0.95 Level"=0.75, "0.66 Level"=1.5))+
    labs(x="Estimate", y="", title=title)+
    guides(linewidth = guide_legend(title="",
                                    nrow = 2, 
                                    byrow = TRUE, 
                                    override.aes = list(shape = c(NA), linetype = c("solid", "solid"))),
           shape=guide_legend(title="")) +
    theme(plot.title = element_text(hjust=0.5))
  
  return(plot)
  
}


get_random_effects <- function(model,
                               variable_of_interest,
                               title,
                               sort=T,
                               prefix="r_id_form"){
  
  # model <-weak_prior_tva_random
  # variable_of_interest <- "norm_growing_period"
  # prefix="r_id_form"
  
  all_vars <- get_variables(model)
  
  random_vars <- all_vars[grepl(paste0("^",prefix),all_vars) & grepl(variable_of_interest,all_vars)]
  clean_vars <- gsub(".*\\[","",random_vars)
  clean_vars <- gsub(",.*","",clean_vars)
  params_list <- setNames(as.list(random_vars), clean_vars)
  
  
  draws_df <- as_draws_df(model)[as.character(random_vars)]
  
  
  plot <- estimates_plot(draws_df,
  params_list,
  title,
  sort=sort)
  
  
  return(plot)
}


dir.create("outputs/overall_model_results/")
dir.create("outputs/overall_model_results/location_only_tva/")


all_plots <- function(model,
                      model_name,
                      param_list){
  draws <- as_draws_array(model)
  mcmc_scatter <- mcmc_pairs(draws,pars = as.character(params_list),off_diag_fun = "hex")
  ggsave(filename = paste0("outputs/overall_model_results/",model_name,"/mcmc_scatter.png"),
         plot = mcmc_scatter,width = 5000,height=3500,units = "px")
  
  # Variable Estimate
  draws_df <- as_draws_df(model)[as.character(params_list)]
  estimate_plot <- estimates_plot(draws_df = draws_df,params_list = params_list,
                                  title=paste0("Estimates for ",model_name," TVA Model")
  )
  ggsave(filename = paste0("outputs/overall_model_results/",model_name,"/location_estimates.png"),
         plot = estimate_plot,width = 1800,height=1200,units = "px")
  
  
  # VPC Estimates
  vpcs <- vpc(model,as.character(params_list))
  vpc_estimates <- estimates_plot(draws_df = vpcs,params_list = params_list,
                                  title=paste0("VPCs for ",model_name," TVA Model")
  )
  ggsave(filename = paste0("outputs/overall_model_results/",model_name,"/location_vpcs.png"),
         plot = vpc_estimates,width = 1800,height=1200,units = "px")
  
}

# Country Only -------------------------------------------------------

dir.create("outputs/overall_model_results/location_only_tva/country_only")


params_list <- list(
  "Country"="sd_iso_country_code__Intercept",
  "Unexplained"="sigma"
)

country_only <- loadRData("outputs/14_04_2023/outputs/overall_models/location_only/country_only.rda")

# Country County -------------------------------------------------------
dir.create("outputs/overall_model_results/location_only_tva/country_county")

params_list <- list(
  "Country"="sd_iso_country_code__Intercept",
  "County"="sd_iso_country_code:gdlcode__Intercept",
  "Unexplained"="sigma"
)

country_county <- loadRData("outputs/14_04_2023/outputs/overall_models/location_only/country_county.rda")

# Country County Village -------------------------------------------------------
dir.create("outputs/overall_model_results/location_only_tva/country_county_village")

params_list <- list(
  "Country"="sd_iso_country_code__Intercept",
  "County"="sd_iso_country_code:gdlcode__Intercept",
  "Village"="sd_iso_country_code:gdlcode:village__Intercept",
  "Unexplained"="sigma"
)

county_country_village <- loadRData("outputs/14_04_2023/outputs/overall_models/location_only/county_country_village.rda")

# Country County Village KG Class -------------------------------------------------------
dir.create("outputs/overall_model_results/location_only_tva/country_county_village_kg")

params_list <- list(
  "Country"="sd_iso_country_code__Intercept",
  "County"="sd_iso_country_code:gdlcode__Intercept",
  "Village"="sd_iso_country_code:gdlcode:village__Intercept",
  "KG Class"="kg_class_name",
  "Unexplained"="sigma"
)

county_country_village_kg <- loadRData("outputs/14_04_2023/outputs/overall_models/location_only/county_country_village_kg.rda")


# Country County Village KG Class Form -------------------------------------------------------
dir.create("outputs/overall_model_results/location_only_tva/country_county_village_form")

params_list <- list(
  "Country"="sd_iso_country_code__Intercept",
  "County"="sd_iso_country_code:gdlcode__Intercept",
  "Village"="sd_iso_country_code:gdlcode:village__Intercept",
  "KG Class"="kg_class_name",
  "Project"="sd_id_form__Intercept",
  "Unexplained"="sigma"
)

county_country_village_kg_form <- loadRData("outputs/14_04_2023/outputs/overall_models/location_only/county_country_village_kg_form.rda")









#MCMC Pair plots




