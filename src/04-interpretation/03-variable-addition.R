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


all_plots <- function(model,
                      model_name,
                      variables,
                      levels_variables
){
  
  dir.create(paste0("outputs/overall_model_results/variable_addition/",model_name))
  
  all_vars <- get_variables(model)
  
  draws_df <- as_draws_df(model)
  
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
  
  
  
  # Plotting Levels Estimates
  
  levels_summary <- draw_summary %>% filter(key %in% as.character(levels_variables))
  clean_names <- names(levels_variables)[match(levels_summary$key,as.character(levels_variables))]
  levels_summary$key <- clean_names
  
  levels_summary$key <- factor(levels_summary$key,
                               levels=names(levels_variables),
                               ordered = T)
  
  
  levels_plots <- quick_estimates_plot(levels_summary, title=paste0("Levels of Variation for Model ", model_name), sort=F)
  
  ggsave(filename = paste0("outputs/overall_model_results/variable_addition/",model_name,"/levels_estimates.png"),
         plot = levels_plots,width = 5000,height=3500,units = "px")
  
  
  # Plotting Fixed Effects
  
  fixed_effects <- c(paste0("b_",variables))
  fixed_effects <- setNames(fixed_effects,variables) %>% as.list()
  
  
  
  fixed_effects_summary <- draw_summary %>% filter(key %in% as.character(fixed_effects))
  
  
  clean_names <- names(fixed_effects)[match(fixed_effects_summary$key,as.character(fixed_effects))]
  fixed_effects_summary$key <- clean_names
  
  clean_names <- names(variables)[match(fixed_effects_summary$key,as.character(variables))]
  fixed_effects_summary$key <- clean_names
  
  fixed_effects_summary$key <- factor(fixed_effects_summary$key,
                                      levels=names(variables),
                                      ordered = T)
  
  
  fixed_plots <- quick_estimates_plot(fixed_effects_summary, title=paste0("Fixed Effects for Model ", model_name), sort=T)
  
  
  ggsave(filename = paste0("outputs/overall_model_results/variable_addition/",model_name,"/fixed_effects_plots.png"),
         plot = fixed_plots,width = 5000,height=3500,units = "px")
  
  
  # Mixed  Effects
  
  variables_in <- lapply(variables,function(x){
    grepl(as.character(x),draw_summary$key)
  }) %>% bind_cols()
  
  variables_in <- rowSums(variables_in)
  
  
  # levels_in <-lapply(levels_variables,function(x){
  #   grepl(as.character(x),draw_summary$key)
  # }) %>% bind_cols()
  # 
  # levels_in <- rowSums(levels_in)
  # 
  
  sd_in <- as.numeric(grepl("sd_",draw_summary$key))
  
  subset <- sd_in>0 & variables_in > 0    
  
  if (any(subset)){
    
    
    mixed_effects <- draw_summary$key[subset]
    names <- gsub(".*__","",mixed_effects)
    mixed_effects <- setNames(mixed_effects,names) %>% as.list()
    
    
    
    mixed_effects_summary <- draw_summary %>% filter(key %in% as.character(mixed_effects))
    clean_names <- names(mixed_effects)[match(mixed_effects_summary$key,as.character(mixed_effects))]
    mixed_effects_summary$key <- clean_names
    
    clean_names <- names(variables)[match(mixed_effects_summary$key,as.character(variables))]
    mixed_effects_summary$key <- clean_names
    
    mixed_effects_summary$key <- factor(mixed_effects_summary$key,
                                        levels=names(variables),
                                        ordered = T)
    
    
    mixed_plots <- quick_estimates_plot(mixed_effects_summary, title=paste0("Random Effects for Model ", model_name), sort=T)
    
    ggsave(filename = paste0("outputs/overall_model_results/variable_addition/",model_name,"/mixed_effects_plots.png"),
           plot = mixed_plots,width = 5000,height=3500,units = "px")
    
    
    # Plotting random effects
    
    dir.create(paste0("outputs/overall_model_results/variable_addition/",model_name,"/random_effects"))
    
    variables_in <- lapply(variables,function(x){
      grepl(as.character(x),draw_summary$key)
    }) %>% bind_cols()
    
    variables_in <- rowSums(variables_in)
    
    r_in <- as.numeric(grepl("^r_",draw_summary$key))
    # cor_not_in <- as.numeric(!grepl("corr_",draw_summary$key))
    
    
    subset <- r_in >0 & variables_in > 0    
    
    random_effects <- draw_summary$key[subset]
    names <- gsub(".*,","",random_effects)
    names <- gsub("\\]","",names)
    levels <- gsub(".*\\[","",random_effects)
    levels <- gsub(",.*","",levels)
    
    effect_names <- tibble(
      level=levels,
      variable=names,
      random_effect=random_effects
    )
    
    
    all_variables <- unique(effect_names$variable)
    
    for (variable_to_plot in all_variables){
      
      all_levels <- effect_names$level[effect_names$variable==variable_to_plot]
      all_effects <- effect_names$random_effect[effect_names$variable==variable_to_plot]
      
      random_effects <- setNames(all_effects,all_levels) %>% as.list()
      
      
      
      
      random_effects_summary <- draw_summary %>% filter(key %in% as.character(random_effects))
      clean_names <- names(random_effects)[match(random_effects_summary$key,as.character(random_effects))]
      random_effects_summary$key <- clean_names
      
      
      
      
      
      
      random_plots <- quick_estimates_plot(random_effects_summary, title=paste0("Random Effects for Model: ", model_name,'\nVariable: ',variable_to_plot), sort=T)
      
      ggsave(filename = paste0("outputs/overall_model_results/variable_addition/",model_name,"/random_effects/",variable_to_plot,".png"),
             plot = random_plots,width = 5000,height=4500,units = "px")
      
    }
    
    
  }
  
  
  
  
}


quick_estimates_plot <- function(draw_summary, title,sort=F){
  
  if(sort==T){
    
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


dir.create(paste0("outputs/overall_model_results/variable_addition/"))

# Model Comparison


variables <- list(
  # "Education (Pre Primary)"="education_cleanedpre_primary",
  "Education (Primary)"="education_cleanedprimary",
  "Education (Secondary)"="education_cleanedsecondary_or_higher",
  "Livestock TLU"="log_livestock_tlu",
  "Land Cultivated"="log_land_cultivated",
  "Off Farm Orientation"="logit_off_farm_orientation",
  "Market Orientation"="logit_market_orientation",
  "Female Control"="logit_proportion_female_control",
  "Income Diversity"="log_income_diversity",
  "Growing Period"="norm_growing_period",
  "Minimum Travel Time"="log_min_travel_time",
  "County Life Expectancy"="norm_gdl_lifexp"
  
)

levels_variables <- list(
  "Country"="sd_iso_country_code__Intercept",
  "Village"="sd_iso_country_code:village__Intercept",
  "Project"="sd_id_form__Intercept",
  "Unexplained"="sigma"
)

model_files <- list.files("outputs/14_04_2023/outputs/overall_models/variable_addition/") 
model_files <- model_files[grepl("^r2",x=model_files)==F & grepl("^loo",x=model_files)==F]



for (model_file in model_files){
  
  
  model_name <- gsub(".rda","",model_file,fixed=T)
  model <- loadRData(paste0("outputs/14_04_2023/outputs/overall_models/variable_addition/",model_file))
  
  all_plots(model=model,
            model_name = model_name,
            variables = variables,
            levels_variables = levels_variables
  )
}


# R2 Comparison

r2_files <- list.files("outputs/14_04_2023/outputs/overall_models/variable_addition/") %>% grep("^r2",x=., value=T)


r2_all <- sapply(r2_files, function(x){
  r2_temp <- loadRData(paste0(
    "outputs/14_04_2023/outputs/overall_models/variable_addition/",
    x
    
  ))
  
  model_name <- gsub("r2_", "",x)
  model_name <- gsub(".rda", "",model_name,fixed=T)
  r2_temp <- as_tibble(r2_temp)
  r2_temp$model_type <- model_name
  return(r2_temp)
  
},simplify=F)


r2_all <- r2_all %>% bind_rows()

r2_all <- r2_all[order(r2_all$Estimate),]

r2_all$model_type <- factor(r2_all$model_type, 
                            levels=r2_all$model_type,
                            ordered=T)

r_2_comparison <- ggplot(r2_all)+
  geom_point(aes(x=model_type, y=Estimate))+
  geom_path(aes(x=model_type, y=Estimate,),group=1, color="blue") +
  geom_segment(aes(x = model_type,xend=model_type,y=Q2.5,yend=Q97.5))+
  
  geom_hline(yintercept = max(r2_all$Estimate),linetype="dashed")+
  
  # ylim(c(0.25,1))+
  
  labs(title = bquote(~'Bayesian '~R^2 ~'for Intercept Only Models'),
       x="Levels Included", 
       y=bquote('Bayesian '~R^2))+
  theme(
    plot.title = element_text(hjust=0.5),
    axis.text.x = element_text(angle=45,hjust=1))

ggsave("outputs/overall_model_results/variable_addition/r2_summary.png",r_2_comparison, width=1500,height=1500,units="px")




# Loo Comparison

loo_files <- list.files("outputs/14_04_2023/outputs/overall_models/variable_addition/") %>% grep("^loo",x=., value=T)


loo_all <- sapply(loo_files, function(x){
  loo_temp <- loadRData(paste0(
    "outputs/14_04_2023/outputs/overall_models/variable_addition/",
    x
    
  ))
  
  loo_temp
  
},simplify=F)

loo_compare <- loo_compare(loo_all) %>% as_data_frame()
loo_compare$model <- row.names(loo_compare(loo_all))
loo_compare$model <- gsub(".rda","",loo_compare$model,fixed=T)
loo_compare$model <- gsub("loo_","",loo_compare$model,fixed=T)
loo_compare <- loo_compare[c("model","elpd_diff","se_diff")]

loo_compare$elpd_diff <- round(loo_compare$elpd_diff,1)
loo_compare$se_diff <- round(loo_compare$se_diff,1)

readr::write_csv(loo_compare,"outputs/overall_model_results/variable_addition/loo_comparison.csv")

loo_compare_flextable <- loo_compare %>% flextable::flextable()

save_as_image(loo_compare_flextable, "outputs/overall_model_results/variable_addition/loo_comparison.png")


# 
# dir.create("outputs/overall_model_results/tva_random_effects/")
# 
# 
# 
# 
# params_list <- list(
#   "Project"="sd_id_form__Intercept",
#   "Country"="sd_iso_country_code__Intercept",
#   "County"="sd_iso_country_code:gdlcode__Intercept",
#   "Village"="sd_iso_country_code:gdlcode:village__Intercept",
#   "Unexplained"="sigma"
# )
# #MCMC Pair plots
# draws <- as_draws_array(weak_prior_tva_random)
# mcmc_scatter <- mcmc_pairs(draws,pars = as.character(params_list),off_diag_fun = "hex")
# ggsave(filename = "outputs/overall_model_results/tva_random_effects/mcmc_scatter.png",
#        plot = mcmc_scatter,width = 5000,height=3500,units = "px")
# 
# 
# params_list <- get_variables(weak_prior_tva_random)
# params_list <- setNames(params_list,params_list)
# draws_df <- as_draws_df(weak_prior_tva_random)
# 
# full_summary <- summarise_estimates(draws_df,params_list)
# 
# full_summary <- full_summary[!is.na(full_summary$key),]
# full_summary <- full_summary[full_summary$key %in% c("lp__","lprior","lp__","lprior")==F,]
# 
# 
# # sd
# # Intercept
# # effect
# # random effect
# # correlation effect
# 
# full_summary$type <- NA
# full_summary$country <- NA
# full_summary$county <- NA
# full_summary$project <- NA
# full_summary$variable <- NA
# 
# full_summary <- full_summary[grepl("^r_iso_country_code:gdlcode:village",full_summary$key)==F,]
# full_summary <- full_summary[grepl("^cor",full_summary$key)==F,]
# 
# # Intercept
# full_summary$type[full_summary$key=="b_Intercept"] <- "Intercept"
# 
# # Fixed Effects
# full_summary$type[grepl("^b_",full_summary$key) & full_summary$key!="b_Intercept"] <- "Fixed Effect"
# 
# # Variance Effects
# full_summary$type[full_summary$key %in% c("sd_id_form__Intercept",
#                                           "sd_iso_country_code__Intercept",
#                                           "sd_iso_country_code:gdlcode__Intercept",
#                                           "sd_iso_country_code:gdlcode:village__Intercept",
#                                           "sigma")] <- "Group Effects"
# 
# 
# # Variation in effects 
# subset <- grepl("^sd_id_form",full_summary$key) & full_summary$key!="sd_id_form__Intercept"
# 
# variable <- full_summary$key[subset] %>% 
#   # gsub("r_iso_country_code\\[","",.) %>% 
#   gsub(".*__","",.)
# 
# full_summary$type[subset] <- "Variation in Random Effects"
# full_summary$variable[subset] <- variable
# 
# 
# # Country Effects
# subset <-  grepl("r_iso_country_code\\[",full_summary$key)
# countries <- full_summary$key[subset] %>% 
#   gsub("r_iso_country_code\\[","",.) %>% 
#   gsub(",.*","",.)
# 
# full_summary$type[subset] <- "Country Intercepts"
# full_summary$country[subset] <- countries
# 
# # County Effects
# 
# subset <-  grepl("r_iso_country_code:gdlcode\\[",full_summary$key)
# country_counties <- full_summary$key[subset] %>% 
#   gsub("r_iso_country_code:gdlcode\\[","",.) %>% 
#   gsub(",.*","",.)
# 
# countries <- country_counties %>%    gsub("_.*","",.)
# counties <- country_counties %>%    gsub(".*_","",.)
# 
# full_summary$type[subset] <- "County Intercepts"
# full_summary$country[subset] <- countries
# full_summary$county[subset] <- counties
# 
# # Form Intercepts
# subset <-  grepl("r_id_form\\[",full_summary$key) & grepl("Intercept",full_summary$key)
# form_ids <- full_summary$key[subset]%>% 
#   gsub("r_id_form\\[","",.) %>% 
#   gsub(",.*","",.)
# 
# countries <- form_ids%>% 
#   gsub("_.*","",.) %>% toupper()
# 
# full_summary$type[subset] <- "Project Intercepts"
# full_summary$country[subset] <- countries
# full_summary$project[subset] <- form_ids
# 
# 
# # Random Project Effects per variable
# subset <-  grepl("r_id_form\\[",full_summary$key) & !grepl("Intercept",full_summary$key)
# form_ids <- full_summary$key[subset]%>% 
#   gsub("r_id_form\\[","",.) %>% 
#   gsub(",.*","",.)
# 
# countries <- form_ids%>% 
#   gsub("_.*","",.) %>% toupper()
# 
# variables <- full_summary$key[subset]%>% 
#   gsub("r_id_form\\[","",.) %>% 
#   gsub(".*,","",.) %>% 
#   gsub("\\]","",.)
# 
# 
# full_summary$type[subset] <- "Project Random Effects"
# full_summary$country[subset] <- countries
# full_summary$project[subset] <- form_ids
# full_summary$variable[subset] <- variables
# 
# # r_id_form[cd_frt_2017,norm_gdl_lifexp]
# # random_effects <- full_summary$key[]
# table(is.na(full_summary$type))
# full_summary$key[is.na(full_summary$type)]
# 
# # village <- grep("village", full_summary$key, value=T)
# # village[1:1000]
# # village[2001:3000]
# 
# 
# 
# write_csv(full_summary,file = "./outputs/overall_model_results/tva_random_effects/full_model_summary.csv")
# 
# # Overall Effects
# params_list <- list(
#   "Education (Pre Primary)"="b_education_cleanedpre_primary",
#   "Education (Primary)"="b_education_cleanedprimary",
#   "Education (Secondary)"="b_education_cleanedsecondary",
#   "Livestock TLU"="b_log_livestock_tlu",
#   "Land Cultivated"="b_log_land_cultivated",
#   "Livestock Orienation"="b_logit_livestock_orientation",
#   "Crop Orientation"="b_logit_crop_orientation",
#   "Off Farm Orientation"="b_logit_off_farm_orientation",
#   "Market Orientation"="b_logit_market_orientation",
#   "Income Diversity"="b_log_income_diversity",
#   "Growing Period"="b_norm_growing_period",
#   "Minimum Travel Time"="b_log_min_travel_time",
#   "AEZ (Hydropromorphic Soils)"="b_aez_class_cleanedhydromorphic_soils",
#   "AEZ (Irrigated Soils)"="b_aez_class_cleanedirrigated_soils",
#   "AEZ (Land With Limitations)"="b_aez_class_cleanedland_with_limitations",
#   "AEZ (Semi Arid or Arid)"="b_aez_class_cleanedsemi_arid_or_arid",
#   "AEZ (Sub Humid)"="b_aez_class_cleanedsub_humid",
#   "County Life Expectancy"="b_norm_gdl_lifexp",
#   "County HDI"="b_logit_gdl_hdi"
#   
# )
# 
# 
# draws_df <- as_draws_df(weak_prior_tva_random)[as.character(params_list)]
# estimate_plot <- estimates_plot(draws_df = draws_df,params_list = params_list,
#                                 title="Aggregated Affects for TVA Model"
# )
# ggsave(filename = "outputs/overall_model_results/tva_random_effects/param_estimates.png",
#        plot = estimate_plot,width = 1800,height=1200,units = "px")
# 
# 
# 
# random_plot <- get_random_effects(model = weak_prior_tva_random,
#                                   variable_of_interest = "log_livestock_tlu",
#                                   title = ""
# )
# 
# random_plot <- get_random_effects(model = weak_prior_tva_random,
#                                   variable_of_interest = "land_cultivated",
#                                   title = ""
# )
# 
# 
# random_plot <- get_random_effects(model = weak_prior_tva_random,
#                                   variable_of_interest = "education_cleanedpre_primary",
#                                   title = ""
# )
# estimates_plot(draws_df = draws_df,params_list = params_list,title = "",sort=T)
# 
# # Variance Reduction Comparisons ------------------------------------------
# dir.create("outputs/overall_model_results/model_comparison")
# 
# 
# 
# params_list <- list(
#   "Project"="sd_id_form__Intercept",
#   "Country"="sd_iso_country_code__Intercept",
#   "County"="sd_iso_country_code:gdlcode__Intercept",
#   "Village"="sd_iso_country_code:gdlcode:village__Intercept",
#   "Unexplained"="sigma"
# )
# draws_df <- as_draws_df(location_only_tva)[as.character(params_list)]
# location_only_tva_estimates <- summarise_estimates(draws_df,params_list)
# location_only_tva_estimates$model <- "Location Only"
# 
# draws_df <- as_draws_df(weak_prior_tva)[as.character(params_list)]
# weak_prior_tva_estimates <- summarise_estimates(draws_df,params_list)
# weak_prior_tva_estimates$model <- "Fixed Effects"
# 
# 
# 
# # draws_df <- as_draws_df(horseshoe_tva)[as.character(params_list)]
# # horseshoe_estimates <- summarise_estimates(draws_df,params_list)
# # horseshoe_estimates$model <- "Horseshoe"
# 
# draws_df <- as_draws_df(weak_prior_tva_random)[as.character(params_list)]
# random_tva_weak_estimates <- summarise_estimates(draws_df,params_list)
# random_tva_weak_estimates$model <- "Project Random Effects"
# 
# estimates_to_compare <- rbind(
#   location_only_tva_estimates,
#   weak_prior_tva_estimates,
#   # horseshoe_estimates,
#   random_tva_weak_estimates
# )
# 
# 
# 
# 
# model_comparison_plot <- ggplot(estimates_to_compare)+
#   # Point plot
#   geom_point(aes(x=key,y=Estimate,color=model,size="Estimate"), position= position_dodge(width = 0.5))+
#   
#   # Thinner 0.95 confidence interval lines
#   geom_linerange(data = estimates_to_compare[estimates_to_compare$level=="0.95 Level",],
#                  aes(x=key, ymin=min,ymax=max, color=model,linewidth=level),
#                  position = position_dodge(width = 0.5)
#   )  +
#   # Thinner .66 level lines
#   geom_linerange(data = estimates_to_compare[estimates_to_compare$level=="0.66 Level",],
#                  aes(x=key, ymin=min,ymax=max, color=model,linewidth=level),
#                  position = position_dodge(width = 0.5)
#   )  +
#   
#   # Allocating Names/Values to scales manually
#   scale_discrete_manual("shape", values = c("Estimate"=2))+
#   scale_discrete_manual("linewidth", values = c("0.95 Level"=1, "0.66 Level"=1.5))+
#   scale_color_brewer(palette = "Dark2")+
#   
#   labs(x="Variables", y="Estimate", title="Sources of Variance for Different Models")+
#   
#   # Fixing the legend scales
#   guides(linewidth = guide_legend(title="",
#                                   nrow = 2, 
#                                   byrow = TRUE, 
#                                   override.aes = list(shape = c(1), linetype = c("solid", "solid"))),
#          shape=guide_legend(title=""),
#          size=guide_legend(title="", shape = c(1)),
#          color=guide_legend(title="",
#                             override.aes = list(linetype="blank", shape = 15, size = 10))) +
#   theme(plot.title = element_text(hjust=0.5))
# 
# 
# ggsave(filename = "outputs/overall_model_results/model_comparison/variance_comparison.png",
#        plot = model_comparison_plot,
#        width = 2400,
#        height = 2000,
#        units = "px"
# )
# 
# 
# 
