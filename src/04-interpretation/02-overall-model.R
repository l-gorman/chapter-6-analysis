library(readr)
library(tidyr)
library(tibble)
library(magrittr)
library(dplyr)
library(reshape2)
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

summarise_estimates <- function(draws_df, param_list){
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
  clean_names <- names(param_list)[match(draw_summary$key,as.character(param_list))]
  
  # clean_names <- c("Total",names(params_list))[match(draw_summary$key,c("Total",as.character(params_list)))]
  draw_summary$key <- clean_names
  
  return(draw_summary)
  
}


estimates_plot <- function(draws_df,
                           param_list,
                           title,
                           sort=F
){
  
  draw_summary <-summarise_estimates(draws_df,
                                     param_list)
  
  draw_summary$key <- factor(draw_summary$key,
                             levels=names(param_list),
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

plot_levels_correlations <- function(
    model,
    level_1, 
    level_2,
    facet=T
){
  
  
  
  draws <- as_draws_df(model,variable =c(level_1,level_2) )
  vars <- colnames(draws)
  vars <- vars[vars %in% c(".chain",".iteration",".draw")==F]
  
  level_1_vars <- vars[grep(paste0("^",level_1,"\\["),vars)]
  level_1_levels <- gsub(".*\\[","",level_1_vars)
  
  level_1_levels <- gsub(",.*","",level_1_levels)
  
  level_2_vars <- vars[grep(paste0("^",level_2,"\\["),vars)]
  
  
  all_data_to_plot <-list()
  for (i in 1:length(level_1_vars))
  {
    upper_level <- level_1_levels[i]
    upper_level_whole <- level_1_vars[i]
    
    relevant_level_2_levels <- level_2_vars[grep(paste0(upper_level,"_"),level_2_vars)]
    
    data_to_plot <- draws[c(upper_level_whole,relevant_level_2_levels)]
    
    data_to_plot <- data_to_plot %>% melt(id.vars=upper_level_whole)
    data_to_plot$variable <- NULL
    
    colnames(data_to_plot) <- c("upper_group", "lower_group")
    data_to_plot$upper_level <- upper_level
    
    all_data_to_plot[[upper_level]] <- data_to_plot
  }
  
  all_data_to_plot <- bind_rows(all_data_to_plot)
  
  
  if (facet==F){
    plot_random_cors <- ggplot(all_data_to_plot,aes(x=upper_group,y=lower_group))+
      geom_hex()
    return(plot_random_cors)
  }
  
  plot_random_cors <- ggplot(all_data_to_plot,aes(x=upper_group,y=lower_group))+
    geom_hex()+
    facet_wrap(~upper_level)
  
  return(plot_random_cors)
  
  
}



dir.create("outputs/overall_model_results/")
dir.create("outputs/overall_model_results/location_only_tva/")


all_plots <- function(model,
                      model_name,
                      param_list,
                      base_dir){
  draws <- as_draws_array(model)
  mcmc_scatter <- mcmc_pairs(draws,pars = as.character(param_list),off_diag_fun = "hex")
  
  mcmc_scatter <- mcmc_pairs(draws,pars = as.character(param_list))
  # mcmc_scatter <- mcmc_scatter + stat_density_2d(color = "black", size = .5)
  
  ggsave(filename = paste0(base_dir,model_name,"/mcmc_scatter.png"),
         plot = mcmc_scatter,width = 5000,height=3500,units = "px")
  
  # Variable Estimate
  draws_df <- as_draws_df(model)[as.character(param_list)]
  estimate_plot <- estimates_plot(draws_df = draws_df,param_list = param_list,
                                  title=paste0("Estimates for ",model_name," TVA Model")
  )
  ggsave(filename = paste0(base_dir,model_name,"/location_estimates.png"),
         plot = estimate_plot,width = 1800,height=1200,units = "px")
  
  
  # VPC Estimates
  vpcs <- vpc(model,as.character(param_list))
  vpc_estimates <- estimates_plot(draws_df = vpcs,param_list = param_list,
                                  title=paste0("VPCs for ",model_name," TVA Model")
  )
  ggsave(filename = paste0(base_dir,model_name,"/location_vpcs.png"),
         plot = vpc_estimates,width = 1800,height=1200,units = "px")
  
  
  all_vars <-get_variables(model)
  
  all_vars <-get_variables(model)
  group_effects <- grep("^sd",all_vars, value=T)
  
  dir.create(paste0(base_dir,model_name,"/random_cors/"))
  
  
  if (any(grepl("r_iso_country_code\\[",all_vars)) & any(grepl("r_iso_country_code:id_form\\[",all_vars))){
    
    temp <- plot_levels_correlations(
      model=model ,
      level_1 = "r_iso_country_code",
      level_2="r_iso_country_code:id_form"
      
    )
    ggsave(filename = paste0(base_dir,model_name,"/random_cors/country_form.png"),
           plot = temp,width = 1800,height=1200,units = "px")
  }
  
  if (any(grepl("r_iso_country_code\\[",all_vars)) & any(grepl("r_iso_country_code:village\\[",all_vars))){
    
    temp <- plot_levels_correlations(
      model=model ,
      level_1 = "r_iso_country_code",
      level_2="r_iso_country_code:village"
      
    )
    ggsave(filename = paste0(base_dir,model_name,"/random_cors/country_village.png"),
           plot = temp,width = 1800,height=1200,units = "px")
  }
  
  if (any(grepl("r_iso_country_code\\[",all_vars)) & any(grepl("r_iso_country_code:gdlcode\\[",all_vars))){
    
    temp <- plot_levels_correlations(
      model=model ,
      level_1 = "r_iso_country_code",
      level_2="r_iso_country_code:gdlcode"
      
    )
    ggsave(filename = paste0(base_dir,model_name,"/random_cors/country_county.png"),
           plot = temp,width = 1800,height=1200,units = "px")
  }
  
  if (any(grepl("r_iso_country_code:gdlcode\\[",all_vars)) & any(grepl("r_iso_country_code:gdlcode:village\\[",all_vars))){
    
    temp <- plot_levels_correlations(
      model=model ,
      level_1 = "r_iso_country_code:gdlcode",
      level_2="r_iso_country_code:gdlcode:village",
      facet=F
      
    )
    ggsave(filename = paste0(base_dir,model_name,"/random_cors/county_village.png"),
           plot = temp,width = 4000,height=3000,units = "px")
  }
  
  
  if (any(grepl("r_gdlcode\\[",all_vars)) & any(grepl("r_gdlcode:village\\[",all_vars))){
    
    temp <- plot_levels_correlations(
      model=model ,
      level_1 = "r_gdlcode",
      level_2="r_gdlcode:village",
      facet=F
      
    )
    ggsave(filename = paste0(base_dir,model_name,"/random_cors/county_village.png"),
           plot = temp,width = 4000,height=3000,units = "px")
  }
  
  if (any(grepl("r_gdlcode\\[",all_vars)) & any(grepl("r_iso_country_code:village\\[",all_vars))){
    
    temp <- plot_levels_correlations(
      model=model ,
      level_1 = "r_gdlcode",
      level_2="r_gdlcode:village",
      facet=F
      
    )
    ggsave(filename = paste0(base_dir,model_name,"/random_cors/country_village.png"),
           plot = temp,width = 1800,height=1200,units = "px")
  }
  
  if (any(grepl("r_kg_class\\[",all_vars)) & any(grepl("r_kg_class:village\\[",all_vars))){
    
    temp <- plot_levels_correlations(
      model=model ,
      level_1 = "r_kg_class",
      level_2="r_kg_class:village",
      facet=F
      
    )
    ggsave(filename = paste0(base_dir,model_name,"/random_cors/kg_class_village.png"),
           plot = temp,width = 1800,height=1200,units = "px")
  }
  
  
  
  
  
}


# All Models

params_list <- list(
  "Between Countries (sd)"="sd_iso_country_code__Intercept",
  
  "Between Counties (sd)"="sd_iso_country_code:gdlcode__Intercept",
  "Between Counties (sd)"="sd_gdlcode__Intercept",
  
  "Between Villages (sd)"="sd_iso_country_code:gdlcode:village__Intercept",
  "Between Villages (sd)"="sd_iso_country_code:village__Intercept",
  "Between Villages (sd)"="sd_gdlcode:village__Intercept",
  "Between Villages (sd)"="sd_kg_class:village__Intercept",
  
  
  "Between Projects (sd)"="sd_id_form__Intercept",
  "Between Projects (sd)"="sd_iso_country_code:id_form__Intercept",
  
  "Between KG Class (sd)"="sd_kg_class_name__Intercept",
  "Unexplained"="sigma"
)



model_files <- list.files("outputs/31_05_2023/outputs/location_only/hdds/") 
model_files <- c(model_files, list.files("outputs/31_05_2023/outputs/location_only/tva/"))
model_files <- unique(model_files) 

model_files <- model_files[grepl("^r2",x=model_files)==F & grepl("^loo",x=model_files)==F]

for (model_file in model_files){
  model_name <- gsub(".rda","",model_file,fixed=T)
  dir.create(paste0("outputs/overall_model_results/location_only_tva/",model_name))
  model <- loadRData(paste0("outputs/31_05_2023/outputs/location_only/",model_file))
  
  all_variables <- get_variables(model)
  
  param_list_temp <-params_list[as.character(params_list) %in% all_variables]
  # model <- country_village_form
  # model_name <- "country_village_form"
  # params_list <- params_list
  all_plots(model,model_name,param_list_temp)
  
  
  # Random effects plots
  
  temp <- sapply(as.character(params_list),function(x){
    grep(paste0(x,"\\:"),all_variables, value=T)
    
  })
  random_vars <- temp[lapply(temp, function(x){length(x)>0}) %>% unlist()]
  fixed_vars <- param_list_temp
  if (length(random_vars)>0){
    dir.create(paste0("outputs/overall_model_results/location_only_tva/",model_name,"/random_vars"))
    
    for (i in 1:length(random_vars)){
      
      
      draws_temp <- as_draws_df(model,variable = c(random_vars[[i]],fixed_vars))
      
      draws_temp <- draws_temp[colnames(draws_temp) %in% c(".chain",".iteration",".draw")==F]
      
      params_list_temp <- c(random_vars[[i]],fixed_vars)
      
      names_params_list_temp_ <- gsub(paste0(names(random_vars)[i],":iso_country_code"),"",params_list_temp, fixed=T)
      
      clean_name_random <- names(params_list)[params_list==names(random_vars)[i]]
      names_params_list_temp_[names_params_list_temp_ %in% fixed_vars==F] <- paste0(names_params_list_temp_[names_params_list_temp_ %in% fixed_vars==F])
      names_params_list_temp_[names_params_list_temp_ %in% fixed_vars==T] <- names(fixed_vars)
      params_list_temp <- setNames(params_list_temp, names_params_list_temp_)
      
      
      
      
      draw_summary <-summarise_estimates(draws_temp,
                                         params_list_temp)
      
      draw_summary$key <- factor(draw_summary$key,
                                 levels=names(params_list_temp),
                                 ordered = T)
      draw_summary$level <- factor(draw_summary$level, levels=c("0.66 Level","0.95 Level"),ordered = T)
      
      draw_summary$facet_split <- ifelse(draw_summary$key %in% names(fixed_vars),"Whole Dataset",paste0("Per Country: ",clean_name_random))
      draw_summary$facet_split <- factor(draw_summary$facet_split, 
                                            levels=c("Whole Dataset",
                                                     paste0("Per Country: ",clean_name_random)),
                                            ordered=T)
      
      plot <- ggplot(draw_summary, aes(y = key,x=Estimate,shape="Estimate"))+
        geom_point(show.legend = T,size=3)+
        geom_segment(aes(y=key,yend=key,x=min,xend=max,linewidth=level))+
        scale_discrete_manual("linewidth", values = c("0.95 Level"=0.75, "0.66 Level"=1.5))+
        facet_wrap(~facet_split, ncol = 1, scales = "free_y",strip.position = "right")+
        labs(x="Estimate", y="", title="Estimates for Standard Deviation in Group Effects")+
        guides(linewidth = guide_legend(title="",
                                        nrow = 2,
                                        byrow = TRUE,
                                        override.aes = list(shape = c(NA), linetype = c("solid", "solid"))),
               shape=guide_legend(title="")) +
        theme(plot.title = element_text(hjust=0.5))
        # axis.text.y = element_text(face =  draw_summary$embolden_overall[draw_summary$level=="0.66 Level"]))

       
      

      ggsave(paste0("outputs/overall_model_results/location_only_tva/",model_name,"/random_vars/",names(random_vars)[i], ".png"), 
             plot,
             width=2000, 
             height = 2500, units = "px")
      
    }
    
  }
  
  
  
  

}


# Loo Comparison

loo_files <- list.files("outputs/31_05_2023/outputs/location_only/") %>% grep("^loo",x=., value=T)


loo_all <- sapply(loo_files, function(x){
  loo_temp <- loadRData(paste0(
    "outputs/31_05_2023/outputs/location_only/",
    x
    
  ))
  
  loo_temp
  
},simplify=F)

loo_compare <- loo_compare(loo_all) %>% as_tibble()


loo_compare$model <- row.names(loo_compare(loo_all))
loo_compare$model <- gsub(".rda","",loo_compare$model,fixed=T)
loo_compare$model <- gsub("loo_","",loo_compare$model,fixed=T)
loo_compare <- loo_compare[c("model","elpd_diff","se_diff")]

loo_compare$elpd_diff <- round(loo_compare$elpd_diff,1)
loo_compare$se_diff <- round(loo_compare$se_diff,1)

readr::write_csv(loo_compare,"outputs/overall_model_results/location_only_tva/loo_comparison.csv")

loo_compare <- loo_compare[order(loo_compare$elpd_diff),]

loo_compare$model <- factor(loo_compare$model, 
                            levels=loo_compare$model,
                            ordered=T)

loo_compare$lower <- loo_compare$elpd_diff-loo_compare$se_diff
loo_compare$upper <- loo_compare$elpd_diff+loo_compare$se_diff



loo_comparison_plot <- ggplot(loo_compare)+
  geom_point(aes(x=model, y=elpd_diff))+
  geom_path(aes(x=model, y=elpd_diff,),group=1, color="blue") +
  geom_segment(aes(x = model,xend=model,y=lower,yend=upper))+
  
  geom_hline(yintercept = max(loo_compare$elpd_diff),linetype="dashed")+
  
  # ylim(c(0.25,1))+
  
  labs(title = 'ELPD for Intercept Only Models',
       x="Levels Included",
       y="ELPD")+
  theme(
    plot.title = element_text(hjust=0.5),
    axis.text.x = element_text(angle=45,hjust=1))

ggsave("outputs/overall_model_results/location_only_tva/loo_summary.png",loo_comparison_plot, width=1500,height=1500,units="px")


loo_compare_flextable <- loo_compare %>% flextable::flextable()

save_as_image(loo_compare_flextable, "outputs/overall_model_results/location_only_tva/loo_comparison.png")


 loo_order <- loo_compare$model












# R2 Comparison

r2_files <- list.files("outputs/31_05_2023/outputs/location_only/") %>% grep("^r2",x=., value=T)


r2_all <- sapply(r2_files, function(x){
  r2_temp <- loadRData(paste0(
    "outputs/31_05_2023/outputs/location_only/",
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
                            levels=loo_compare$model,
                            ordered=T)

r2_all <- r2_all[order(r2_all$model_type),]

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

ggsave("outputs/overall_model_results/location_only_tva/r2_summary.png",r_2_comparison, width=1500,height=1500,units="px")





# 
# # Looking at correlations for random effects
# 
# dir.create("outputs/overall_model_results/location_only_tva/effect_correlation")
# 
# 
# 
# var_type <- list(
#   form="r_id_form",
#   country="r_iso_country_code",
#   county="r_iso_country_code:gdlcode",
#   village="iso_country_code:gdlcode:village",
#   climate_class="kg_class_name")
#  
# 
# c 
# 
# 
#   
# random_effects_cors <- as_draws_df(full_model,variable = "^r", regex=T)
# 
# 
# ?spread_draws
# temp <- spread_rvars(full_model)
# random_effects_correlation <- cor(random_effects_cors)
# random_effects_correlation_df <- as.data.frame(as.table(random_effects_correlation))
# 
# random_effects_correlation_df$var_1_type <- NA
# random_effects_correlation_df$var_1_form <- NA
# random_effects_correlation_df$var_1_country <- NA
# random_effects_correlation_df$var_1_county <- NA
# random_effects_correlation_df$var_1_village <- NA
# random_effects_correlation_df$var_1_climate_class <- NA
# 
# random_effects_correlation_df$var_2_type <- NA
# random_effects_correlation_df$var_2_form <- NA
# random_effects_correlation_df$var_2_country <- NA
# random_effects_correlation_df$var_2_county <- NA
# random_effects_correlation_df$var_2_village <- NA
# random_effects_correlation_df$var_2_climate_class <- NA
# 
# for (var in names(var_type)){
#   
#   reg_name <- var_type[[var]]
#   
#   subset_var_1 <- grep(paste0("^",reg_name),random_effects_correlation_df$Var1)
#   random_effects_correlation_df$var_1_type[subset_var_1] <- var
#   
#   if(var=="form"){
#     form <- gsub(".*\\[","",random_effects_correlation_df$Var1[subset_var_1])
#     form <- gsub(",.*","",form)
#     country <- gsub(",.*","",form)
#     
#   }
#   
#   
# 
#   
#   
#   if ()
#   
#   
#   var_1 <- 
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
