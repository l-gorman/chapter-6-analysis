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
                      levels_variables,
                      base_path
){
  
  dir.create(base_path)
  
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
  
  ggsave(filename = paste0(base_path,"/levels_estimates.png"),
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
  
  
  ggsave(filename = paste0(base_path,"/fixed_effects_plots.png"),
         plot = fixed_plots,width = 2500,height=3000,units = "px")
  
  
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
    
    ggsave(filename = paste0(base_path,"/mixed_effects_plots.png"),
           plot = mixed_plots,width = 2500,height=3000,units = "px")
    
    
    # Plotting random effects
    
    dir.create(paste0(base_path,"/random_effects"))
    
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
      
      ggsave(filename = paste0(base_path,"/random_effects/",variable_to_plot,".png"),
             plot = random_plots,width = 2500,height=3000,units = "px")
      
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


dir.create(paste0("outputs/overall_model_results/variable_addition_final_fit/"))

# Model Comparison


variables <- list(
  # "Education (Pre Primary)"="education_cleanedpre_primary",
  "Household Size"="hh_size",
  
  "Education (Primary)"="educationprimary",
  "Education (Secondary/Higher)"="educationsecondary_or_higher",
  
  "Livestock TLU"="livestock_tlu",
  "Land Cultivated"="land_cultivated",
  "Any Off Farm Income"= "off_farm_any",
  "Assisted Tillage"="assisted_tillage",
  "External Labour"="external_labour",
  # "Use Pesticide"="pesticide",
  "Have Debts"="debts_have",
  # "Received Aid"="aidreceived",
  "Use Livestock Inputs"="livestock_inputs_any",
  "Irrigate Land"="land_irrigated_any",
  "Use Fertiliser"="use_fert",
  
  
  "Market Orientation"="market_orientation",
  "Home Garden"="kitchen_garden",
  "Number of Income Sources"="number_income_sources",
  
  "Growing Period"="length_growing_period",
  "Minimum Travel Time"="min_travel_time",
  
  "Country HDI"="gdl_country_shdi"
  
)

levels_variables <- list(
  "Country"="sd_iso_country_code__Intercept",
  "Village"="sd_iso_country_code:village__Intercept",
  "Project"="sd_id_form__Intercept",
  "Unexplained"="sigma"
)


model_files <- list.files("outputs/31_05_2023/outputs/overall_models/variable_addition_final_fit/ /") 
model_files <- c(model_files, list.files("outputs/31_05_2023/outputs/overall_models/variable_addition/tva/"))
model_files <- unique(model_files) 

model_files <- model_files[grepl("^r2",x=model_files)==F & grepl("^loo",x=model_files)==F]


# model <- loadRData("outputs/31_05_2023/outputs/overall_models/variable_addition/tva/weak_prior_fixed.rda")
dir.create("outputs/overall_model_results/variable_addition_final_fit/hdds/")
dir.create("outputs/overall_model_results/variable_addition_final_fit/tva/")
# for (model_file in model_files){
#   
#   
#   
#   model_name <- gsub(".rda","",model_file,fixed=T)
#   
#   tva_path <- paste0("outputs/31_05_2023/outputs/overall_models/variable_addition/tva/",model_file)
#   if (file.exists(tva_path)){
#     tva_model <- loadRData(tva_path)
#     
#     
#     all_plots(model=tva_model,
#               model_name = model_name,
#               variables = variables,
#               levels_variables = levels_variables,
#               base_path=paste0("outputs/overall_model_results/variable_addition/tva/",model_name)
#     )
#     
#   }
#   
#   
#   hdds_path <- paste0("outputs/31_05_2023/outputs/overall_models/variable_addition/hdds/",model_file)
#   if (file.exists(hdds_path)){
#     hdds_model <- loadRData(hdds_path)
#     
#     
#     all_plots(model=hdds_model,
#               model_name = model_name,
#               variables = variables,
#               levels_variables = levels_variables,
#               base_path=paste0("outputs/overall_model_results/variable_addition/hdds/",model_name)
#     )
#     
#   }
# }
# 
# 


loo_comparison_plot <- function(base_input_path,
                                base_output_path,
                                return_data=F){
  
  loo_files <- list.files(base_input_path) %>% grep("^loo",x=., value=T)
  
  
  loo_all <- sapply(loo_files, function(x){
    loo_temp <- loadRData(paste0(
      base_input_path,
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
  
  readr::write_csv(loo_compare,paste0(base_output_path,"loo_comparison.csv"))
  
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
  
  ggsave(paste0(base_output_path,"loo_summary.png"),loo_comparison_plot, width=1500,height=1500,units="px")
  
  
  loo_compare_flextable <- loo_compare %>% flextable::flextable()
  
  save_as_image(loo_compare_flextable, paste0(base_output_path,"loo_comparison.png"))
  
  
  loo_order <- loo_compare$model
  
  if (return_data==T){
    return(loo_compare)
  }
  return(loo_order)
}



# Loo Comparison

hdds_loo_table<- loo_comparison_plot(base_input_path = "./outputs/31_05_2023/outputs/overall_models/variable_addition_final_fit/hdds/",
                                     base_output_path = "./outputs/overall_model_results/variable_addition_final_fit/hdds/",
                                     return_data = T
)

tva_loo_table <- loo_comparison_plot(base_input_path = "./outputs/31_05_2023/outputs/overall_models/variable_addition_final_fit/tva/",
                                     base_output_path = "./outputs/overall_model_results/variable_addition_final_fit/tva/",
                                     return_data = T
                                     
)











# R2 Comparison



r2_comparison <- function(loo_order,
                          base_input_path,
                          base_output_path,
                          return_data=T){
  
  
  r2_files <- list.files(base_input_path) %>% grep("^r2",x=., value=T)
  
  r2_all <- sapply(r2_files, function(x){
    r2_temp <- loadRData(paste0(
      base_input_path,
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
                              levels=loo_order,
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
  
  ggsave(paste0(base_output_path,"r2_summary.png"),r_2_comparison, width=1500,height=1500,units="px")
  return(r2_all)
  
}



hdds_r2_table <- r2_comparison(loo_order = hdds_loo_table$model,
                               base_input_path = "./outputs/31_05_2023/outputs/overall_models/variable_addition_final_fit/hdds/",
                               base_output_path = "./outputs/overall_model_results/variable_addition_final_fit/hdds/")

tva_r2_table <- r2_comparison(tva_loo_table$model,
                              base_input_path = "./outputs/31_05_2023/outputs/overall_models/variable_addition_final_fit/tva/",
                              base_output_path = "./outputs/overall_model_results/variable_addition_final_fit/tva/")




dual_axis_plot <- function(loo_table,
                           r2_table,
                           title,
                           base_output_path,
                           candidate_models
){
  
  plot_df <- bind_cols(loo_table,r2_table)
  plot_df <- plot_df[order(plot_df$number_of_variables),]
  
  
  if (all(plot_df$model==plot_df$model_type)==F){
    stop("Mismatched rows")
  }
  
  
  max_r2_axis <- max(plot_df$Estimate)
  min_r2_axis <- min(plot_df$Estimate)
  
  max_elpd_axis <- abs(min(plot_df$elpd_diff))
  
  # rescaled_value <- max_elpd_axis*(x/(max-min)) - min/(max-min)-max_elpd_axis
  
  # temp <- (plot_df$Estimate*max_elpd_axis)/max_r2_axis-max_elpd_axis

  
  # plot_df$ticks_colour <- ifelse(grepl("group",plot_df$model),"darkgreen","black")
  
  plot <- ggplot(plot_df)+
    geom_point(aes(x=number_of_variables, y=elpd_diff,color="red"))+
    geom_point(aes(x=model_type, y= max_elpd_axis*(Estimate-min_r2_axis)/(max_r2_axis-min_r2_axis)-max_elpd_axis,color="blue"))+
  scale_color_manual(values = c("red" = "red", "blue" = "blue"))
    # geom_point(aes(x=model_type, y= max_elpd_axis*Estimate/(max_r2_axis-min_r2_axis)-max_elpd_axis,color="blue"))
    
  
  plot <- ggplot(plot_df)+
    geom_point(aes(x=number_of_variables, y=elpd_diff,color="red"))+
    
    geom_point(aes(x=number_of_variables, y= max_elpd_axis*(Estimate-min_r2_axis)/(max_r2_axis-min_r2_axis)-max_elpd_axis,color="blue"))+
    geom_path(aes(x=number_of_variables, y=max_elpd_axis*(Estimate-min_r2_axis)/(max_r2_axis-min_r2_axis)-max_elpd_axis, color="blue"),group=1) +
    
    geom_path(aes(x=number_of_variables, y=elpd_diff,color="red"),group=1) +
    
    geom_segment(aes(x = number_of_variables,xend=number_of_variables,y=max_elpd_axis*(Q2.5-min_r2_axis)/(max_r2_axis-min_r2_axis)-max_elpd_axis,yend=max_elpd_axis*(Q97.5-min_r2_axis)/(max_r2_axis-min_r2_axis)-max_elpd_axis),color="blue")+
    geom_segment(aes(x = number_of_variables,xend=number_of_variables,y=lower,yend=upper),color="red")+
    
    geom_hline(yintercept = max(plot_df$Estimate),linetype="dashed")+
    scale_colour_manual(name = 'Measure', 
                        values =c('blue'='blue','red'='red'), labels = c(bquote(~R^2),'ELPD'),guide='legend')+
    scale_size_manual(values =c('Candidate Model'=3,'Other Model'=1.5),guide = 'none')+
    
    
    scale_y_continuous(
      
      # Features of the first axis
      name = "ELPD",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~ ((.+max_elpd_axis)*(max_r2_axis-min_r2_axis)/max_elpd_axis)+min_r2_axis, name=bquote(~'Bayesian '~R^2))
      
      # sec.axis = sec_axis(~ ((.+max_elpd_axis)*max_r2_axis)/max_elpd_axis, name=bquote(~'Bayesian '~R^2 ~'for Intercept Only Models'))
    ) +
    labs(title = title,
         x="Number of Predictor Variables")+
    theme(
      plot.title = element_text(hjust=0.5),
      # axis.text.x = element_text(angle=45,
      #                            hjust=1,
      #                            face =  plot_df$embolden_ticks,
      #                            colour=plot_df$ticks_colour),
      panel.background  = element_blank(),
      panel.border = element_rect(fill=NA, colour = "black"))
  plot
  
  
  
  ggsave(paste0(base_output_path,"r2_loo_comparison.png"),plot, width=3000,height=2000,units="px")
  return(plot)
  
}


tva_loo_table$number_of_variables <- as.numeric(gsub("tva_","",tva_loo_table$model))
# tva_r2_table$number_of_variables <- as.numeric(gsub("tva_","",tva_r2_table$model_type))

dual_axis_plot(loo_table=tva_loo_table,
               r2_table=tva_r2_table,
               title=bquote(~'ELPD and Bayesian '~R^2 ~'\nvs Number of Explanatory Variables (TVA)'),
               base_output_path = "./outputs/overall_model_results/variable_addition_final_fit/tva/"
)


hdds_loo_table$number_of_variables <- as.numeric(gsub("hdds_","",hdds_loo_table$model))

dual_axis_plot(loo_table=hdds_loo_table,
               r2_table=hdds_r2_table,
               title=bquote(~'ELPD and Bayesian '~R^2 ~'\nvs Number of Explanatory Variables (HDDS)'),
               base_output_path = "./outputs/overall_model_results/variable_addition_final_fit/hdds/"
)



# Fixed Effects Plots -----------------------------------------------------

fixed_effect_plot <- function(model,
                              model_name,
                              variables,
                              base_path){
  
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
  
  
  ggsave(filename = paste0(base_path,"/fixed_effects_plots.png"),
         plot = fixed_plots,width = 2500,height=3000,units = "px")
  
}

#TVA

# Variable Number: 6


model_tva <- loadRData("./outputs/31_05_2023/outputs/overall_models/variable_addition_final_fit/tva/tva_6.rda")

variables <- list(
  "Number of Income Sources"="number_income_sources",
  "Household Size"= "hh_size",
  "Market Orientation"="market_orientation",
  "Use of Fertiliser"="use_fert",
  "Land Cultivated"="land_cultivated",
  "Land Irrigation"="land_irrigated_any"
)


fixed_effect_plot(model=model_tva,
                  model_name="TVA",
                  variables=variables,
                  base_path="./outputs/overall_model_results/variable_addition_final_fit/tva/")


