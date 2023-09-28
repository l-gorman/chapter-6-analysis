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
library(reshape2)

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
  draw_summary <- draw_summary[!is.na(draw_summary$key),]
  
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




# Evaluation

# Model 1, Many Countries, Individual projects
model_1 <- loadRData("./outputs/11_09_2023/simulation/simulation_1/model.rda")

levels <- list(
  "Between Country"="sd_country__Intercept",
  "Between Project"="sd_country:project__Intercept",
  "Between Village"="sd_country:project:village__Intercept",
  "Unexplained"="sigma"
  )

draws <- as_draws_df(model_1,variable = as.character(levels))

estimate_plot <- estimates_plot(draws_df = draws,params_list = levels, title = "Simulation 1\n Group Effect Estimates")
estimate_plot <- estimate_plot + xlim(c(0,6))

plot_levels_correlations(model = model_1,level_1 = "r_country",level_2 = "r_country:project")
vpcs <- vpc(model = model_1,params = as.character(levels))
vpc_estimates <- estimates_plot(draws_df = vpcs,params_list  = levels,
                                title=paste0("Simulation 1 VPCs")
)

# Model 2, Many Projects, All in 1 country

model_2 <- loadRData("./outputs/11_09_2023/simulation/simulation_2/model.rda")

levels <- list(
  "Between Country"="sd_country__Intercept",
  "Between Project"="sd_country:project__Intercept",
  "Between Village"="sd_country:project:village__Intercept",
  "Unexplained"="sigma"
)

draws <- as_draws_df(model_2,variable = as.character(levels))
draws$

estimate_plot <- estimates_plot(draws_df = draws,params_list = levels, title = "Simulation 2\n Group Effect Estimates")
estimate_plot <- estimate_plot + xlim(c(0,6))
vpcs <- vpc(model = model_2,params = as.character(levels))
vpc_estimates <- estimates_plot(draws_df = vpcs,params_list  = levels,
                                title=paste0("Simulation 2\n 1 Country, 20 Projects per Country")
)

plot_levels_correlations(model = model_2,level_1 = "r_country",level_2 = "r_country:project",facet = T)


plot_levels_correlations(model = model_2,level_1 = "r_country",level_2 = "r_country:project")

mcmc_pairs(model_2,regex_pars = "r_country")




# Model 3 Many Villages

model_3 <- loadRData("./outputs/11_09_2023/simulation/simulation_3/model.rda")

levels <- list(
  "Between Country"="sd_country__Intercept",
  "Between Project"="sd_country:project__Intercept",
  "Between Village"="sd_country:project:village__Intercept",
  "Unexplained"="sigma"
)

draws <- as_draws_df(model_3,variable = as.character(levels))

estimate_plot <- estimates_plot(draws_df = draws,params_list = levels, title = "Simulation 3\n Group Effect Estimates")
estimate_plot <- estimate_plot + xlim(c(0,6))

vpcs <- vpc(model = model_3,params = as.character(levels))
estimates_plot(draws_df = vpcs,params_list = levels, title = "")

colMeans(vpcs)


plot_levels_correlations(model = model_3,level_1 = "r_country",level_2 = "r_country:project",facet = T)
