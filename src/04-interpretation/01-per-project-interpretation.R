library(readr)
library(tidyr)
library(tibble)
library(magrittr)
library(dplyr)
library(tidybayes)
library(brms)
library(ggplot2)
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}



dirs <- list.dirs("./outputs/14_04_2023/outputs")


dirs <- dirs[dirs %in% c("./outputs/14_04_2023/outputs/NA","./outputs/14_04_2023/outputs")==F]

model_types <- c(
  "horseshoe_food_sec",
  "horseshoe_tva",
  "weak_prior_food_sec",
  "weak_prior_tva"
)

list_of_summaries <- list()

i <- 0


# cd_frt_2017 has issues

for (dir in dirs){
  
  project <- gsub(".*/", "", dir)
  
  for (model_name in model_types){
    
    
    path <- paste0(dir,"/",model_name,".rda")
    
    if (file.exists(path)){
      i <- i + 1
      
    
    model <- loadRData(path)
    
    all_vars <- get_variables(model)
    
    vars_of_interst <- all_vars[
      grepl("b_",all_vars)|
        grepl("sd_",all_vars)
    ]    
    
    draw_summary <- as_draws_df(model)[vars_of_interst] %>% gather() %>% 
      group_by(key) %>% 
      summarise(
        estimate=mean(value),
        q.05=quantile(value,probs=c(0.05)),
        
        q.16=quantile(value,probs=c(0.16)),
        q.84=quantile(value,probs=c(0.84)),
        q.95=quantile(value,probs=c(0.95))
      )
    
    draw_summary$project <- project
    
    draw_summary$model <- model_name
    
    list_of_summaries[[i]] <-draw_summary
    }
    
    
  }
}

full_draw_summaries <- list_of_summaries %>% bind_rows()



b_logit_gdl_hdi

dir.create("./outputs/per_project_summaries")
dir.create("./outputs/per_project_summaries/variable_effects")


for (variable in unique(full_draw_summaries$key)){
  for (model in unique(full_draw_summaries$model)){
    dir.create(paste0("./outputs/per_project_summaries/variable_effects/",model),showWarnings = F)
    subset <- full_draw_summaries$key==variable&
      full_draw_summaries$model==model
    
    variable_summary <- ggplot(full_draw_summaries[subset,])+
      geom_segment(aes(y=project,yend=project,x=q.05,xend=q.95),size = 1,color="darkslategray4")+  
      geom_segment(aes(y=project,yend=project,x=q.16,xend=q.84), size = 2, color="darkslategray4")+
      geom_point(aes(y=project,x=estimate))+
      labs(title=paste0("Variable Summary: ", variable,"\n(Model Type: ", model),
           x="Draws Distribution",
           y="Form ID")
    
    ggsave(filename = paste0("./outputs/per_project_summaries/variable_effects/",model,"/",variable,".png"),
           width = 1500,
           height=2000,units = "px")
    
    
    
  }
}









draw_summar

params <- 


params <- c(
  "b_"
)


as_draws(model)

plot(model)
