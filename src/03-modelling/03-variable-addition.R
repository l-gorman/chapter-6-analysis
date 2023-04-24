# sbatch src/03-modelling/02-overall-model.sh 
library(brms)
# library(ggplot2)
# library(ggridges)
library(dplyr)
library(tidyr)
# library(ggdist)
library(magrittr)
library(optparse)
library(fastDummies)
# library(projpred)
library(cmdstanr)


option_list = list(
  make_option(c("-i", "--iter"),  type='integer',
              help="Iterations"),
  make_option(c("-w", "--warmup"),  type='integer',
              help="Warmup"),
  make_option(c("-d", "--data"), type='character',
              help="Directory where data will be loaded from"),
  make_option(c("-o", "--output"), type='character',
              help="The directory where results will be written"),
  make_option(c("-c", "--ncores"), type='character',
              help="The number of chains/cores"),
  make_option(c("-j", "--index"), type='character',
              help="Index for project to model")
  
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
# 
# 
# opt <- list(
#   iter=20,
#   warmup=10,
#   data="./data/",
#   output="./outputs/test_overall_model_proj",
#   ncores=4,
#   index='1'
# )


options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading
options(loo.cores = opt$ncores)

opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)


writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))
indicator_data <- readr::read_csv(paste0(opt$data,"/02-prepared-data/modelling_df.csv"))








dir.create(paste0(opt$output,"/overall_models/"))
dir.create(paste0(opt$output,"/overall_models/variable_addition"))

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Weak Prior Model --------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Fixed effects 
if(as.numeric(opt$index)==1){
  
  model_name <- "weak_prior_fixed"
  model <- brm(
    formula=log_tva ~ 1 +  
      #Household Level
      education_cleaned + 
      log_livestock_tlu + 
      log_land_cultivated + 
      logit_off_farm_orientation +
      logit_market_orientation +
      logit_proportion_female_control+
      log_income_diversity +
      
      # Village Level
      norm_growing_period +
      log_min_travel_time +
      #County Level
      norm_gdl_lifexp +
      # Levels
      (1 | iso_country_code) +
      (1 | iso_country_code:village)+
      (1 | id_form),
    data = indicator_data,
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = 4,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
  save(model,file=paste0(opt$output,"/overall_models/variable_addition/",model_name,".rda"))
  
  loo_model <- loo(model)
  save(loo_model,file=paste0(opt$output,"/overall_models/variable_addition/loo_",model_name,".rda"))
  loo_model <- NULL
  
  r2_model <- bayes_R2(model)
  save(r2_model,file=paste0(opt$output,"/overall_models/variable_addition/r2_",model_name,".rda"))
  
}

# Mixed effects Country
if(as.numeric(opt$index)==2){
  model_name <- "weak_prior_mixed_country"
  model <- brm(
    formula=log_tva ~ 1 +  
      #Household Level
      education_cleaned + 
      log_livestock_tlu + 
      log_land_cultivated + 
      logit_off_farm_orientation +
      logit_market_orientation +
      logit_proportion_female_control+
      log_income_diversity +
      
      # Village Level
      norm_growing_period +
      log_min_travel_time +
      #County Level
      norm_gdl_lifexp +
      # Levels
      (1 +  
         #Household Level
         education_cleaned + 
         log_livestock_tlu + 
         log_land_cultivated + 
         logit_off_farm_orientation +
         logit_market_orientation +
         logit_proportion_female_control+
         log_income_diversity +
         
         # Village Level
         norm_growing_period +
         log_min_travel_time +
         #County Level
         norm_gdl_lifexp | iso_country_code) +
      (1 | iso_country_code:village)+
      (1 | id_form),
    data = indicator_data,
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = 4,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
  save(model,file=paste0(opt$output,"/overall_models/variable_addition/",model_name,".rda"))
  
  loo_model <- loo(model)
  save(loo_model,file=paste0(opt$output,"/overall_models/variable_addition/loo_",model_name,".rda"))
  loo_model <- NULL
  
  r2_model <- bayes_R2(model)
  save(r2_model,file=paste0(opt$output,"/overall_models/variable_addition/r2_",model_name,".rda"))
  
}


# Mixed effects Village
if(as.numeric(opt$index)==3){
  model_name <- "weak_prior_mixed_village"
  model <- brm(
    formula=log_tva ~ 1 +  
      #Household Level
      education_cleaned + 
      log_livestock_tlu + 
      log_land_cultivated + 
      logit_off_farm_orientation +
      logit_market_orientation +
      logit_proportion_female_control+
      log_income_diversity +
      
      # Village Level
      norm_growing_period +
      log_min_travel_time +
      #County Level
      norm_gdl_lifexp +
      # Levels
      (1 | iso_country_code) +
      (1 +  
         #Household Level
         education_cleaned + 
         log_livestock_tlu + 
         log_land_cultivated + 
         logit_off_farm_orientation +
         logit_market_orientation +
         logit_proportion_female_control+
         log_income_diversity | iso_country_code:village)+
      (1 | id_form),
    data = indicator_data,
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = 4,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
  save(model,file=paste0(opt$output,"/overall_models/variable_addition/",model_name,".rda"))
  
  loo_model <- loo(model)
  save(loo_model,file=paste0(opt$output,"/overall_models/variable_addition/loo_",model_name,".rda"))
  loo_model <- NULL
  
  r2_model <- bayes_R2(model)
  save(r2_model,file=paste0(opt$output,"/overall_models/variable_addition/r2_",model_name,".rda"))
  
}


# Mixed effects Country
if(as.numeric(opt$index)==4){
  model_name <- "weak_prior_mixed_form"
  model  <- brm(
    formula=log_tva ~ 1 +  
      #Household Level
      education_cleaned + 
      log_livestock_tlu + 
      log_land_cultivated + 
      logit_off_farm_orientation +
      logit_market_orientation +
      logit_proportion_female_control+
      log_income_diversity +
      
      # Village Level
      norm_growing_period +
      log_min_travel_time +
      #County Level
      norm_gdl_lifexp +
      # Levels
      (1 | iso_country_code) +
      (1 | iso_country_code:village)+
      (1  +  
         #Household Level
         education_cleaned + 
         log_livestock_tlu + 
         log_land_cultivated + 
         logit_off_farm_orientation +
         logit_market_orientation +
         logit_proportion_female_control+
         log_income_diversity | id_form),
    data = indicator_data,
    prior = c(
      set_prior("normal(0, 1)", class = "b"),
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = 4,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
  save(model,file=paste0(opt$output,"/overall_models/variable_addition/",model_name,".rda"))
  
  loo_model <- loo(model)
  save(loo_model,file=paste0(opt$output,"/overall_models/variable_addition/loo_",model_name,".rda"))
  loo_model <- NULL
  
  r2_model <- bayes_R2(model)
  save(r2_model,file=paste0(opt$output,"/overall_models/variable_addition/r2_",model_name,".rda"))
  
}



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Horse Shoe Model --------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Fixed effects 
if(as.numeric(opt$index)==5){
  model_name <- "horseshoe_fixed"
  model <- brm(
    formula=log_tva ~ 1 +  
      #Household Level
      education_cleaned + 
      log_livestock_tlu + 
      log_land_cultivated + 
      logit_off_farm_orientation +
      logit_market_orientation +
      logit_proportion_female_control+
      log_income_diversity +
      
      # Village Level
      norm_growing_period +
      log_min_travel_time +
      #County Level
      norm_gdl_lifexp +
      # Levels
      (1 | iso_country_code) +
      (1 | iso_country_code:village)+
      (1 | id_form),
    data = indicator_data,
    prior = c(
      set_prior("horseshoe(1)", class="b"),
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = 4,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
  save(model,file=paste0(opt$output,"/overall_models/variable_addition/",model_name,".rda"))
  
  loo_model <- loo(model)
  save(loo_model,file=paste0(opt$output,"/overall_models/variable_addition/loo_",model_name,".rda"))
  loo_model <- NULL
  
  r2_model <- bayes_R2(model)
  save(r2_model,file=paste0(opt$output,"/overall_models/variable_addition/r2_",model_name,".rda"))
  
}

# Mixed effects Country
if(as.numeric(opt$index)==6){
  model_name <- "horseshoe_mixed_country"
  model <- brm(
    formula=log_tva ~ 1 +  
      #Household Level
      education_cleaned + 
      log_livestock_tlu + 
      log_land_cultivated + 
      logit_off_farm_orientation +
      logit_market_orientation +
      logit_proportion_female_control+
      log_income_diversity +
      
      # Village Level
      norm_growing_period +
      log_min_travel_time +
      #County Level
      norm_gdl_lifexp +
      # Levels
      (1 +  
         #Household Level
         education_cleaned + 
         log_livestock_tlu + 
         log_land_cultivated + 
         logit_off_farm_orientation +
         logit_market_orientation +
         logit_proportion_female_control+
         log_income_diversity +
         
         # Village Level
         norm_growing_period +
         log_min_travel_time +
         #County Level
         norm_gdl_lifexp | iso_country_code) +
      (1 | iso_country_code:village)+
      (1 | id_form),
    data = indicator_data,
    prior = c(
      set_prior("horseshoe(1)", class="b"),
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = 4,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
  save(model,file=paste0(opt$output,"/overall_models/variable_addition/",model_name,".rda"))
  
  loo_model <- loo(model)
  save(loo_model,file=paste0(opt$output,"/overall_models/variable_addition/loo_",model_name,".rda"))
  loo_model <- NULL
  
  r2_model <- bayes_R2(model)
  save(r2_model,file=paste0(opt$output,"/overall_models/variable_addition/r2_",model_name,".rda"))
  
}


# Mixed effects Village
if(as.numeric(opt$index)==7){
  model_name <- "horseshoe_mixed_village"
  model <- brm(
    formula=log_tva ~ 1 +  
      #Household Level
      education_cleaned + 
      log_livestock_tlu + 
      log_land_cultivated + 
      logit_off_farm_orientation +
      logit_market_orientation +
      logit_proportion_female_control+
      log_income_diversity +
      
      # Village Level
      norm_growing_period +
      log_min_travel_time +
      #County Level
      norm_gdl_lifexp +
      # Levels
      (1 | iso_country_code) +
      (1 +  
         #Household Level
         education_cleaned + 
         log_livestock_tlu + 
         log_land_cultivated + 
         logit_off_farm_orientation +
         logit_market_orientation +
         logit_proportion_female_control+
         log_income_diversity | iso_country_code:village)+
      (1 | id_form),
    data = indicator_data,
    prior = c(
      set_prior("horseshoe(1)", class="b"),
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = 4,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
  save(model,file=paste0(opt$output,"/overall_models/variable_addition/",model_name,".rda"))
  
  loo_model <- loo(model)
  save(loo_model,file=paste0(opt$output,"/overall_models/variable_addition/loo_",model_name,".rda"))
  loo_model <- NULL
  
  r2_model <- bayes_R2(model)
  save(r2_model,file=paste0(opt$output,"/overall_models/variable_addition/r2_",model_name,".rda"))
  
}


# Mixed effects From
if(as.numeric(opt$index)==8){
  
  model_name <- "horseshoe_mixed_form"
  model <- brm(
    formula=log_tva ~ 1 +  
      #Household Level
      education_cleaned + 
      log_livestock_tlu + 
      log_land_cultivated + 
      logit_off_farm_orientation +
      logit_market_orientation +
      logit_proportion_female_control+
      log_income_diversity +
      
      # Village Level
      norm_growing_period +
      log_min_travel_time +
      #County Level
      norm_gdl_lifexp +
      # Levels
      (1 | iso_country_code) +
      (1 | iso_country_code:village)+
      (1  +  
         #Household Level
         education_cleaned + 
         log_livestock_tlu + 
         log_land_cultivated + 
         logit_off_farm_orientation +
         logit_market_orientation +
         logit_proportion_female_control+
         log_income_diversity | id_form),
    data = indicator_data,
    prior = c(
      set_prior("horseshoe(1)", class="b"),
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = 4,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
  save(model,file=paste0(opt$output,"/overall_models/variable_addition/",model_name,".rda"))
  
  loo_model <- loo(model)
  save(loo_model,file=paste0(opt$output,"/overall_models/variable_addition/loo_",model_name,".rda"))
  loo_model <- NULL
  
  r2_model <- bayes_R2(model)
  save(r2_model,file=paste0(opt$output,"/overall_models/variable_addition/r2_",model_name,".rda"))
  
}




