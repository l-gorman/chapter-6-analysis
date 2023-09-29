# sbatch src/03-modelling/06-sensitivity-analysis.sh

library(brms)
# library(ggplot2)
# library(ggridges)
library(dplyr)
library(tidyr)
# library(ggdist)
library(magrittr)
library(optparse)
# library(projpred)
library(cmdstanr)



set_cmdstan_path("/user/home/lg14410/.cmdstan/cmdstan-2.32.2")

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
#   index='4'
# )


options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading
options(loo.cores = opt$ncores)


opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)
dir.create(paste0(opt$output,"/overall_models/"))
dir.create(paste0(opt$output,"/overall_models/sensitivity_analysis"))
dir.create(paste0(opt$output,"/overall_models/sensitivity_analysis/variance_component"))
dir.create(paste0(opt$output,"/overall_models/sensitivity_analysis/mixed_effects"))

dir.create(paste0(opt$output,"/overall_models/sensitivity_analysis/variance_component/tva"))
dir.create(paste0(opt$output,"/overall_models/sensitivity_analysis/variance_component/hdds"))

dir.create(paste0(opt$output,"/overall_models/sensitivity_analysis/mixed_effects/tva"))
dir.create(paste0(opt$output,"/overall_models/sensitivity_analysis/mixed_effects/hdds"))



writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))
indicator_data <- readr::read_csv(paste0(opt$data,"/02-prepared-data/modelling_df.csv"))

opt$index <- as.numeric(opt$index)


index_1 <- opt$index
index_2 <- opt$index - 50
index_3 <- opt$index - 100 
index_4 <- opt$index - 150


# Configuration

if (opt$index <= 49){
  index <- index_1
  indicator_to_check <- "hdds"
  model_type <- "variance_component"
  
  formula <- bf(hdds ~ 1 +
                  (1 | iso_country_code) +
                  (1 | iso_country_code_village))
  
}

if (opt$index >=50  &
  opt$index <= 99){
  index <- index_2
  
  indicator_to_check <- "tva"
  model_type <- "variance_component"
  
  formula <- bf(tva ~ 1 +
                  (1 | iso_country_code) +
                  (1 | iso_country_code_village))
  
}

if (opt$index >=100  &
    opt$index <= 149){
  index <- index_3
  
  indicator_to_check <- "hdds"
  model_type <- "mixed_effects"
  
  
  formula <- bf(hdds ~ 1 +

               number_income_sources+
               land_cultivated+
               education+
               use_fert+
               external_labour+
               kitchen_garden+
               land_irrigated_any+
               min_travel_time+
               assisted_tillage+
               off_farm_any+
               market_orientation+
               length_growing_period+
               hh_size+
               
               
               
               # Levels
               (1 | iso_country_code) +
               (1 | iso_country_code_village))
  

}

if (opt$index >=150  &
    opt$index <= 199){
  index <- index_4
  
  indicator_to_check <- "tva"
  model_type <- "mixed_effects"
  
  formula <- bf(tva ~ 1 +
               
               number_income_sources +
               hh_size +
               market_orientation +
               land_cultivated + 
               use_fert +
               external_labour +
               education +
               assisted_tillage +
               kitchen_garden +
               land_irrigated_any +
               livestock_inputs_any +
               
               # Levels
               (1 | iso_country_code) +
               (1 | iso_country_code_village))
}




# Subsetting
form_ids <- unique(indicator_data$id_form)

if(index==0){
  print("using whole dataset")
  data_subset <- indicator_data
}

if(index>0){
  
  form_subset <- form_ids[index]
  print(paste0("excluding_project: ",form_subset))
  
  data_subset <- indicator_data[indicator_data$id_form!=form_subset,]
}

# Model Fitting

if (model_type=="variance_component"){
  
  model <- brm(
    formula=formula,
    data=data_subset,
    prior = c(
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = opt$ncores,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    family=gaussian() 
  )
  
}


if (model_type=="mixed_effects"){
  
  model <- brm(
    
    formula=formula,
    data=data_subset,
    prior = c(
      set_prior('normal(0, 1)', class = 'b'),
      set_prior('normal(0, 1)', class = 'sd'),
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = opt$ncores,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
}

base_path <- paste0(opt$output,"/overall_models/sensitivity_analysis/",model_type,"/",indicator_to_check,"/")


save(model,file=paste0(base_path,"model_",index,".rda"))


loo_model <- loo(model)
save(loo_model,file=paste0(base_path,"loo_",index,".rda"))
loo_model <- NULL

r2_model <- bayes_R2(model)
save(r2_model,file=paste0(base_path,"r2_",index,".rda"))

