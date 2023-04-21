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
              help="The number of chains/cores")
  
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
#   ncores=4
# )


options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading

opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)


writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))
indicator_data <- readr::read_csv(paste0(opt$data,"/02-prepared-data/modelling_df.csv"))


dir.create(paste0(opt$output,"/overall_models/"))
dir.create(paste0(opt$output,"/overall_models/location_only"))

country_only <- brm(
  formula=log_tva ~ 1 +  
    (1 | iso_country_code),
  
  # (1 | village),
  data = indicator_data,
  prior = c(
    set_prior('normal(0, 1)', class = 'sd'),
    # set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  ),
  cores = 4,
  backend = "cmdstanr",
  iter = opt$iter,
  warmup = opt$warmup,
  
  
  family=gaussian() 
)

save(country_only,file=paste0(opt$output,"/overall_models/location_only/country_only.rda"))

country_county <- brm(
  formula=log_tva ~ 1 +  
    (1 | iso_country_code) +
    (1 | iso_country_code:gdlcode) ,
  
  # (1 | village),
  data = indicator_data,
  prior = c(
    set_prior('normal(0, 1)', class = 'sd'),
    # set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  ),
  cores = 4,
  backend = "cmdstanr",
  iter = opt$iter,
  warmup = opt$warmup,
  
  
  family=gaussian() 
)

save(country_county,file=paste0(opt$output,"/overall_models/location_only/country_county.rda"))

county_country_village <- brm(
  formula=log_tva ~ 1 +  
    (1 | iso_country_code) +
    (1 | iso_country_code:gdlcode) +
    (1 | iso_country_code:gdlcode:village),
  
  # (1 | village),
  data = indicator_data,
  prior = c(
    set_prior('normal(0, 1)', class = 'sd'),
    # set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  ),
  cores = 4,
  backend = "cmdstanr",
  iter = opt$iter,
  warmup = opt$warmup,
  
  
  family=gaussian() 
)

save(county_country_village,file=paste0(opt$output,"/overall_models/county_country_village.rda"))


county_country_village_kg <- brm(
  formula=log_tva ~ 1 +  
    (1 | iso_country_code) +
    (1 | iso_country_code:gdlcode) +
    (1 | iso_country_code:gdlcode:village)+
    (1 | kg_class_name),
  
  # (1 | village),
  data = indicator_data,
  prior = c(
    set_prior('normal(0, 1)', class = 'sd'),
    # set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  ),
  cores = 4,
  backend = "cmdstanr",
  iter = opt$iter,
  warmup = opt$warmup,
  
  
  family=gaussian() 
)

save(county_country_village_kg,file=paste0(opt$output,"/overall_models/county_country_village_kg.rda"))


county_country_village_kg_form <- brm(
  formula=log_tva ~ 1 +  
    (1 | iso_country_code) +
    (1 | iso_country_code:gdlcode) +
    (1 | iso_country_code:gdlcode:village)+
    (1 | kg_class_name)+
    (1 | id_form),
  
  # (1 | village),
  data = indicator_data,
  prior = c(
    set_prior('normal(0, 1)', class = 'sd'),
    # set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  ),
  cores = 4,
  backend = "cmdstanr",
  iter = opt$iter,
  warmup = opt$warmup,
  
  
  family=gaussian() 
)

save(county_country_village_kg_form,file=paste0(opt$output,"/overall_models/county_country_village_kg_form.rda"))

country_only <- add_criterion(country_only, "loo")
country_county <- add_criterion(country_county, "loo")
county_country_village <- add_criterion(county_country_village, "loo")
county_country_village_kg <- add_criterion(county_country_village_kg, "loo")
county_country_village_kg_form <- add_criterion(county_country_village_kg_form, "loo")

loo_results <- loo_compare(country_only,
  country_county,
  county_country_village,
  county_country_village_kg,
  county_country_village_kg_form, criterion = c("loo"))

save(loo_results,file=paste0(opt$output,"/overall_models/loo_comparison.rda"))


