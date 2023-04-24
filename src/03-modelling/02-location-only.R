# sbatch src/03-modelling/02-location-only.sh 
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
              help="Array Index")
  
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

options(loo.cores = opt$ncores)




# 
# 
# opt <- list(
#   iter=20,
#   warmup=10,
#   data="./data/",
#   output="./outputs/test_overall_model_proj",
#   ncores=4,
#   index=1
# )


options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading

opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)


writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))
indicator_data <- readr::read_csv(paste0(opt$data,"/02-prepared-data/modelling_df.csv"))


dir.create(paste0(opt$output,"/overall_models/"))
dir.create(paste0(opt$output,"/overall_models/location_only"))


models <-  list(
  
  # list(
  #   tag="country_only",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code))
  # ),
  # 
  # list(
  #   tag="country_county",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code) +
  #                (1 | iso_country_code:gdlcode))
  # ),
  # 
  # list(
  #   tag="country_county_village",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code) +
  #                (1 | iso_country_code:gdlcode) +
  #                (1 | iso_country_code:gdlcode:village))
  # ),
  # 
  # list(
  #   tag="country_county_village_kg",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code) +
  #                (1 | iso_country_code:gdlcode) +
  #                (1 | iso_country_code:gdlcode:village)+
  #                (1 | kg_class_name))
  # ),
  # 
  # list(
  #   tag="country_county_village_kg_form",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code) +
  #                (1 | iso_country_code:gdlcode) +
  #                (1 | iso_country_code:gdlcode:village)+
  #                (1 | kg_class_name)+
  #                (1 | id_form))
  # ),
  # 
  # list(
  #   tag="country_county_village_form",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code) +
  #                (1 | iso_country_code:gdlcode) +
  #                (1 | iso_country_code:gdlcode:village)+
  #                (1 | id_form))
  # ),
  # 
  # list(
  #   tag="country_county_form",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code) +
  #                (1 | iso_country_code:gdlcode) +
  #                (1 | id_form))
  # ),
  # 
  # 
  # list(
  #   tag="country_form",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code) +
  #                (1 | id_form))
  # ),
  # 
  # list(
  #   tag="country_village_form",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code) +
  #                (1 | iso_country_code:gdlcode:village)+
  #                (1 | id_form))
  # ),
  # 
  # list(
  #   tag="country_village_form",
  #   data=indicator_data,
  #   formula=bf(log_tva ~ 1 +  
  #                (1 | iso_country_code) +
  #                (1 | iso_country_code:gdlcode:village)+
  #                (1 | id_form))
  # ),
  
  
  list(
    tag="kg_only",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +  
                 (1 | kg_class_name))
  ),
  
  list(
    tag="kg_form",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +  
                 (1 | kg_class_name)+
                 (1 | id_form))
  ),
  
  list(
    tag="kg_village_form",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +  
                 (1 | kg_class_name)+
                 (1 | iso_country_code:gdlcode:village)+
                 (1 | id_form))
  ),
  
  list(
    tag="country_kg_form",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +  
                 (1 | iso_country_code)+
                 (1 | kg_class_name)+
                 (1 | id_form))
  )
  
)



# Running Model
model <- brm(
  
  formula=models[[opt$index]][["formula"]],
  data=models[[opt$index]][["data"]],
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


save(model,file=paste0(opt$output,"/overall_models/location_only/",models[[opt$index]][["tag"]],".rda"))

loo_model <- loo(model)
save(loo_model,file=paste0(opt$output,"/overall_models/location_only/loo_",models[[opt$index]][["tag"]],".rda"))
loo_model <- NULL

r2_model <- bayes_R2(loo_model)
save(r2_model,file=paste0(opt$output,"/overall_models/location_only/r2_",models[[opt$index]][["tag"]],".rda"))







