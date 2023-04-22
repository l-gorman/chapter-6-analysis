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

if (as.numeric(opt$index)==1)
{
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
  
  loo_country_only <- loo(country_only)
  save(loo_country_only,file=paste0(opt$output,"/overall_models/location_only/loo_country_only.rda"))
  
  r2_country_only <- bayes_R2(country_only)
  save(r2_country_only,file=paste0(opt$output,"/overall_models/location_only/r2_country_only.rda"))
}


if (as.numeric(opt$index)==2)
{
  country_county <- brm(
    formula=log_tva ~ 1 +  
      (1 | iso_country_code) +
      (1 | iso_country_code:gdlcode),
    
    # (1 | village),
    data = indicator_data,
    prior = c(
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
  save(country_county,file=paste0(opt$output,"/overall_models/location_only/country_county.rda"))
  
  loo_country_county <- loo(country_county)
  save(loo_country_county,file=paste0(opt$output,"/overall_models/location_only/loo_country_county.rda"))
  
  r2_country_county <- bayes_R2(country_county)
  save(r2_country_county,file=paste0(opt$output,"/overall_models/location_only/r2_country_county.rda"))
  
  
}

if (as.numeric(opt$index)==3)
{
  country_county_village <- brm(
    formula=log_tva ~ 1 +  
      (1 | iso_country_code) +
      (1 | iso_country_code:gdlcode) +
      (1 | iso_country_code:gdlcode:village),
    
    # (1 | village),
    data = indicator_data,
    prior = c(
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
  save(country_county_village,file=paste0(opt$output,"/overall_models/location_only/country_county_village.rda"))
  
  loo_country_county_village <- loo(country_county_village)
  save(loo_country_county_village,file=paste0(opt$output,"/overall_models/location_only/loo_country_county_village.rda"))
  
  r2_country_county_village <- bayes_R2(country_county_village)
  save(r2_country_county_village,file=paste0(opt$output,"/overall_models/location_only/r2_country_county_village.rda"))
  
  }



if (as.numeric(opt$index)==4)
{
  country_county_village_kg <- brm(
    formula=log_tva ~ 1 +  
      (1 | iso_country_code) +
      (1 | iso_country_code:gdlcode) +
      (1 | iso_country_code:gdlcode:village)+
      (1 | kg_class_name),
    
    # (1 | village),
    data = indicator_data,
    prior = c(
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
  
  save(country_county_village_kg,file=paste0(opt$output,"/overall_models/location_only/country_county_village_kg.rda"))
  
  loo_country_county_village_kg <- loo(country_county_village_kg)
  save(loo_country_county_village_kg,file=paste0(opt$output,"/overall_models/location_only/loo_country_county_village_kg.rda"))
  
  r2_country_county_village_kg <- bayes_R2(country_county_village_kg)
  save(r2_country_county_village_kg,file=paste0(opt$output,"/overall_models/location_only/r2_country_county_village_kg.rda"))
  
}


if (as.numeric(opt$index)==5)
{
  country_county_village_kg_form <- brm(
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
      set_prior('normal(0, 1)', class = 'sigma'),
      set_prior('normal(0, 1)', class = 'Intercept')
    ),
    cores = 4,
    backend = "cmdstanr",
    iter = opt$iter,
    warmup = opt$warmup,
    
    
    family=gaussian() 
  )
  
  save(country_county_village_kg_form,file=paste0(opt$output,"/overall_models/location_only/country_county_village_kg_form.rda"))
  
  loo_country_county_village_kg_form <- loo(country_county_village_kg_form)
  save(loo_country_county_village_kg_form,file=paste0(opt$output,"/overall_models/location_only/loo_country_county_village_kg_form.rda"))
  
  r2_country_county_village_kg_form <- bayes_R2(country_county_village_kg_form)
  save(r2_country_county_village_kg_form,file=paste0(opt$output,"/overall_models/location_only/r2_country_county_village_kg_form.rda"))
  
  
  
}


if (as.numeric(opt$index)==6)
{
  country_county_village_form <- brm(
    formula=log_tva ~ 1 +  
      (1 | iso_country_code) +
      (1 | iso_country_code:gdlcode) +
      (1 | iso_country_code:gdlcode:village)+
      (1 | id_form),
    
    # (1 | village),
    data = indicator_data,
    prior = c(
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
  
  save(country_county_village_form,file=paste0(opt$output,"/overall_models/location_only/country_county_village_form.rda"))
  
  loo_country_county_village_form <- loo(country_county_village_form)
  save(loo_country_county_village_form,file=paste0(opt$output,"/overall_models/location_only/loo_country_county_village_form.rda"))
  
  r2_country_county_village_form <- bayes_R2(country_county_village_form)
  save(r2_country_county_village_form,file=paste0(opt$output,"/overall_models/location_only/r2_country_county_village_form.rda"))
  
}


if (as.numeric(opt$index)==7)
{
  country_county_form <- brm(
    formula=log_tva ~ 1 +  
      (1 | iso_country_code) +
      (1 | iso_country_code:gdlcode) +
      (1 | id_form),
    
    # (1 | village),
    data = indicator_data,
    prior = c(
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
  
  save(country_county_form,file=paste0(opt$output,"/overall_models/location_only/country_county_form.rda"))
  
  loo_country_county_form <- loo(country_county_form)
  save(loo_country_county_form,file=paste0(opt$output,"/overall_models/location_only/loo_country_county_form.rda"))
  
  r2_country_county_form <- bayes_R2(country_county_form)
  save(r2_country_county_form,file=paste0(opt$output,"/overall_models/location_only/r2_country_county_form.rda"))
  
  
  }


if (as.numeric(opt$index)==8)
{
  country_form <- brm(
    formula=log_tva ~ 1 +  
      (1 | iso_country_code) +
      (1 | id_form),
    
    # (1 | village),
    data = indicator_data,
    prior = c(
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
  
  save(country_form,file=paste0(opt$output,"/overall_models/location_only/country_form.rda"))
  
  loo_country_form <- loo(country_form)
  save(loo_country_form,file=paste0(opt$output,"/overall_models/location_only/loo_country_form.rda"))
  
  r2_country_form <- bayes_R2(country_form)
  save(r2_country_form,file=paste0(opt$output,"/overall_models/location_only/r2_country_form.rda"))
  
  
}


if (as.numeric(opt$index)==9)
{
  country_village_form <- brm(
    formula=log_tva ~ 1 +  
      (1 | iso_country_code) +
      (1 | iso_country_code:gdlcode:village)+
      (1 | id_form),
    
    # (1 | village),
    data = indicator_data,
    prior = c(
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
  
  save(country_village_form,file=paste0(opt$output,"/overall_models/location_only/country_village_form.rda"))
  
  loo_country_village_form <- loo(country_village_form)
  save(loo_country_village_form,file=paste0(opt$output,"/overall_models/location_only/loo_country_village_form.rda"))
  
  r2_country_village_form <- bayes_R2(country_village_form)
  save(r2_country_village_form,file=paste0(opt$output,"/overall_models/location_only/r2_country_village_form.rda"))
  
}





