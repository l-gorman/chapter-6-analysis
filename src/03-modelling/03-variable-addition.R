# 

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



models <-  list(
  
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # TVA --------------------------------------------------------
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # Weak Prior Model --------------------------------------------------------
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  list(


    tag="weak_prior_fixed",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +

                 #------------------
               #Household Level
               # Demographics
               log_hh_size +
                 education_cleaned +

                 #Assets
                 log_livestock_tlu +
                 log_land_cultivated +

                 # Practices
                 off_farm_any+
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+

                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi+

                 # Levels
                 (1 | iso_country_code) +
                 (1 | iso_country_code:village)),
    prior="weak"
  ),


  list(
    tag="weak_prior_mixed_country",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned +

                 #Assets
                 log_livestock_tlu +
                 log_land_cultivated +

                 # Practices
                 off_farm_any+
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+

                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi+
                 # Levels
                 (1 +
                    log_land_cultivated +
                    log_livestock_tlu +
                    off_farm_any | iso_country_code) +
                 (1 | iso_country_code:village)),
    prior="weak"),


  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # Horse Shoe Model --------------------------------------------------------
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------

  list(
    tag="horseshoe_fixed",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned +

                 #Assets
                 log_livestock_tlu +
                 log_land_cultivated +

                 # Practices
                 off_farm_any+
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+

                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi+

                 # Levels
                 (1 | iso_country_code) +
                 (1 | iso_country_code:village)),
    prior="horseshoe"
  ),


  list(
    tag="horseshoe_mixed_country",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned +

                 #Assets
                 log_livestock_tlu +
                 log_land_cultivated +

                 # Practices
                 off_farm_any+
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+

                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi+

                 # Levels
                 (1 +
                    log_land_cultivated +
                    log_livestock_tlu +
                    off_farm_any | iso_country_code) +
                 (1 | iso_country_code:village)),
    prior="horseshoe"),


  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # HDDS --------------------------------------------------------
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------

  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # Weak Prior Model --------------------------------------------------------
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  list(


    tag="weak_prior_fixed",
    data=indicator_data,
    formula=bf(norm_hdds_lean_season ~ 1 +

                 #------------------
               #Household Level
               # Demographics
               log_hh_size +
                 education_cleaned +

                 #Assets
                 log_livestock_tlu +
                 log_land_cultivated +

                 # Practices
                 off_farm_any+
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+

                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi+

                 # Levels
                 (1 | iso_country_code) +
                 (1 | iso_country_code:village)),
    prior="weak"
  ),


  list(
    tag="weak_prior_mixed_country",
    data=indicator_data,
    formula=bf(norm_hdds_lean_season ~ 1 +
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned +

                 #Assets
                 log_livestock_tlu +
                 log_land_cultivated +

                 # Practices
                 off_farm_any+
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+
                 log_pop_dens +
                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi+
                 # Levels
                 (1 +
                    log_land_cultivated +
                    log_livestock_tlu +
                    off_farm_any | iso_country_code) +
                 (1 | iso_country_code:village)),
    prior="weak"),


  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # Horse Shoe Model --------------------------------------------------------
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------

  list(
    tag="horseshoe_fixed",
    data=indicator_data,
    formula=bf(norm_hdds_lean_season ~ 1 +
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned +

                 #Assets
                 log_livestock_tlu +
                 log_land_cultivated +

                 # Practices
                 off_farm_any+
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+

                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi+

                 # Levels
                 (1 | iso_country_code) +
                 (1 | iso_country_code:village)),
    prior="horseshoe"
  ),


  list(
    tag="horseshoe_mixed_country",
    data=indicator_data,
    formula=bf(norm_hdds_lean_season ~ 1 +
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned +

                 #Assets
                 log_livestock_tlu +
                 log_land_cultivated +

                 # Practices
                 off_farm_any+
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+

                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi+

                 # Levels
                 (1 +
                    log_land_cultivated +
                    log_livestock_tlu +
                    off_farm_any | iso_country_code) +
                 (1 | iso_country_code:village)),
    prior="horseshoe"),


  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # Fixed effects only ------------------------------------------------------
  # -------------------------------------------------------------------------
  # -------------------------------------------------------------------------
  
  list(
    tag="weak_fixed_only",
    data=indicator_data,
    formula=bf(norm_hdds_lean_season ~ 1 +  
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned + 
                 
                 #Assets
                 log_livestock_tlu + 
                 log_land_cultivated + 
                 
                 # Practices
                 off_farm_any+ 
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+
                 
                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi),
    prior="weak"),
  
  list(
    tag="horseshoe_fixed_only",
    data=indicator_data,
    formula=bf(norm_hdds_lean_season ~ 1 +  
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned + 
                 
                 #Assets
                 log_livestock_tlu + 
                 log_land_cultivated + 
                 
                 # Practices
                 off_farm_any+ 
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+
                 
                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi),
    prior="horseshoe"),
  
  list(
    tag="weak_fixed_only",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +  
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned + 
                 
                 #Assets
                 log_livestock_tlu + 
                 log_land_cultivated + 
                 
                 # Practices
                 off_farm_any+ 
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+
                 
                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi),
    prior="weak"),
  
  list(
    tag="horseshoe_fixed_only",
    data=indicator_data,
    formula=bf(log_tva ~ 1 +  
                 #Household Level
                 # Demographics
                 log_hh_size +
                 education_cleaned + 
                 
                 #Assets
                 log_livestock_tlu + 
                 log_land_cultivated + 
                 
                 # Practices
                 off_farm_any+ 
                 till_not_by_hand+
                 external_labour+
                 pesticide+
                 debts_have+
                 aidreceived+
                 livestock_inputs_any+
                 land_irrigated_any+
                 
                 #------------------
               # Village Level
               norm_growing_period +
                 log_min_travel_time +
                 log_pop_dens +
                 #------------------
               #County Level
               norm_gdl_country_shdi),
    prior="horseshoe")
  
  
  
  
)



opt$index <- as.numeric(opt$index)

if(models[[opt$index]][["prior"]]=="weak"){
  prior_to_use <- c(
    set_prior('normal(0, 1)', class = 'b'),
    set_prior('normal(0, 1)', class = 'sd'),
    set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  )
}

if(models[[opt$index]][["prior"]]=="horseshoe"){
  prior_to_use <- c(
    set_prior('horseshoe(1)', class = 'b'),
    set_prior('normal(0, 1)', class = 'sd'),
    set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  )
}

if (models[[opt$index]][["prior"]]=="weak" & grepl("fixed_only",models[[opt$index]][["tag"]])){
  prior_to_use <- c(
    set_prior('normal(0, 1)', class = 'b'),
    set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  )
}

if (models[[opt$index]][["prior"]]=="horseshoe" & grepl("fixed_only",models[[opt$index]][["tag"]])){
  prior_to_use <- c(
    set_prior('horseshoe(1)', class = 'b'),
    set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  )
}


model <- brm(
  
  formula=models[[opt$index]][["formula"]],
  data=models[[opt$index]][["data"]],
  prior = prior_to_use,
  cores = opt$ncores,
  backend = "cmdstanr",
  iter = opt$iter,
  warmup = opt$warmup,
  
  
  family=gaussian() 
)


dir.create(paste0(opt$output,"/overall_models/variable_addition/tva"))
dir.create(paste0(opt$output,"/overall_models/variable_addition/hdds"))



if (grepl("hdds",as.character(models[[opt$index]][["formula"]])[1])){
  base_path <- paste0(opt$output,"/overall_models/variable_addition/hdds/")
}

if(grepl("tva",as.character(models[[opt$index]][["formula"]])[1])){
  base_path <- paste0(opt$output,"/overall_models/variable_addition/tva/")
}



save(model,file=paste0(base_path,models[[opt$index]][["tag"]],".rda"))

loo_model <- loo(model)
save(loo_model,file=paste0(base_path,"loo_",models[[opt$index]][["tag"]],".rda"))
loo_model <- NULL

r2_model <- bayes_R2(model)
save(r2_model,file=paste0(base_path,"r2_",models[[opt$index]][["tag"]],".rda"))


