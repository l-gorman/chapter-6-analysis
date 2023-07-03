# sbatch src/03-modelling/05-refitting.sh

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
#   index='1'
# )


options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading
options(loo.cores = opt$ncores)


opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)


writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))
indicator_data <- readr::read_csv(paste0(opt$data,"/02-prepared-data/modelling_df.csv"))



index <- as.numeric(opt$index)





dir.create(paste0(opt$output,"/overall_models/"))
dir.create(paste0(opt$output,"/overall_models/variable_addition_final_fit"))

variable_order <- c("number_income_sources",
                    "hh_size",
                    "market_orientation",
                    "land_cultivated",
                    "use_fert",
                    "land_irrigated_any",
                    "education",
                    "external_labour",
                    "assisted_tillage",
                    "min_travel_time",
                    "length_growing_period",
                    "gdl_country_shdi",
                    "livestock_tlu",
                    "livestock_inputs_any",
                    "kitchen_garden",
                    "off_farm_any",
                    "debts_have")

grouping_vars <- c("(1 | iso_country_code)",
                   "(1 | iso_country_code:village)")

# for (var in c(1:(2*length(variable_order)))){




if (index <=length(variable_order)){
  
  auxilliary_variables <-variable_order[c(1:index)]
  formula_end <- paste0(
    paste0(grouping_vars,collapse = " + "),
    " + ",
    paste0(auxilliary_variables,collapse = " + ")
  )
  
  y_var <- "tva"
  model_name <-paste0(y_var,"_",index)
  forumla_temp <- bf(paste0(y_var, " ~ ", formula_end))
  
  model_item <-  list(
    tag=model_name,
    data=indicator_data,
    formula=forumla_temp,
    prior="weak")
  
}


if (index >length(variable_order)){
  auxilliary_variables <-variable_order[c(1:(index-length(variable_order)))]
  formula_end <- paste0(
    paste0(grouping_vars,collapse = " + "),
    " + ",
    paste0(auxilliary_variables,collapse = " + ")
  )
  
  y_var <- "hdds"
  model_name <-paste0(y_var,"_",index-length(variable_order))
  forumla_temp <- bf(paste0(y_var, " ~ ", formula_end))
  model_item <-  list(
    tag=model_name,
    data=indicator_data,
    formula=forumla_temp,
    prior="weak")
  
}
# models_all[[var]] <- model_item



# }





prior_to_use <- c(
  set_prior('normal(0, 1)', class = 'b'),
  set_prior('normal(0, 1)', class = 'sd'),
  set_prior('normal(0, 1)', class = 'sigma'),
  set_prior('normal(0, 1)', class = 'Intercept')
)



model <- brm(
  
  formula=model_item[["formula"]],
  data=model_item[["data"]],
  prior = prior_to_use,
  cores = opt$ncores,
  backend = "cmdstanr",
  iter = opt$iter,
  warmup = opt$warmup,
  
  
  family=gaussian() 
)


dir.create(paste0(opt$output,"/overall_models/variable_addition_final_fit/tva"))
dir.create(paste0(opt$output,"/overall_models/variable_addition_final_fit/hdds"))



if (grepl("hdds",as.character(model_item[["formula"]])[1])){
  base_path <- paste0(opt$output,"/overall_models/variable_addition_final_fit/hdds/")
}

if(grepl("tva",as.character(model_item[["formula"]])[1])){
  base_path <- paste0(opt$output,"/overall_models/variable_addition_final_fit/tva/")
}



save(model,file=paste0(base_path,model_item[["tag"]],".rda"))

loo_model <- loo(model)
save(loo_model,file=paste0(base_path,"loo_",model_item[["tag"]],".rda"))
loo_model <- NULL

r2_model <- bayes_R2(model)
save(r2_model,file=paste0(base_path,"r2_",model_item[["tag"]],".rda"))





# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Horse Shoe Model --------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# 
#   list(
#     tag="horseshoe_fixed",
#     data=indicator_data,
#     formula=bf(log_tva ~ 1 +
#                  #Household Level
#                  # Demographics
#                  log_hh_size +
#                  education_cleaned +
# 
#                  #Assets
#                  log_livestock_tlu +
#                  log_land_cultivated +
# 
#                  # Practices
#                  off_farm_any+
#                  till_not_by_hand+
#                  external_labour+
#                  pesticide+
#                  debts_have+
#                  aidreceived+
#                  livestock_inputs_any+
#                  land_irrigated_any+
# 
#                  #------------------
#                # Village Level
#                  norm_growing_period +
#                  log_min_travel_time +
#                  log_pop_dens +
#                  #------------------
#                #County Level
#                norm_gdl_country_shdi+
# 
#                  # Levels
#                  (1 | iso_country_code) +
#                  (1 | iso_country_code:village)),
#     prior="horseshoe"
#   ),
# 
# 
#   list(
#     tag="horseshoe_mixed_country",
#     data=indicator_data,
#     formula=bf(log_tva ~ 1 +
#                  #Household Level
#                  # Demographics
#                  log_hh_size +
#                  education_cleaned +
# 
#                  #Assets
#                  log_livestock_tlu +
#                  log_land_cultivated +
# 
#                  # Practices
#                  off_farm_any+
#                  till_not_by_hand+
#                  external_labour+
#                  pesticide+
#                  debts_have+
#                  aidreceived+
#                  livestock_inputs_any+
#                  land_irrigated_any+
# 
#                  #------------------
#                # Village Level
#                norm_growing_period +
#                  log_min_travel_time +
#                  log_pop_dens +
#                  #------------------
#                #County Level
#                norm_gdl_country_shdi+
# 
#                  # Levels
#                  (1 +
#                     log_land_cultivated +
#                     log_livestock_tlu +
#                     off_farm_any | iso_country_code) +
#                  (1 | iso_country_code:village)),
#     prior="horseshoe"),
# 


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Horse Shoe Model --------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# list(
#   tag="horseshoe_fixed",
#   data=indicator_data,
#   formula=bf(norm_hdds_lean_season ~ 1 +
#                #Household Level
#                # Demographics
#                log_hh_size +
#                education_cleaned +
# 
#                #Assets
#                log_livestock_tlu +
#                log_land_cultivated +
# 
#                # Practices
#                off_farm_any+
#                till_not_by_hand+
#                external_labour+
#                pesticide+
#                debts_have+
#                aidreceived+
#                livestock_inputs_any+
#                land_irrigated_any+
# 
#                #------------------
#              # Village Level
#              norm_growing_period +
#                log_min_travel_time +
#                log_pop_dens +
#                #------------------
#              #County Level
#              norm_gdl_country_shdi+
# 
#                # Levels
#                (1 | iso_country_code) +
#                (1 | iso_country_code:village)),
#   prior="horseshoe"
# ),

# 
#   list(
#     tag="horseshoe_mixed_country",
#     data=indicator_data,
#     formula=bf(norm_hdds_lean_season ~ 1 +
#                  #Household Level
#                  # Demographics
#                  log_hh_size +
#                  education_cleaned +
# 
#                  #Assets
#                  log_livestock_tlu +
#                  log_land_cultivated +
# 
#                  # Practices
#                  off_farm_any+
#                  till_not_by_hand+
#                  external_labour+
#                  pesticide+
#                  debts_have+
#                  aidreceived+
#                  livestock_inputs_any+
#                  land_irrigated_any+
# 
#                  #------------------
#                # Village Level
#                norm_growing_period +
#                  log_min_travel_time +
#                  log_pop_dens +
#                  #------------------
#                #County Level
#                norm_gdl_country_shdi+
# 
#                  # Levels
#                  (1 +
#                     log_land_cultivated +
#                     log_livestock_tlu +
#                     off_farm_any | iso_country_code) +
#                  (1 | iso_country_code:village)),
#     prior="horseshoe"),

