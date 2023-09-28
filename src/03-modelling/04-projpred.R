# sbatch src/03-modelling/04-projpred.sh


library(brms)
library(ggplot2)
library(ggridges)
library(dplyr)
library(tidyr)
library(ggdist)
library(magrittr)
library(optparse)
library(projpred)
library(cmdstanr)
library(optimx)
library(lme4)

set_cmdstan_path("/user/home/lg14410/.cmdstan/cmdstan-2.32.2")

# Solution to globals size, found here:
# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
options(future.globals.maxSize = 150000 * 1024^2)
# options(mc.cores = parallel::detectCores())
options(mc.cores = 4,  brms.backend = "cmdstanr")

option_list = list(
  make_option(c("-o", "--output"), type='character',
              help="The directory where results will be written"),
  make_option(c("-c", "--ncores"), type='character',
              help="The number of chains/cores"),
  
  make_option(c("-j", "--index"), type='character',
              help="Index of proj-pred selection")
)

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
# 

# opt <- list(
#   output="outputs/14_04_2023/outputs/overall_models/",
#   ncores=4,
#   index="1"
# )
writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))

opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)
dir.create(opt$output,"/proj_pred/")

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


# Syntax & Usage
start <- Sys.time()
# Add here R code or 
# Add function call




seed <- as.numeric(opt$index)


if (seed==1){
  # ref_model <- loadRData(paste0(opt$output,"/tva/weak_prior_fixed.rda"))
  ref_model <- loadRData(paste0(opt$output,"/tva/weak_prior_mixed_country.rda"))
  
  dir.create(paste0(opt$output,"/proj_pred/"))
  dir.create(paste0(opt$output,"/proj_pred/tva"))
  # output_dir <- paste0(opt$output,"/proj_pred/tva/weak_prior_fixed")
  output_dir <- paste0(opt$output,"/proj_pred/tva/weak_prior_mixed_country")
  
  dir.create(output_dir)
  
}

if (seed==2){
  # ref_model <- loadRData(paste0(opt$output,"/hdds/weak_prior_fixed.rda"))
  ref_model <- loadRData(paste0(opt$output,"/hdds/weak_prior_mixed_country.rda"))
  
  dir.create(paste0(opt$output,"/proj_pred/"))
  dir.create(paste0(opt$output,"/proj_pred/hdds"))
  # output_dir <- paste0(opt$output,"/proj_pred/hdds/weak_prior_fixed")
  output_dir <- paste0(opt$output,"/proj_pred/hdds/weak_prior_mixed_country")
  
  dir.create(output_dir)
  
}

ref_model <- get_refmodel(ref_model)

# Adapted from Frank Weber's Solution
# https://github.com/stan-dev/projpred/issues/346

force_search_terms <- function(forced_terms, optional_terms) {
  forced_terms <- paste(forced_terms, collapse = " + ")
  return(c(forced_terms, paste0(forced_terms, " + ", optional_terms)))
}

# get_search_terms <- function(fixed_terms, other_predictors, max_terms) {
#   
#   if (max_terms > length(other_predictors)){
#     stop("Cannot have max terms more than predictors")
#   }
#   
#   search_terms <- unlist(lapply(1:max_terms, function(m_predictors) {
#     lapply(combn(other_predictors, m = m_predictors, simplify = FALSE),
#            function(idxs_predictors) {
#              paste0(idxs_predictors, collapse = " + ")
#            })
#   }))
#   search_terms <- c(fixed_terms, paste(fixed_terms, "+", search_terms))
#   return(search_terms)
# }



search_terms <- c(
  
    "(1 | iso_country_code) + (1 | iso_country_code_village) + hh_size",
    "(1 + hh_size | iso_country_code) + (1 | iso_country_code_village) + hh_size",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + education",
    "(1 + education | iso_country_code) + (1 | iso_country_code_village) + education",
    
    
    #Assets
    "(1 | iso_country_code) + (1 | iso_country_code_village) + livestock_tlu",
    "(1 + livestock_tlu | iso_country_code) + (1 | iso_country_code_village) + livestock_tlu",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + land_cultivated",
    "(1 + land_cultivated | iso_country_code) + (1 | iso_country_code_village) + land_cultivated",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + market_orientation",
    "(1 + market_orientation | iso_country_code) + (1 | iso_country_code_village) + market_orientation",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + debts_have",
    "(1 + debts_have | iso_country_code) + (1 | iso_country_code_village) + debts_have",
    

    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + off_farm_any",
    "(1 + off_farm_any | iso_country_code) + (1 | iso_country_code_village) + off_farm_any",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + kitchen_garden",
    "(1 + kitchen_garden | iso_country_code) + (1 | iso_country_code_village) + kitchen_garden",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + number_income_sources",
    "(1 + number_income_sources | iso_country_code) + (1 | iso_country_code_village) + number_income_sources",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + market_orientation",
    "(1 + market_orientation | iso_country_code) + (1 | iso_country_code_village) + market_orientation",
    
    # Practices
    "(1 | iso_country_code) + (1 | iso_country_code_village) + assisted_tillage",
    "(1 + assisted_tillage | iso_country_code) + (1 | iso_country_code_village) + assisted_tillage",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + external_labour",
    "(1 + external_labour | iso_country_code) + (1 | iso_country_code_village) + external_labour",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + livestock_inputs_any",
    "(1 + livestock_inputs_any | iso_country_code) + (1 | iso_country_code_village) + livestock_inputs_any",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + land_irrigated_any",
    "(1 + land_irrigated_any | iso_country_code) + (1 | iso_country_code_village) + land_irrigated_any",
    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + use_fert",
    "(1 + use_fert | iso_country_code) + (1 | iso_country_code_village) + use_fert",
    
    
    
    # Village level
    "(1 | iso_country_code) + (1 | iso_country_code_village) + length_growing_period",
    "(1 + length_growing_period | iso_country_code) + (1 | iso_country_code_village) + length_growing_period",
    

    "(1 | iso_country_code) + (1 | iso_country_code_village) + min_travel_time",
    "(1 + min_travel_time | iso_country_code) + (1 | iso_country_code_village) + min_travel_time",
    

    
    "(1 | iso_country_code) + (1 | iso_country_code_village) + gdl_country_shdi")



# group_effects <-"(1 | iso_country_code) + (1 | iso_country_code_village)"
# fixed_effects <- paste0(group_effects, " + ", fixed_effects)

# all_args <- c(auxilliary_variables,group_effects)
# max_vars <- length(all_args)-1

# max_vars <- length(auxilliary_variables)


# Basing this off of discussion on stan forum:
# https://discourse.mc-stan.org/t/projpred-fixing-group-effects-in-search-terms-and-tips-for-speed/31678/4
# search_terms <- force_search_terms(forced_terms=group_effects,
#                                  optional_terms=auxilliary_variables)


# Basing from this: https://discourse.mc-stan.org/t/advice-on-using-search-terms-in-projpred/22846/3
varsel_model <- cv_varsel(ref_model,
                          method = 'forward', 
                          cv_method = 'kfold', 
                          K = 5,
                          verbose = TRUE, 
                          seed = seed,
                          nclusters=3,
                          # ndraws_pred=2000,
                          # search_terms=search_terms,
                          search_terms=search_terms,
                          # nterms_max=max_vars,
                          # refit_prj=TRUE,
                          control=lme4::lmerControl(optimizer = "Nelder_Mead")
                          )

save(varsel_model,file=paste0(output_dir,"/projpred_cv_varsel_model_",seed,".rda"))

print("Execution Time")
print( Sys.time() - start )

selection_summary <- unclass(summary(varsel_model))[["selection"]]
save(selection_summary,file=paste0(output_dir,"/selection_summary_",seed,".rda"))

rk <- ranking(varsel_model)
cv_props_direct <- cv_proportions(rk)
save(cv_props_direct,file=paste0(output_dir,"/cv_props_direct_",seed,".rda"))

cv_props_cumul <- cv_proportions(rk, cumulate = TRUE)
save(cv_props_cumul,file=paste0(output_dir,"/cv_props_cumul_",seed,".rda"))

print("Done")



