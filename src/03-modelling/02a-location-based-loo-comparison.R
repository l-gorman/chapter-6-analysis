# sbatch src/03-modelling/02a-location-based-loo-comparison.sh 
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


loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

option_list = list(
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
#   output="./outputs/14_04_2023/outputs/",
#   ncores="4"
# )


options(loo.cores = opt$ncores)
options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading

opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)


# dir.create(paste0(opt$output,"/overall_models/"))
# dir.create(paste0(opt$output,"/overall_models/location_only"))


country_only <- loadRData(paste0(opt$output,"/overall_models/location_only/country_only.rda"))
bayes_2_country_only <- bayes_R2(country_only)
loo_country_only <- loo(country_only)

country_county <- loadRData(paste0(opt$output,"/overall_models/location_only/country_county.rda"))
bayes_2_country_county <- bayes_R2(country_county)
loo_country_county <- loo(country_county)

country_county_village <- loadRData(paste0(opt$output,"/overall_models/location_only/country_county_village.rda"))
bayes_2_country_county_village <- bayes_R2(country_county_village)
loo_county_country_village <- loo(county_country_village)

country_county_village_kg <- loadRData(paste0(opt$output,"/overall_models/location_only/country_county_village_kg.rda"))
bayes_2_country_county_village_kg <- bayes_R2(country_county_village_kg)
loo_county_country_village_kg <- loo(county_country_village_kg)

country_county_village_kg_form <- loadRData(paste0(opt$output,"/overall_models/location_only/country_county_village_kg_form.rda"))
bayes_2_country_county_village_kg_form <- bayes_R2(country_county_village_kg_form)
loo_county_country_village_kg_form <- loo(county_country_village_kg_form)

country_county_village_form <- loadRData(paste0(opt$output,"/overall_models/location_only/country_county_village_form.rda"))
bayes_2_country_county_village_form <- bayes_R2(country_county_village_form)
country_county_village_form <- loo(country_county_village_form)

country_county_form <- loadRData(paste0(opt$output,"/overall_models/location_only/country_county_form.rda"))
bayes_2_country_county_village_form <- bayes_R2(country_county_village_form)
country_county_form <- loo(country_county_form)

country_form <- loadRData(paste0(opt$output,"/overall_models/location_only/country_form.rda"))
country_form <- loo(country_form)


country_village_form <- loadRData(paste0(opt$output,"/overall_models/location_only/country_village_form.rda"))
country_village_form <- loo(country_village_form)




# country_only <- add_criterion(country_only, "loo")
# country_county <- add_criterion(country_county, "loo")
# county_country_village <- add_criterion(county_country_village, "loo")
# county_country_village_kg <- add_criterion(county_country_village_kg, "loo")
# county_country_village_kg_form <- add_criterion(county_country_village_kg_form, "loo")

loo_results <- loo_compare(
  loo_country_only,
  loo_country_county,
  loo_county_country_village,
  loo_county_country_village_kg,
  loo_county_country_village_kg_form,
  country_county_village_form,
  country_county_form,
  country_form,
  country_village_form, criterion = c("loo"))

save(loo_results,file=paste0(opt$output,"/overall_models/location_only/loo_comparison.rda"))



