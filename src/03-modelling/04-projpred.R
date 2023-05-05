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


# Solution to globals size, found here:
# https://stackoverflow.com/questions/40536067/how-to-adjust-future-global-maxsize
options(future.globals.maxSize = 8000 * 1024^2)

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

ref_model <- loadRData(paste0(opt$output,"/weak_prior_mixed_country.rda"))
ref_model <- projpred::get_refmodel(ref_model)

seed <- as.numeric(opt$index)

varsel_model <- cv_varsel(ref_model,
                          method = 'forward', 
                          cv_method = 'kfold', 
                          K = 5, 
                          verbose = TRUE, 
                          seed = seed)

save(varsel_model,file=paste0(opt$output,"/proj_pred/proj_pred_varsel_model_",seed,".rda"))
