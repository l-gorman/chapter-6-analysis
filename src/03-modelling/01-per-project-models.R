# sbatch src/bc-run-scripts/run_brms_anova_location_per_country.sh  -i 5000 -w 2000 -n 4 -o brms_anova_21_03_2023
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
  make_option(c("-j", "--proj"), type='character',
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
#   output="./outputs/test_per_proj",
#   ncores=4,
#   proj='1'
# )


options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading

opt$data <- gsub("/$", "", opt$data)
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)


writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))
indicator_data <- readr::read_csv(paste0(opt$data,"/02-prepared-data/modelling_df.csv"))

project_id <- unique(indicator_data$id_form)[as.numeric(opt$proj)]

subset_df <- indicator_data[indicator_data$id_form==project_id,]


levels <- c("iso_country_code",
            "gdlcode",
            "village")


forumula <-



country_codes <- unique(indicator_data$iso_country_code)
for (country in country_codes){
  temp_data <-indicator_data[indicator_data$iso_country_code==country,]
  dir.create(paste0(opt$output,"/continental_gaussian_location/per_country/",country,"/"))
  result <- run_model(temp_data,levels =  c("gdlcode","village"), sigma=F, iter=opt$iter, warmup=opt$warmup,ncores=opt$ncores)
  save(result,file=paste0(opt$output,"/continental_gaussian_location/per_country/",country,"/",paste0( c("ADM2_CODE","village"), collapse="_"),".rda"))
}




# loadRData <- function(fileName){
#   #loads an RData file, and returns it
#   load(fileName)
#   get(ls()[ls() != "fileName"])
# }
# d <- loadRData("outputs/brm_anov_31_01_2023/continental_gaussian_location/ADM0_NAME.rda")
# 



