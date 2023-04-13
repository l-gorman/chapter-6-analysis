# sbatch src/03-modelling/01-per-project-models.sh 
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


# summary_per_proj <- indicator_data %>% 
#  group_by(id_form) %>% 
#   summarise(gdlcode=n_distinct(gdlcode),
#             village=n_distinct(village),
#             aez_class_cleaned=n_distinct(aez_class_cleaned),
#             education_cleaned=n_distinct(education_cleaned))



project_id <- unique(indicator_data$id_form)[as.numeric(opt$proj)]

subset_df <- indicator_data[indicator_data$id_form==project_id,]

subset_df$combined_fs_score <- factor(subset_df$combined_fs_score,
                                      levels=c("severely_fi","moderately_fi","mildly_fi","not_fi"),
                                      ordered = T)








dir.create(paste0(opt$output,"/",project_id))

horseshoe_food_sec <- brm(
  formula=combined_fs_score ~ 1 +  
    education_cleaned +
    log_livestock_tlu + 
    log_land_cultivated + 
    logit_livestock_orientation +
    logit_crop_orientation + 
    logit_off_farm_orientation +
    logit_market_orientation +
    log_income_diversity +
    norm_growing_period +
    log_min_travel_time +
    # aez_class_cleaned +
    norm_gdl_lifexp +
    logit_gdl_hdi + 
    # (1 | iso_country_code) + 
    # (1 | iso_country_code:gdlcode) + 
    # (1 | iso_country_code:gdlcode:village),
    (1 | village),
  data = subset_df,
  prior = c(
    set_prior("horseshoe(1)", class="b"),# HorseShoe
    set_prior('normal(0, 1)', class = 'sd'),
    # set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  ),
  cores = 4,
  backend = "cmdstanr",
  iter = opt$iter,
  warmup = opt$warmup,
  
  
  family=cumulative("logit") 
)

save(horseshoe_food_sec,file=paste0(opt$output,"/",project_id,"/horseshoe_food_sec.rda"))

horseshoe_tva <- brm(
  formula=log_tva ~ 1 +  
    education_cleaned +
    log_livestock_tlu + 
    log_land_cultivated + 
    logit_livestock_orientation +
    logit_crop_orientation + 
    logit_off_farm_orientation +
    logit_market_orientation +
    log_income_diversity +
    norm_growing_period +
    log_min_travel_time +
    # aez_class_cleaned +
    norm_gdl_lifexp +
    logit_gdl_hdi + 
    # (1 | iso_country_code) + 
    # (1 | iso_country_code:gdlcode) + 
    # (1 | iso_country_code:gdlcode:village),
    (1 | village),
  data = subset_df,
  prior = c(
    set_prior("horseshoe(1)", class="b"),# HorseShoe
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


save(horseshoe_tva,file=paste0(opt$output,"/",project_id,"/horseshoe_tva.rda"))


weak_prior_food_sec <- brm(
  formula=combined_fs_score ~ 1 +  
    education_cleaned +
    log_livestock_tlu + 
    log_land_cultivated + 
    logit_livestock_orientation +
    logit_crop_orientation + 
    logit_off_farm_orientation +
    logit_market_orientation +
    log_income_diversity +
    norm_growing_period +
    log_min_travel_time +
    # aez_class_cleaned +
    norm_gdl_lifexp +
    logit_gdl_hdi + 
    # (1 | iso_country_code) + 
    # (1 | iso_country_code:gdlcode) + 
    # (1 | iso_country_code:gdlcode:village),
    (1 | village),
  data = subset_df,
  prior = c(
    set_prior("normal(0, 1)", class = "b"),
    set_prior('normal(0, 1)', class = 'sd'),
    # set_prior('normal(0, 1)', class = 'sigma'),
    set_prior('normal(0, 1)', class = 'Intercept')
  ),
  cores = 4,
  backend = "cmdstanr",
  iter = opt$iter,
  warmup = opt$warmup,
  
  
  family=cumulative("logit") 
)

save(weak_prior_food_sec,file=paste0(opt$output,"/",project_id,"/weak_prior_food_sec.rda"))

weak_prior_tva <- brm(
  formula=log_tva ~ 1 +  
    education_cleaned +
    log_livestock_tlu + 
    log_land_cultivated + 
    logit_livestock_orientation +
    logit_crop_orientation + 
    logit_off_farm_orientation +
    logit_market_orientation +
    log_income_diversity +
    norm_growing_period +
    log_min_travel_time +
    # aez_class_cleaned +
    norm_gdl_lifexp +
    logit_gdl_hdi + 
    # (1 | iso_country_code) + 
    # (1 | iso_country_code:gdlcode) + 
    # (1 | iso_country_code:gdlcode:village),
    (1 | village),
  data = subset_df,
  prior = c(
    set_prior("normal(0, 1)", class = "b"),
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


save(weak_prior_tva,file=paste0(opt$output,"/",project_id,"/weak_prior_tva.rda"))







