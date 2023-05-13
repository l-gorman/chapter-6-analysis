# sbatch src/05-simulation/01-simulation.sh 

library(brms)
library(tidybayes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(bayesplot)
library(optparse)
library(tibble)
library(readr)

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

# opt <- list(
#   iter=20,
#   warmup=10,
#   output="./outputs/test_simulation",
#   ncores=4,
#   index=1
# )


#' Simulating multi-level models
#'


options(mc.cores = opt$ncores, brms.backend = "cmdstanr") # allows threading
opt$output <- gsub("/$", "", opt$output)

dir.create(opt$output)
writeLines("test_file_output",paste0(opt$output,"/test_file.txt"))

dir.create(paste0(opt$output,"/simulation/"))
opt$index <- as.numeric(opt$index)
i <- opt$index


generate_simulated_data <- function(
    seed=123,
    n_countries,
    
    mu=1,
    sigma=0.25,
    
    
    avg_projects_per_country,
    project_imbalance=0.25,
    
    avg_villages_per_project,
    village_imbalance=0.25,
    
    avg_individuals_per_village,
    individual_imbalance=0.25,
    
    project_vpc,
    country_vpc,
    village_vpc,
    unexplained_vpc,
    
    
    
    
    
    
    per_country_effect=NULL
    
){
  
  
  
  
  
  
  if (
    country_vpc+
    project_vpc + 
    village_vpc + 
    unexplained_vpc!=1
  ){
    stop("VPCs should add to one")
  }
  
  
  set.seed(seed)
  
  countries <- paste0("country_",c(1:n_countries))
  
  # Generating Projects and Countries
  #-------------------------------------------
  projects_per_country <- round(rnorm(n_countries,mean=
                                        avg_projects_per_country,
                                      sd =avg_projects_per_country * project_imbalance ),0)
  projects_per_country[projects_per_country==0] <- 1
  
  df <- lapply(c(1:length(countries)),function(i){
    number_projects <- projects_per_country[i]
    country_vec <- rep(countries[i],number_projects)
    project_vec <- paste0(countries[i], "_proj_",c(1:number_projects))
    
    return(tibble::as_tibble(list(
      country=country_vec,
      project=project_vec
    )))
    
  }) %>% bind_rows()
  
  # Generating Villages
  #-------------------------------------------
  
  villages_per_project <-  round(rnorm(nrow(df),
                                       mean=avg_villages_per_project,
                                       sd =avg_villages_per_project * village_imbalance ),0)
  
  villages_per_project[villages_per_project==0] <- 1
  
  
  df <- lapply(c(1:length(df$project)),function(i){
    
    country <- df$country[i]
    project <- df$project[i]
    
    number_villages <- villages_per_project[i]
    country_vec <- rep(country,number_villages)
    project_vec <- rep(project,number_villages)
    
    village_vec <- paste0(project_vec,"_village_",c(1:number_villages))
    
    return(tibble::as_tibble(list(
      country=country_vec,
      project=project_vec,
      village=village_vec
    )))
    
  }) %>% bind_rows()
  
  
  # Generating Individuals
  #-------------------------------------------
  individuals_per_village <-  round(rnorm(nrow(df),
                                          mean=avg_individuals_per_village,
                                          sd =avg_individuals_per_village * individual_imbalance ),0)
  
  individuals_per_village[individuals_per_village==0] <- 1
  
  
  df <- lapply(c(1:length(df$project)),function(i){
    
    country <- df$country[i]
    project <- df$project[i]
    village <- df$village[i]
    
    
    number_individuals <- individuals_per_village[i]
    country_vec <- rep(country,number_individuals)
    project_vec <- rep(project,number_individuals)
    village_vec <- rep(village, number_individuals)
    
    individual_vec <- paste0(village_vec,"_individual_",c(1:number_individuals))
    
    
    return(tibble::as_tibble(list(
      country=country_vec,
      project=project_vec,
      village=village_vec,
      individual = individual_vec
    )))
    
  }) %>% bind_rows()
  
  
  # Country Values
  countries <- unique(df$country)
  country_means <- rnorm(length(countries),mean = mu,sd = sigma*country_vpc)
  country_means <- tibble::as_tibble(list(
    country=countries,
    country_value=country_means
  ))
  
  df <- df %>% merge(country_means, by="country")
  
  # Project_values 
  projects <- df[c("project","country_value")]
  projects <- unique(projects)
  
  project_means <- rnorm(nrow(projects),mean = projects$country_value,sd =project_vpc*sigma)
  project_means <- tibble::as_tibble(list(
    project=projects$project,
    project_value=project_means
  ))
  
  df <- df %>% merge(project_means, by="project")
  
  # village values 
  villages <- df[c("village","project_value")]
  villages <- unique(villages)
  
  village_means <- rnorm(nrow(villages),mean = villages$project_value,sd =village_vpc*sigma)
  village_means <- tibble::as_tibble(list(
    village=villages$village,
    village_value=village_means
  ))
  
  df <- df %>% merge(village_means, by="village")
  
  
  # individual values
  individuals <- df[c("individual","village_value")]
  individuals <- unique(individuals)
  
  individual_values <- rnorm(nrow(individuals),mean = individuals$village_value,sd =unexplained_vpc*sigma)
  individual_values <- tibble::as_tibble(list(
    individual=individuals$individual,
    individual_value=individual_values
  ))
  
  df <- df %>% merge(individual_values, by="individual")
  
  return(df)
}



configs <- list(
  
  # 1 Project per Country, 20 countries
  list(
    n_countries = 20,
    avg_projects_per_country = 1,
    avg_villages_per_project=20,
    avg_individuals_per_village=20
  ),
  
  # 1 Country, many projects
  list(
    n_countries = 1,
    avg_projects_per_country = 20,
    avg_villages_per_project=20,
    avg_individuals_per_village=20
  ),
  
  # Many countries,
  # Many projects
  list(
    n_countries = 20,
    avg_projects_per_country = 20,
    avg_villages_per_project=20,
    avg_individuals_per_village=20
  )
  
)


temp_config <- configs[[i]]



dir.create(paste0(opt$output,"/simulation/simulation_",i))

write_csv(as_tibble(temp_config),paste0(opt$output,"/simulation/simulation_",i,"/config.csv"))


test_df <- generate_simulated_data(
  seed = 123,
  
  mu = 10,
  sigma = 4,
  
  n_countries = temp_config$n_countries,
  country_vpc = 0.4,
  
  avg_projects_per_country = temp_config$avg_projects_per_country,
  project_vpc=0.2,
  project_imbalance = 0.25,
  
  avg_villages_per_project = temp_config$avg_villages_per_project,
  village_imbalance = 0.25,
  village_vpc=0.1,
  
  avg_individuals_per_village = temp_config$avg_individuals_per_village,
  individual_imbalance = 0.25,
  
  unexplained_vpc=0.3
)

model <- brms::brm(formula = 
                     bf(individual_value ~ 1 +
                          (1 | country)+
                          (1 | country:project)+
                          (1 | country:project:village)),
                   data = test_df,
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


save(model,file=paste0(opt$output,"/simulation/simulation_",i,"/model.rda"))

r2_model <- bayes_R2(model)
save(r2_model,file=paste0(opt$output,"/simulation/simulation_",i,"/r2_model.rda"))


