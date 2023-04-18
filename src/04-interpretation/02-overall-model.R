library(readr)
library(tidyr)
library(tibble)
library(magrittr)
library(dplyr)
library(tidybayes)
library(brms)
library(ggplot2)
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


vpc <- function(model, params){
  
  draws_df <-  as_draws_df(model)[params]
  
  vpcs <- list()
  for (param in params){
    other_params <- params[params!=param]
    
    vpcs[[param]] <- draws_df[[param]]^2/rowSums(draws_df[params]^2)
  }
  vpcs <- vpcs %>% as_tibble()
  
  return(vpcs)
}




# Location Only TVA -------------------------------------------------------
location_tva_model <- loadRData("./outputs/14_04_2023/outputs/overall_models/location_only_tva.rda")
#get_variables(location_tva_model)[1:10]
params <- c("sd_iso_country_code__Intercept",
            "sd_iso_country_code:gdlcode__Intercept",
            "sd_iso_country_code:gdlcode:village__Intercept",
            "sigma")
draws <- as_draws_df(location_tva_model)[params]

vpcs <- vpc(location_tva_model,params)

vpcs %>% 
  gather() %>%
  ggplot(aes(y = key, x = value)) +
  stat_halfeye(aes(fill = after_stat(level))) +
  scale_fill_brewer(na.translate = FALSE) +
  labs(y="", x="",title = "")
  # scale_y_discrete(name=xlab,
  #                  breaks=params,
  #                  labels=readable_params)


# Full TVA -------------------------------------------------------
full_tva_model <- loadRData("./outputs/14_04_2023/outputs/overall_models/horseshoe_tva.rda")
plot(full_tva_model)
#get_variables(location_tva_model)[1:10]


params <- grep("^b_",get_variables(full_tva_model), value = T)
params <- params[params!="b_Intercept"]
draws <- as_draws_df(full_tva_model)[params]
draws %>% 
  gather() %>%
  ggplot(aes(y = key, x = value)) +
  stat_halfeye(aes(fill = after_stat(level))) +
  scale_fill_brewer(na.translate = FALSE) +
  labs(y="", x="",title = "")


params <- c("sd_iso_country_code__Intercept",
            "sd_iso_country_code:gdlcode__Intercept",
            "sd_iso_country_code:gdlcode:village__Intercept",
            "sigma")
vpcs <- vpc(full_tva_model,params)

vpcs %>% 
  gather() %>%
  ggplot(aes(y = key, x = value)) +
  stat_halfeye(aes(fill = after_stat(level))) +
  scale_fill_brewer(na.translate = FALSE) +
  labs(y="", x="",title = "")
# scale_y_discrete(name=xlab,
#                  breaks=params,
#                  labels=readable_params)





