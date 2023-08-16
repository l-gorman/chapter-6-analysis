library(tibble)
library(dplyr)

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}




quantile_resource_allociation <- function(means_vector, 
                                 amount_to_distribute){
  
  # N <- 2000
  # 
  # test_df <- tibble::as_tibble(list(
  #   y=rnorm(N,mean=10,sd=0.5)
  # ))
  # 
  # means_vector <- test_df$y
  # amount_to_distribute <- 10^6
  # 
  # 
  df <- tibble::as_tibble(list(
    index=c(1:length(means_vector)),
    y=means_vector
  ))
  
  df <- df %>% mutate(cume_dist=cume_dist(y))
  
  
  # df <- df %>% mutate(quantile=ntile(y,100)/100)
  df <- df %>% mutate(weight=(1-cume_dist))
  df <- df %>% mutate(weight_norm=weight/sum(weight))
  
  df$amount_allocated <- amount_to_distribute*df$weight_norm
  
  return(df$amount_allocated)
  
}

#log_tva <- log(tva)
# tva=exp(log_tva)
# y = (log_tva - mean(log_tva)) / sd(log_tva))
# y = (log(tva)- mean(log(tva))/sd(log(tva)))
# log(tva)= y*sd(log(tva))+mean(log(tva))
# tva = exp(y*sd(log(tva))+mean(log(tva)))

tva_original <- readr::read_csv("data/02-prepared-data/income_data_only.csv")
sd_log_tva <- sd(log(tva_original$tva_per_mae_per_day_ppp))
mean_log_tva <- mean(log(tva_original$tva_per_mae_per_day_ppp))


transform_prediction <- function(value, mean_log_tva,sd_log_tva){
  converted_value  <- exp(value*sd_log_tva+mean_log_tva)
  return(converted_value
         )
}

test_df <- readr::read_csv("data/02-prepared-data/test_df.csv")


tva_test_conv <- transform_prediction(test_df$tva,mean_log_tva,sd_log_tva)

test_df$allocation_tva <- quantile_resource_allociation(test_df$tva,
                                            amount_to_distribute = 10^6)
test_df$allocation_hdds <- quantile_resource_allociation(test_df$hdds,
                                                        amount_to_distribute = 10^6)
plot(test_df$tva,test_df$allocation_tva)



# Random Guess ------------------------------------------------------------

predictions_tva <- tibble::as_tibble(list(
  allocation_tva_pred=rep(10^6/nrow(test_df),nrow(test_df))))

predictions_tva_converted <- transform_prediction(predictions_tva$Estimate,mean_log_tva,sd_log_tva)

predictions_tva$allocation_tva_true <- test_df$allocation_tva
predictions_tva$misallocation_pred <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred

hist(predictions_tva$misallocation_pred)
sum(predictions_tva$misallocation_pred[predictions_tva$misallocation_pred>0])


# Location Only -----------------------------------------------------------


model <- loadRData("./outputs/31_05_2023/outputs/overall_models/location_only/tva/country_county_village.rda")

predictions_tva <- as_tibble(predict(model,newdata = test_df,allow_new_levels=T))
predictions_tva$allocation_tva_pred <- quantile_resource_allociation(predictions_tva$Estimate,
                                                         amount_to_distribute = 10^6)

predictions_tva$allocation_tva_pred_2.5 <- quantile_resource_allociation(predictions_tva$Q2.5,
                                                                     amount_to_distribute = 10^6)

predictions_tva$allocation_tva_pred_97.5 <- quantile_resource_allociation(predictions_tva$Q97.5,
                                                                         amount_to_distribute = 10^6)

predictions_tva$allocation_tva_true <- test_df$allocation_tva
plot(predictions_tva$allocation_tva_true,predictions_tva$allocation_tva_pred)

# explanation <- lm(allocation_tva_true~allocation_tva_pred, predictions_tva)

predictions_tva$misallocation_pred <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred
predictions_tva$misallocation_pred_2.5 <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred_2.5
predictions_tva$misallocation_pred_97.5 <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred_97.5

hist(predictions_tva$misallocation_pred)

# sum(misallocation[misallocation<0])
sum(predictions_tva$misallocation_pred[predictions_tva$misallocation_pred>0])
sum(predictions_tva$misallocation_pred_2.5[predictions_tva$misallocation_pred_2.5>0])
sum(predictions_tva$misallocation_pred_97.5[predictions_tva$misallocation_pred_97.5>0])

# 193638.7
#20% of the funding misallocated.


# Household Attributes Only -----------------------------------------------

model <- loadRData("./outputs/31_05_2023/outputs/overall_models/variable_addition/tva/weak_fixed_only.rda")

predictions_tva <- as_tibble(predict(model,newdata = test_df,allow_new_levels=T))
predictions_tva$allocation_tva_pred <- quantile_resource_allociation(predictions_tva$Estimate,
                                                                     amount_to_distribute = 10^6)

predictions_tva$allocation_tva_pred_2.5 <- quantile_resource_allociation(predictions_tva$Q2.5,
                                                                         amount_to_distribute = 10^6)

predictions_tva$allocation_tva_pred_97.5 <- quantile_resource_allociation(predictions_tva$Q97.5,
                                                                          amount_to_distribute = 10^6)

predictions_tva$allocation_tva_true <- test_df$allocation_tva
plot(predictions_tva$allocation_tva_true,predictions_tva$allocation_tva_pred)

# explanation <- lm(allocation_tva_true~allocation_tva_pred, predictions_tva)

predictions_tva$misallocation_pred <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred
predictions_tva$misallocation_pred_2.5 <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred_2.5
predictions_tva$misallocation_pred_97.5 <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred_97.5

hist(predictions_tva$misallocation_pred)

# sum(misallocation[misallocation<0])
sum(predictions_tva$misallocation_pred[predictions_tva$misallocation_pred>0])
sum(predictions_tva$misallocation_pred_2.5[predictions_tva$misallocation_pred_2.5>0])
sum(predictions_tva$misallocation_pred_97.5[predictions_tva$misallocation_pred_97.5>0])



# All Variables Added -----------------------------------------------------




model <- loadRData("./outputs/31_05_2023/outputs/overall_models/variable_addition/tva/weak_prior_fixed.rda")

predictions_tva <- as_tibble(predict(model,newdata = test_df,allow_new_levels=T))

predictions_tva$allocation_tva_pred <- quantile_resource_allociation(predictions_tva$Estimate,
                                                                     amount_to_distribute = 10^6)
predictions_tva$allocation_tva_pred_2.5 <- quantile_resource_allociation(predictions_tva$Q2.5,
                                                                         amount_to_distribute = 10^6)

predictions_tva$allocation_tva_pred_97.5 <- quantile_resource_allociation(predictions_tva$Q97.5,
                                                                          amount_to_distribute = 10^6)
predictions_tva$allocation_tva_true <- test_df$allocation_tva
plot(predictions_tva$allocation_tva_true,predictions_tva$allocation_tva_pred)

# explanation <- lm(allocation_tva_true~allocation_tva_pred, predictions_tva)

predictions_tva$misallocation_pred <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred
predictions_tva$misallocation_pred_2.5 <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred_2.5
predictions_tva$misallocation_pred_97.5 <- predictions_tva$allocation_tva_true-predictions_tva$allocation_tva_pred_97.5

hist(predictions_tva$misallocation_pred)

plot(predictions_tva$allocation_tva_true,predictions_tva$misallocation_pred)
# explanation <- lm(allocation_tva_true~misallocation_pred, predictions_tva)
# summary(explanation)
plot(explanation)
# This plot is showing that in some
# sum(misallocation[misallocation<0])
sum(predictions_tva$misallocation_pred[predictions_tva$misallocation_pred>0])
sum(predictions_tva$misallocation_pred_2.5[predictions_tva$misallocation_pred_2.5>0])
sum(predictions_tva$misallocation_pred_97.5[predictions_tva$misallocation_pred_97.5>0])



# 
# test_df$allocation <- quantile_resource_allociation(test_df$y,
#                                                     amount_to_distribute = 10^6)
# 
# plot(test_df$y,test_df$allocation)
# 
# hist(y)
# 
# 
# 
