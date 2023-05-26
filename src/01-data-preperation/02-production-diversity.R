#' This script is designed to calculate 
#' two main indicators. Firstly, an index
#' to measure diversity of production
#' at the group level. Secondly, an indicator
#' to measure the uniqueness or conformity
#' of indivitual Households
#' 
#' This is to answer the following questions:
#' 
#' 1. If you live in an area where everyone is
#' producing the same thing, does this have a 
#' negative impact on your food security or income
#' (suggesting gluts)
#' 
#' 2. If you are a household living in a particular
#' area, and you specialise in something not common
#' to your area, does that give you an advantage in
#' terms of income.
#' 
#' 
#' 
#' ------------------------------------------
#' Grouped Production Diversity:
#' 
#' An index to see how much commonality
#' there is in production at the group
#' level.
#' 
#' For example, if all households in the area
#' are relying on one single crop, we want 
#' a grouped production diversity score of
#' zero. 
#' 
#' If every household relies on a different
#' product as a major source of their income,
#' then we have a production diversity score of 1
#' ------------------------------------------
#' Uniqueness of Production:
#' 
#' This is a measure at the individual household
#' level to assess whether a household is producing
#' and selling
#' 

library(rhomis)
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(flextable)
library(reshape2)
gini_calculation <- function(x){

    if(all(is.na(x))){
    return(NA)
  }
  
  x <- x[!is.na(x)]
#   x <- x[x!=0]
  if(length(x)==0){
    return(NA)
    
  }
  
  
  sorted_values <- sort(x)
  n <- length(x)
  
  numerator <- 2*sum(sorted_values*c(1:n))
  denominator <- n*sum(sorted_values)
  
  constant <- (n+1)/n
  
  
  gini_coefficient <- (numerator/denominator)-constant
return(gini_coefficient)

}

diversity_index <- function(x) {
  gini_coefficient <- gini_calculation(x)

  reverse_gini <- 1-gini_coefficient
  
  

  weighted_diversity_index <- reverse_gini*n
  return(weighted_diversity_index)
}




diversity <- function(indicator_data){
  crop_price_and_value <- map_to_wide_format(indicator_data,
                                             name_column = "crop_name",
                                             column_prefixes = c("crop_harvest_kg_per_year","mean_crop_price_lcu_per_kg"),
                                             types=c("num","num"))
  # plain_crop_diversity <- rowSums(!is.na(crop_price_and_value[[1]]))
  
  
  crop_value <- crop_price_and_value[["crop_harvest_kg_per_year"]]*crop_price_and_value[["mean_crop_price_lcu_per_kg"]]
  
  livestock <- map_to_wide_format(indicator_data,
                                  name_column = "livestock_name",
                                  column_prefixes = c(
                                    "livestock_sale_income",
                                    
                                    "meat_kg_per_year",
                                    "mean_meat_price_per_kg",
                                    
                                    "milk_collected_litres_per_year",
                                    "milk_price_per_litre",
                                    
                                    "eggs_collected_kg_per_year",
                                    "eggs_price_per_kg",
                                    
                                    "bees_honey_kg_per_year",
                                    "bees_honey_price_per_kg"),
                                  types=c("num","num","num","num","num","num","num","num","num"))
  # plain_crop_diversity <- rowSums(!is.na(crop_price_and_value[[1]]))
  
  livestock_value <- livestock[["livestock_sale_income"]]
  meat_value <- livestock[["meat_kg_per_year"]]*livestock[["mean_meat_price_per_kg"]]
  milk_value <- livestock[["milk_collected_litres_per_year"]]*livestock[["milk_price_per_litre"]]
  eggs_value <- livestock[["eggs_collected_kg_per_year"]]*livestock[["eggs_price_per_kg"]]
  honey_value <- livestock[["bees_honey_kg_per_year"]]*livestock[["bees_honey_price_per_kg"]]
  
  
  off_farm_income <- map_to_wide_format(indicator_data,
                                        name_column = "offfarm_income_name",
                                        column_prefixes = c("offfarm_income_name"),
                                        types=c("chr"))[["offfarm_income_name"]]
  off_farm_income[!is.na(off_farm_income)] <- "1"
  off_farm_income <- off_farm_income %>% mutate_all(as.numeric) 
  off_farm_income <- off_farm_income/rowSums(off_farm_income,na.rm=T)
  off_farm_value <- off_farm_income*indicator_data$off_farm_income_lcu_per_year
  
  
  value_matrix <- bind_cols(crop_value,
                            livestock_value,
                            meat_value,
                            milk_value,
                            eggs_value,
                            honey_value,
                            off_farm_value)
  
  indicator_data$weighted_income_diversity <- apply(value_matrix,1,diversity_index)
  
  return( indicator_data)
}

dataset <- tibble::tribble(
    ~grp_level_1, ~grp_level_2, ~product_1, ~product_2, ~product_3,
    
    #' grp_level_1: no diversity
    #' individual level: no innovators
    "grp_1", "grp_1_subgrp_1", 100,0,0,
    "grp_1", "grp_1_subgrp_1", 40,10,0,
    "grp_1", "grp_1_subgrp_2", NA,NA,NA,
    "grp_1", "grp_1_subgrp_2", 20,0,NA,

     #' grp_level_1: no diversity
    #' individual level: no innovators
    "grp_2", "grp_2_subgrp_1", 50,50,50,
    "grp_2", "grp_2_subgrp_1", 100,100,100,
    "grp_2", "grp_2_subgrp_2", NA,NA,NA,
    "grp_2", "grp_2_subgrp_2", 10,10,10,

     #' grp_level_1: full diversity
    #' individual level: no innovators
    "grp_3", "grp_3_subgrp_1", 50,0,0,
    "grp_3", "grp_3_subgrp_1", 0,100,0,
    "grp_3", "grp_3_subgrp_2", NA,NA,NA,
    "grp_3", "grp_3_subgrp_2", 0,0,10
)

# Group Production Diversity

exclusion_columns <- c("grp_level_1", "grp_level_2")
columns_to_summarise <- c("product_1","product_2", "product_3")
grouping_column <- "grp_level_1"
new_column_name <- "grp_level_1_prd_diversity"

dataset[dataset$grp_level_1=="grp_1",]




pairwise_dist <- function(x){
    

    temp <- outer(x,x,  `-`) %>%melt()

    weights <- lapply(1:nrow(temp), function(i){

        index_1 <- temp[i,"Var1"]
        index_2 <- temp[i,"Var2"]

        max(c(x[index_1],x[index_2])) 
    })

    temp$weights <- unlist(weights)
    temp <- temp[temp$Var1!=temp$Var2,]

    temp <- apply(temp,1,FUN=function(x){

        val_1 <- min(c(x["Var1"],x["Var2"]))
        val_2 <- max(c(x["Var1"],x["Var2"]))

       sorted_row=list(
        Var1=val_1,
        Var2=val_2,
        value=x["value"],
        weights=x["weights"]

       )

      return(sorted_row)
    })%>%
    # unlist()%>%
    bind_rows()
    temp$value <- abs(temp$value)

    temp <- temp[!duplicated(temp),]

    

    value <- weighted.mean(temp$value,temp$weights, na.rm=T)

    zeroes <- (temp$value==0 | is.na(temp$value)) & (temp$weights==0 | is.na(temp$weights))

    if(all(zeroes)){
        value <- 0   
    }
    return(value)
}



group_production_diversity <- function(dataset,
                               columns_to_summarise,
                               grouping_column,
                               new_column_name){
    new_df <- dataset


    totals <- rowSums(new_df[columns_to_summarise],na.rm=T) 
    na.rows <- rowSums(is.na(new_df[columns_to_summarise]))==ncol(new_df[columns_to_summarise])
    totals[na.rows] <- NA 

    new_df[columns_to_summarise] <- new_df[columns_to_summarise]/totals

    # dist(new_df[columns_to_summarise])
    # abs(as.matrix(new_df[columns_to_summarise]))



    village_diversity <- new_df %>% 
    group_by_at(grouping_column)%>% 
    # summarise_at(columns_to_summarise, mean, na.rm=T)
    # summarise_at(columns_to_summarise, gini_calculation)
    summarise_at(columns_to_summarise, pairwise_dist)

    village_diversity_summary <- tibble::as_tibble(list(
        group=village_diversity[[grouping_column]],
        diversity_summary=rowSums(village_diversity[columns_to_summarise])
    ))

    colnames(village_diversity_summary) <- c(grouping_column,diversity_summary)

    if ("prod_diversity_temp_index" %in% colnames(dataset)){
        stop("Will be an issue with merging as prod diversity index col already existent")
    }

    dataset$prod_diversity_temp_index <- c(1:nrow(dataset))
    dataset <- dataset %>% merge(village_diversity_summary,by=grouping_column)
    dataset <- dataset[order(dataset$prod_diversity_temp_index),]   
    dataset$prod_diversity_temp_index <- NULL

    # village_diversity <- apply(village_diversity[columns_to_summarise],1,diversity_index)
    return(dataset[[diversity_summary]])
    

}


uniqueness_score <- function(x){


    temp <- outer(x,x,  `-`) %>%melt()

    
    temp <- temp[temp$Var1!=temp$Var2,]
    temp$value[temp$value<0] <- 0

    uniqueness <- temp %>% 
    group_by(Var1) %>%
    summarise(
        uniqueness=mean(value, na.rm=T)
    )
    
    uniqueness <- uniqueness[order(uniqueness[["Var1"]]),]

    uniqueness$uniqueness[is.nan(uniqueness$uniqueness)] <- NA


    if (any(!is.na(uniqueness$uniqueness))){
        uniqueness$uniqueness[is.na(uniqueness$uniqueness)] <- 0
    }

    return(uniqueness$uniqueness)


}



individual_uniqueness_score <- function(dataset,
                               columns_to_summarise,
                               grouping_column,
                               new_column_name,threshold=1){


    new_df <- dataset


    totals <- rowSums(new_df[columns_to_summarise],na.rm=T) 
    na.rows <- rowSums(is.na(new_df[columns_to_summarise]))==ncol(new_df[columns_to_summarise])
    totals[na.rows] <- NA 

    new_df[columns_to_summarise] <- new_df[columns_to_summarise]/totals
    NaNs <- lapply(new_df, function(x){
        is.nan(x)
    }) %>% bind_cols()

        NaNs <- as.matrix(NaNs)

    new_df[NaNs] <- 0


    new_df <- new_df %>% 
    group_by_at(grouping_column) %>% 
    summarise_at(columns_to_summarise, uniqueness_score)

    na.rows <- rowSums(is.na(new_df[columns_to_summarise]))==ncol(new_df[columns_to_summarise])
    uniqueness <- rowSums(new_df[columns_to_summarise]>=threshold, na.rm=T)
    uniqueness[na.rows] <- NA
    return(uniqueness)










}


village_and_individual_diversity <- function(indicator_data, grouping_column){

    crop_incomes <- map_to_wide_format(indicator_data,
                                             name_column = "crop_name",
                                             column_prefixes = c("crop_income_per_year"),
                                             types=c("num","num"))[[1]]
  # plain_crop_diversity <- rowSums(!is.na(crop_price_and_value[[1]]))
  
    
  livestock <- map_to_wide_format(indicator_data,
                                  name_column = "livestock_name",
                                  column_prefixes = c(
                                    "livestock_sale_income",
                                    
                                    "meat_sold_income",
                                    
                                    "milk_sold_income_per_year",
                                    
                                    "eggs_sold_income",
                                    
                                    "bees_honey_sold_income"),
                                  types=c("num","num","num","num","num"))
  # plain_crop_diversity <- rowSums(!is.na(crop_price_and_value[[1]]))
  
  livestock_income <-livestock[["livestock_sale_income"]]
  colnames(livestock_income) <- paste0("whole_sale_",colnames(livestock_income))
  

  meat_income <-livestock[["meat_sold_income"]]
  colnames(meat_income) <- paste0("meat_income_",colnames(meat_income))

  milk_income <-livestock[["milk_sold_income_per_year"]]
  colnames(milk_income) <- paste0("milk_income_",colnames(milk_income))

  eggs_income <-livestock[["eggs_sold_income"]]
  colnames(eggs_income) <- paste0("eggs_income_",colnames(eggs_income))

  bees_income <-livestock[["bees_honey_sold_income"]]
  colnames(bees_income) <- paste0("bees_",colnames(bees_income))


#   livestock_value <- livestock[["livestock_sale_income"]]
#   meat_value <- livestock[["meat_kg_per_year"]]*livestock[["mean_meat_price_per_kg"]]
#   milk_value <- livestock[["milk_collected_litres_per_year"]]*livestock[["milk_price_per_litre"]]
#   eggs_value <- livestock[["eggs_collected_kg_per_year"]]*livestock[["eggs_price_per_kg"]]
#   honey_value <- livestock[["bees_honey_kg_per_year"]]*livestock[["bees_honey_price_per_kg"]]
  
  
#   off_farm_income <- map_to_wide_format(indicator_data,
#                                         name_column = "offfarm_income_name",
#                                         column_prefixes = c("offfarm_income_name"),
#                                         types=c("chr"))[["offfarm_income_name"]]
#   off_farm_income[!is.na(off_farm_income)] <- "1"
#   off_farm_income <- off_farm_income %>% mutate_all(as.numeric) 
#   off_farm_income <- off_farm_income/rowSums(off_farm_income,na.rm=T)
#   off_farm_value <- off_farm_income*indicator_data$off_farm_income_lcu_per_year
  
  
  value_matrix <- bind_cols(crop_incomes,
                            livestock_income,
                            meat_income,
                            milk_income,
                            eggs_income,
                            bees_income
                            )

   na_cols <- lapply(value_matrix, function(x){
    any(!is.na(x))
  }) %>% unlist()

  value_matrix <- value_matrix[na_cols]

  value_matrix$grouping_temp <- indicator_data[[grouping_column]]
  colnames(value_matrix) <- gsub("grouping_temp", grouping_column,colnames(value_matrix))

   temp_value_matrix <- value_matrix[c(1:10, 50:60),]


    start_time <- Sys.time()
   individual_uniqueness_score(dataset=temp_value_matrix,
                            columns_to_summarise=colnames(value_matrix)[colnames(value_matrix)!=grouping_column],
                            grouping_column=grouping_column,
                            threshold=1)
end_time <- Sys.time()

end_time - start_time

}





indicator_data <- readr::read_csv("./data/02-prepared-data/rhomis-spatial-merged.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$village),]
indicator_data <- indicator_data[!is.na(indicator_data$iso_country_code),]

grouping_column <- "village"
