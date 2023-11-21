library(readxl)
library(tibble)
original_coefficients <- readxl::read_xlsx("./coefficients.xlsx",sheet = "Original")


transform_log_log <- function(fraction_inc,coeff){
  
  if (is.na(coeff)){
    return(NA)
  }
  
  res <- (((1+fraction_inc)^coeff)-1)*100
  
  return(round(res,2))
}


transform_straight_coeff <- function(coeff){
  if (is.na(coeff)){
    return(NA)
  }
  
  res <- (exp(coeff)-1)*100
  
  return(round(res,2))
}

transform_prop <- function(x1,x2,coeff){
  if (is.na(coeff)){
    return(NA)
  }
  
  num <- exp(coeff*log(x2/(1-x2)))
  denom <- exp(coeff*log(x1/(1-x1)))
  
  
  y2_div_y1 <- 100*((num/denom) -1)
  
  return(round(y2_div_y1,2))
  
}




# TVA ---------------------------------------------------------------------

outcomes <- list()
  
for (i in 1:nrow(original_coefficients)){
  temp_list <- list()
  var <- as.character(original_coefficients[i,"Variable"])
  temp_list[["variable"]] <- var
  temp_list[["Outcome"]] <-  as.character(original_coefficients[i,"Outcome"])
  
  temp_list[["Estimate"]] <-  as.numeric(original_coefficients[i,"Estimate"])
  temp_list[["L95% CI"]] <-  as.numeric(original_coefficients[i,"L95% CI"])
  temp_list[["U95% CI"]] <-  as.numeric(original_coefficients[i,"U95% CI"])
  
  if (original_coefficients[i,"var_type"]=="binary"){
    temp_list[["Transformed Estimate"]] <- transform_straight_coeff(as.numeric(original_coefficients[i,"Estimate"]))
    temp_list[["Transformed L95% CI"]] <-  transform_straight_coeff(as.numeric(original_coefficients[i,"L95% CI"]))
    temp_list[["Transformed U95% CI"]] <-  transform_straight_coeff(as.numeric(original_coefficients[i,"U95% CI"]))
     

  }
  
  if (original_coefficients[i,"var_type"]=="continuous"){
    temp_list[["Transformed Estimate"]] <- transform_log_log(1,as.numeric(original_coefficients[i,"Estimate"]))
    temp_list[["Transformed L95% CI"]] <-  transform_log_log(1,as.numeric(original_coefficients[i,"L95% CI"]))
    temp_list[["Transformed U95% CI"]] <-  transform_log_log(1,as.numeric(original_coefficients[i,"U95% CI"]))
    
    
  }
  
  if (original_coefficients[i,"var_type"]=="proportion"){

    temp_list[["Transformed Estimate"]] <- transform_prop(0.4,0.6,as.numeric(original_coefficients[i,"Estimate"]))
    temp_list[["Transformed L95% CI"]] <-  transform_prop(0.4,0.6,as.numeric(original_coefficients[i,"L95% CI"]))
    temp_list[["Transformed U95% CI"]] <-  transform_prop(0.4,0.6,as.numeric(original_coefficients[i,"U95% CI"]))
    
    
  }
  
  outcomes[[i]] <- temp_list
  
}


result <- dplyr::bind_rows(outcomes)

readr::write_csv(result,"./coefficients_transformed.csv")


result_to_plot_tva <- result[result$Outcome=="TVA",]
result_to_plot_tva <- result_to_plot_tva[complete.cases(result_to_plot_tva),]
result_to_plot_tva <- result_to_plot_tva[result_to_plot_tva$variable!="Market Orientation",]
colnames(result_to_plot_tva) <- gsub("Transformed Estimate","Transformed_Estimate",colnames(result_to_plot_tva))
colnames(result_to_plot_tva) <- gsub("Transformed L95% CI","L95",colnames(result_to_plot_tva))
colnames(result_to_plot_tva) <- gsub("Transformed U95% CI","U95",colnames(result_to_plot_tva))
result_to_plot_tva$level <- "95% CI"

ggplot(result_to_plot_tva, aes(y = variable,x=Transformed_Estimate,shape="Estimate"))+
  geom_point(show.legend = T,size=3)+
  geom_segment(aes(y=variable,yend=variable,x=L95,xend=U95,linewidth=level))+
  scale_discrete_manual("linewidth", values = c("95% CI"=0.75))+
  labs(x="Effect (% difference)", y="", title="Transformed Regression Coefficients
       for Random Intercept TVA Model
       After Variable Selection")+
  xlim(-40,50)+
  geom_vline(xintercept = 0)+
  guides(linewidth = guide_legend(title="",
                                  nrow = 2, 
                                  byrow = TRUE, 
                                  override.aes = list(shape = c(NA), linetype = c("solid"))),
         shape=guide_legend(title="")) +
  theme(plot.title = element_text(hjust=0.5))


result_to_plot_hdds <- result[result$Outcome=="HDDS",]
result_to_plot_hdds <- result_to_plot_hdds[complete.cases(result_to_plot_hdds),]
result_to_plot_hdds <- result_to_plot_hdds[result_to_plot_hdds$variable!="Market Orientation",]
colnames(result_to_plot_hdds) <- gsub("Transformed Estimate","Transformed_Estimate",colnames(result_to_plot_hdds))
colnames(result_to_plot_hdds) <- gsub("Transformed L95% CI","L95",colnames(result_to_plot_hdds))
colnames(result_to_plot_hdds) <- gsub("Transformed U95% CI","U95",colnames(result_to_plot_hdds))
result_to_plot_hdds$level <- "95% CI"

ggplot(result_to_plot_hdds, aes(y = variable,x=Transformed_Estimate,shape="Estimate"))+
  geom_point(show.legend = T,size=3)+
  geom_segment(aes(y=variable,yend=variable,x=L95,xend=U95,linewidth=level))+
  scale_discrete_manual("linewidth", values = c("95% CI"=0.75))+
  labs(x="Effect (% difference)", y="", title="Transformed Regression Coefficients
       for Random Intercept HDDS Model
       After Variable Selection")+
  xlim(-40,50)+
  geom_vline(xintercept = 0)+
  
  guides(linewidth = guide_legend(title="",
                                  nrow = 2, 
                                  byrow = TRUE, 
                                  override.aes = list(shape = c(NA), linetype = c("solid"))),
         shape=guide_legend(title="")) +
  theme(plot.title = element_text(hjust=0.5))

  
  
  
  
  
