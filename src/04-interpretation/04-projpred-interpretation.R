library(projpred)
library(brms)
library(ggplot2)




loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}


cvvs <- loadRData("outputs/31_05_2023/outputs/overall_models/variable_addition/proj_pred/tva/weak_prior_fixed/projpred_cv_varsel_model_1.rda")
ref_model <- loadRData("outputs/31_05_2023/outputs/overall_models/variable_addition/tva/weak_prior_fixed.rda")


# cvvs <- loadRData("outputs/31_05_2023/outputs/overall_models/variable_addition/proj_pred/hdds/weak_prior_fixed/projpred_cv_varsel_model_2.rda")
# ref_model <- loadRData("outputs/31_05_2023/outputs/overall_models/variable_addition/tva/weak_prior_fixed.rda")
# selection_summary <- loadRData("outputs/31_05_2023/outputs/overall_models/variable_addition/proj_pred/hdds/weak_prior_fixed/selection_summary_2.rda")

dir.create("./outputs/overall_model_results/variable_addition/projpred")
dir.create("./outputs/overall_model_results/variable_addition/projpred/tva/")
dir.create("./outputs/overall_model_results/variable_addition/projpred/hdds/")

ranking_res <- ranking(cvvs)
save(ranking_res,file="./outputs/overall_model_results/variable_addition/projpred/tva/ranking.rda")

plot(cvvs,stats = "mlpd", ranking_nterms_max = NA)

plot(cvvs,stats = "elpd", ranking_nterms_max = NA)



# projection <- project(cvvs, solution_terms = "(1|iso_country_code) (1|iso_country_code:village)+pesticide")


# Summary -----------------------------------------------------------------


smmry <- summary(cvvs, stats = "mlpd", type = c("mean", "lower", "upper"),
                 deltas = TRUE)

print(smmry, digits = 1)


# Plotting predictor rank -------------------------------------------------


predictor_plot <- plot(cvvs,stats="mlpd",deltas = TRUE,text_angle=90) 

predictor_plot

predictor_plot <- plot(cvvs)#,ranking_abbreviate = T)


#CV Proportions ----------------------------------------------------------

projpred::solution_terms(cvvs)
# projpred::break_up_matrix_term(cvvs)

# selection_summary <- unclass(summary(cvvs))[["selection"]]
# selection_summary$solution_terms <- gsub("(1 | iso_country_code) + (1 | iso_country_code:village)","location_grouping",selection_summary$solution_terms)
# ggplot(selection_summary)+
#   geom_point(aes(y=elpd.kfold, x=size))+
#   geom_hline(yintercept=0)
# 


# rk[["fulldata"]]

# pr_rk <- cv_proportions(rk)
# plot(pr_rk)
rk <- ranking(cvvs)
plot(cv_proportions(rk, cumulate = TRUE))

size_decided <- 12
predictors_final <- head(rk[["fulldata"]], size_decided)



# Projecting Final Model --------------------------------------------------

# Interpretation
# predictors_final <- c("(1 | iso_country_code)", "(1 | iso_country_code:village)", predictors_final)
# predictors_final <- predictors_final[predictors_final!="(1 | iso_country_code) + (1 | iso_country_code:village)"]
# prj <- project(ref_model, solution_terms = predictors_final)

