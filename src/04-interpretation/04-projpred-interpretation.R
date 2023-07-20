library(projpred)
library(brms)
library(ggplot2)
library(tibble)



loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

dir.create("./outputs/overall_model_results/variable_addition/projpred")
dir.create("./outputs/overall_model_results/variable_addition/projpred/tva/")
dir.create("./outputs/overall_model_results/variable_addition/projpred/hdds/")



# TVA Summary -------------------------------------------------------------


cvvs_tva <- loadRData("outputs/31_05_2023/outputs/overall_models/variable_addition/proj_pred/tva/weak_prior_fixed/projpred_cv_varsel_model_1.rda")

ranking_res <- ranking(cvvs_tva)
save(ranking_res,file="./outputs/overall_model_results/variable_addition/projpred/tva/ranking.rda")

mlpd_plot <- plot(cvvs_tva,stats = "mlpd", ranking_nterms_max = NA)
ggsave("./outputs/overall_model_results/variable_addition/projpred/tva/mlpg_plot.png",mlpd_plot)
# plot(cvvs,stats = "elpd", ranking_nterms_max = NA)

rk <- ranking(cvvs_tva)
ranking_full <-as_tibble(list("ranking"=rk$fulldata))
readr::write_csv(ranking_full,"./outputs/overall_model_results/variable_addition/projpred/tva/rankings.csv")

prop_plotplot(cv_proportions(rk, cumulate = TRUE))
ggsave("./outputs/overall_model_results/variable_addition/projpred/tva/mlpg_plot.png",mlpd_plot)

cvvs_tva <- NULL

# HDDS Summary -------------------------------------------------------------


cvvs_hdds <- loadRData("outputs/31_05_2023/outputs/overall_models/variable_addition/proj_pred/hdds/weak_prior_fixed/projpred_cv_varsel_model_2.rda")

ranking_res <- ranking(cvvs_hdds)
save(ranking_res,file="./outputs/overall_model_results/variable_addition/projpred/hdds/ranking.rda")

mlpd_plot <- plot(cvvs_hdds,stats = "mlpd", ranking_nterms_max = NA)
ggsave("./outputs/overall_model_results/variable_addition/projpred/hdds/mlpg_plot.png",mlpd_plot)
# plot(cvvs,stats = "elpd", ranking_nterms_max = NA)

rk <- ranking(cvvs_hdds)
ranking_full <-as_tibble(list("ranking"=rk$fulldata))
readr::write_csv(ranking_full,"./outputs/overall_model_results/variable_addition/projpred/hdds/rankings")

prop_plotplot(cv_proportions(rk, cumulate = TRUE))
ggsave("./outputs/overall_model_results/variable_addition/projpred/hdds/mlpg_plot.png",mlpd_plot)

cvvs_hdds <- NULL





