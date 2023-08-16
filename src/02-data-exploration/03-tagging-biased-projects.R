library(readr)
library(dplyr)
library(tidyr)
library(tibble)


rhomis_data <- readr::read_csv("data/raw-data/rhomis/processed_data.csv", na=c("-999","NA", "n/a"))
indicator_data <- readr::read_csv("data/raw-data/rhomis/indicator_data.csv", na=c("-999","NA", "n/a"))
indicator_data$beneficiary <- rhomis_data$beneficiary

indicator_data <- indicator_data %>% merge(rhomis_data[c("id_unique","x_gps_latitude", "x_gps_longitude", "village")],by="id_unique")

# indicator_data <- indicator_data %>% merge(rhomis_data[c("id_unique","x_gps_latitude", "x_gps_longitude")],by="id_unique")

indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- st_as_sf(indicator_data, coords = c("x_gps_longitude", "x_gps_latitude"), 
                           crs = 4326, agr = "constant", remove = F)






# Filtering ---------------------------------------------------------------

projects_to_exclude <- c(
  "GT_S4N_2015",
  "HN_S4N_2015",
  "SV_S4N_2015",
  "TZ_GLV_2017",
  "BF_CIR_2018",
  "BF_TA5_2018",
  "ET_TA6_2018",
  "NI_CFN_2018",
  "EC_AID_2019",
  "ET_SRL_2019",
  "KE_SRL_2019",
  "TZ_SRL_2019",
  "VN_NT1_2019",
  "MA_CRD_2019",
  "NG_IIT_2020",
  "ML_MB2_2020",
  "UG_CRP_2020",
  "KE_STP_2020",
  "ZA_NEO_2020",
  "VN_NT3_2020",
  "UG_NUT_2020",
  "VN_CRP_2020",
  "BF_UPS_2020",
  "ET_PCS_2020",
  "MA_CRD_2020",
  "UG_PCS_2020",
  "ET_ARI_2022",
  "GT_CAT_2022",
  "BF_EC3_2022",
  "ML_MC3_2022",
  "NE_NA6_2022",
  "BI_PRD_2022",
  "BI_SNV_2022",
  "ET_ECO_2023",
  "KM_DHA_2021",
  "PH_USM_2022",
  "VN_NT2_2019",
  "ZM_FA2_2019"
)


projects_with_control_groups <- c(
  "MW_CFG_2015",
  "IN_CM3_2016",
  "GH_TA2_2017",
  "IN_BIO_2018",
  "ET_ARI_2018",
  "RW_OAF_2018",
  "ET_TA9_2019",
  "GH_ADN_2019",
  "NE_T11_2019",
  "ET_CAF_2020"
)


# Removing Datasets in bias
# crit_1 <- (rhomis_data$id_form %in% tolower(projects_to_include) | rhomis_data$id_form %in% tolower(projects_with_control_groups) )
# table(crit_1)
# crit_2 <- !is.na(rhomis_data$x_gps_latitude)&
#   !is.na(rhomis_data$x_gps_longitude) &
#   !is.na(rhomis_data$village)
# table(crit_1&crit_2)
# 
# crit_3 <-  (is.na(rhomis_data$beneficiary) | rhomis_data$beneficiary %in% c("control","n","core","no_participation","non_beneficiary"))
# table(crit_1&crit_2&crit_3)
# 
#  crit_4 <- !is.na(rhomis_data$landcultivated) 
#  table(crit_1&crit_2&crit_3&crit_4)

crit_1 <- (joined_df_rhomis$id_form %in% tolower(projects_to_include) | joined_df_rhomis$id_form %in% tolower(projects_with_control_groups) )
table(crit_1)
crit_2 <- !is.na(joined_df_rhomis$x_gps_latitude)&
  !is.na(joined_df_rhomis$x_gps_longitude) &
  !is.na(joined_df_rhomis$village)
table(crit_1&crit_2)

crit_3 <-  (is.na(joined_df_rhomis$beneficiary) | joined_df_rhomis$beneficiary %in% c("control","n","core","no_participation","non_beneficiary"))
table(crit_1&crit_2&crit_3)

crit_4 <- !is.na(joined_df_rhomis$landcultivated) 
table(crit_1&crit_2&crit_3&crit_4)



subset <- crit_1&crit_2&crit_3

joined_df_rhomis <- joined_df_rhomis[subset,]



