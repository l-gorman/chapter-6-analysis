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

projects_to_include <- c(
  "BF_CW2_2015",
  "ML_CW1_2015",
  "TZ_CFM_2015",
  "BF_SIL_2016",
  "ET_SRL_2016",
  "KE_SRL_2016",
  "TZ_SRL_2016",
  "KE_CM1_2016",
  "KE_CM2_2016",
  "CD_CLP_2017",
  "KE_VCD_2017",
  "ET_TA1_2017",
  "ML_TA3_2017",
  # "UG_CWU_2017",
  
  "CD_FRT_2017",
  "KE_SCN_2017",
  "ZM_SCN_2017",
  # "CR_IND_2017",
  
  "BF_GLD_2018",
  "ML_GLD_2018",
  "NE_GLD_2018",
  "BF_TA4_2018",
  "GH_TA7_2018",
  
  "CI_ARC_2018",
  "GH_ARC_2018",
  # "KH_SIL_2018",
  # "EC_CIP_2018",
  # "SL_IFD_2018",
  # "PS_FAO_2018",
  # "SN_FTF_2018",
  "BI_CLP_2018",
  "ZM_GIZ_2018",
  # "PE_MKP_2018",
  "TZ_CRA_2018",
  # "BO_AID_2018",
  "KE_GLT_2019",
  "NG_GLT_2019",
  "ET_GTC_2019",
  "KE_GTC_2019",
  # "IN_GEF_2018",
  "BF_ADN_2019",
  "BI_SNV_2019",
  "KE_LGS_2019",
  "MA_GDI_2019",
  "CD_LGS_2019",
  "ET_LGS_2020"
  
)

projects_with_control_groups <- c(
  "GH_TA2_2017",
  # "IN_BIO_2018",
  "RW_OAF_2018",
  "ET_ARI_2018",
  # "PS_FAO_2018",
  "ET_TA9_2019", # VTE group membership
  "GH_T10_2019", # VTE group membership
  "NE_TA8_2019",
  "GH_ADN_2019",
  "NE_T11_2019",# VTE group membership
  "MW_FAW_2019",
  "ZM_FAW_2019",
  # "KM_DHA_2019",
  "ET_CAF_2020"
  # "VN_CSI_2020",
  # "KH_CSI_2020"
)

# Remove KM, Peru, SL, SN, UG

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
