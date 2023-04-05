library(magrittr)
library(readr)
library(tibble)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(ggExtra)
library(cowplot)
rhomis_data <- readr::read_csv("./data/02-prepared-data/rhomis-spatial-merged.csv")



rhomis_data$eggs_harvest_1

# biased_projects 

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


columns <- list("id_unique"="character",
                "id_hh"="character",
                "id_rhomis_dataset"="character",
                # "id_form"="character",
                "id_proj"="character",
                "gps_lat"="numeric",
                "gps_lon"="numeric",
                "iso_country_code"="character",
                "year"="numeric",
                "currency_conversion_lcu_to_ppp"="numeric",
                "currency_conversion_factor_year"="numeric",
                "survey_length_minutes"="numeric",
                "land_cultivated_ha"="numeric",
                "land_owned_ha"="numeric",
                "livestock_tlu"="numeric",
                "hh_size_members"="numeric",
                "hh_size_mae"="numeric",
                "household_type"="character",
                "head_education_level"="character",
                "worst_food_security_month"="character",
                "best_food_security_month"="character",
                "nr_months_food_shortage"="numeric",
                "fies_score"="numeric",
                "hfias_status"="character",
            
                "hdds_good_season"="numeric",
                "hdds_good_season_bought"="numeric",
                "hdds_good_season_farm"="numeric",
                "hdds_bad_season"="numeric",
                "hdds_bad_season_bought"="numeric",
                "hdds_bad_season_farm"="numeric",
                "hdds_last_month"="numeric",
                "hdds_last_month_bought"="numeric",
                "hdds_last_month_farm"="numeric",
                "hdds_last_24hr"="numeric",
                "crop_income_lcu_per_year"="numeric",
                "livestock_income_lcu_per_year"="numeric",
                "total_income_lcu_per_year"="numeric",
                "off_farm_income_lcu_per_year"="numeric",
                "value_crop_consumed_lcu_per_hh_per_year"="numeric",
                "value_livestock_products_consumed_lcu_per_hh_per_year"="numeric",
                "value_farm_products_consumed_lcu_per_hh_per_year"="numeric",
                "crop_consumed_calories_kcal_per_hh_per_year"="numeric",
                "livestock_consumed_calories_kcal_per_hh_per_year"="numeric",
                "farm_products_consumed_calories_kcal_per_hh_per_year"="numeric",
                "proportion_of_value_controlled_female_youth"="numeric",
                "proportion_of_value_controlled_female_adult"="numeric",
                "proportion_of_value_controlled_male_youth"="numeric",
                "proportion_of_value_controlled_male_adult"="numeric"
                
)


result <- sapply(colnames(rhomis_data), function(column){
  
  if(!is.null(columns[[column]]))
  {
    num_char <- columns[[column]]
    
    
    if (num_char=="character"){
      rhomis_data[[column]] <- as.character(rhomis_data[[column]])
      result <- rhomis_data[c("id_form", column)] %>% 
        group_by(id_form) %>% 
        summarise(
          prop_nas= sum(is.na(.data[[column]]), na.rm=T)/length(.data[[column]])
        )
      
      colnames(result) <- paste0(column,"_", colnames(result))
      colnames(result)[1]<- "id_form"
      
      return(result)
    }
    
    if (num_char=="numeric"){
      rhomis_data[[column]] <- as.numeric(rhomis_data[[column]])
      
      result <- rhomis_data[c("id_form", column)] %>% 
        group_by(id_form) %>% 
        summarise(q5= quantile(.data[[column]],probs=0.05, na.rm=T),
                  median=median(.data[[column]], na.rm=T),
                  mean=mean(.data[[column]], na.rm=T),
                  q95= quantile(.data[[column]],probs=0.95, na.rm=T),
                  prop_nas= sum(is.na(.data[[column]]), na.rm=T)/length(.data[[column]]),
                  prop_zero= sum(.data[[column]]==0, na.rm=T)/length(.data[[column]]),
                  prop_na_or_zero= sum(is.na(.data[[column]]) | .data[[column]]==0, na.rm=T)/length(.data[[column]])
        )
      
      colnames(result) <- paste0(column,"_", colnames(result))
      colnames(result)[1]<- "id_form"
      
      return(result)
    }
  }
  
  
})


result <- result %>% bind_cols()
colnames(result) <- gsub("id_form...1","id_form",colnames(result))
result <- result[!grepl("id_form...",colnames(result))]
result <- result[!grepl("id_form[[:digit:]]",colnames(result))]


number_of_hhs <- rhomis_data %>% 
  group_by(id_form) %>% summarise(number_of_surveys = n())

final_result <- merge(number_of_hhs, result, by="id_form")
final_result <- final_result[order(final_result$number_of_surveys,decreasing = T),]



dir.create("outputs")
dir.create("outputs/01-data-quality-check")

readr::write_csv(final_result, "outputs/01-data-quality-check/indicator_quality_check_all.csv")




prop_na_cols <- grep("prop_nas",colnames(final_result),value=T)
plotting_df <- final_result[c("id_form","number_of_surveys",prop_na_cols)] %>% gather(key = "variable", value = "prop_na",-id_form, -number_of_surveys)
plotting_df$variable  <- gsub("_prop_nas","",plotting_df$variable)
plotting_df$id_form <- factor(plotting_df$id_form, levels = sort(unique(plotting_df$id_form)), ordered = T)
plotting_df$variable <- factor(plotting_df$variable, levels =rev(names(columns)), ordered = T)

plotting_df$numeric_id <- as.numeric(plotting_df$id_form)



projects <- unique(plotting_df$id_form)[as.character(unique(plotting_df$id_form)) %in% tolower(c(projects_to_include,projects_with_control_groups))==F]
height <- rep(length(unique(plotting_df$variable))+1,length(projects))


rectangles <- as_tibble(list(
  projects = projects,
  height = height
))

rectangles <- rectangles %>% merge(plotting_df[duplicated(plotting_df$id_form)==F,],
                                              by.x="projects", 
                                              by.y="id_form",
                                              all.x=T,
                                              all.y=F)
rectangles$xmin <- rectangles$numeric_id-0.5
rectangles$xmax <- rectangles$numeric_id+0.5
rectangles$ymin <- 0


plot <- ggplot() +
  # Initial Point plot

  geom_point(data = plotting_df, mapping = aes(y=variable, x=id_form,  size = prop_na),shape = 21, stroke = 0, fill="black") +
  
  # Grid lines
  geom_hline(yintercept = seq(.5, length(unique(plotting_df$variable))-0.5, 1), size = .2)+
  geom_vline(xintercept = seq(.5, length(unique(plotting_df$id_form))-0.5, 1), size = .2)+
  
  # Highlighting projects
  # geom_vline(xintercept = seq(4,5,0.01), size = 1, color="green",alpha=0.5)+
  
  theme_minimal()+
  
  # Setting the range of point sizes
  scale_radius(name = "Proportion of NAs",range = c(0, 3))+
  # Putting axis ticks at bottom
  scale_x_discrete(position = "bottom") +
  labs(title = "Missing Data in the Rural Household Multi-Indicator Survey (RHoMIS)", x="Form ID", y="Variable")+
  # Setting Scale Colours for 
  # scale_fill_gradient(low = "green", high = "red", breaks = c(0, .5, 1), labels = c("Low\nProp", "Med\nProp", "High\nProp"), limits = c(0, 1)) +
  theme(axis.text.x = element_text(hjust=1,angle=90,vjust=0.5),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        panel.grid.major = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.background = element_rect(fill="azure3",color="black")) +
  guides(size = guide_legend(override.aes = list(fill = "black", color = "black", stroke = .25), 
                             label.position = "bottom",
                             title.position = "right", 
                             order = 1)) +
 geom_rect(data = rectangles,mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=height),show.legend = F, alpha=0.4, fill="firebrick4") 
  
  

xhist <- 
  axis_canvas(plot, axis = "x") + 
  geom_bar(data = plotting_df[duplicated(plotting_df$id_form)==F,], aes(x=numeric_id,y=number_of_surveys), fill="dodgerblue4",size = 0,stat = "identity")+



plot <- plot %>%
  insert_xaxis_grob(xhist, grid::unit(1,"in"), position = "top") %>%
  ggdraw()

plot

ggsave("outputs/01-data-quality-check/missing_indicators.png",plot,width = 5000,height=3000,units = "px",limitsize = FALSE)


 

# guides(size = guide_legend(override.aes = list(fill = NA, color = "black", stroke = .25), 
#                            label.position = "bottom",
#                            title.position = "right", 
#                            order = 1),
#        fill = guide_colorbar(ticks.colour = NA, title.position = "top", order = 2))

