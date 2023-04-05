# Geo Packages
library(sf)
library(rmapshaper)
library(sp)
library(stars)
library(geojsonsf)
# library(corrplot)
library(raster)
# library(leaflet)
# library(mapview)
# library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
# # Graph Plotting
library(ggplot2)
library(ggpubr)
# library(ggridges)
library(ggExtra)
library(RColorBrewer)
# library(GGally)

library(readr)
library(tidyr)
library(dplyr)
library(flextable)

library(factoextra)
library(dbscan)
library(fpc)
# Loading Data ------------------------------------------------------------


indicator_data <- readr::read_csv("./data/02-prepared-data/rhomis-spatial-merged.csv")
indicator_data <- indicator_data[!is.na(indicator_data$x_gps_latitude) & !is.na(indicator_data$x_gps_longitude),]
indicator_data <- indicator_data[!is.na(indicator_data$village),]
indicator_data <- indicator_data[!is.na(indicator_data$iso_country_code),]
indicator_data <- indicator_data[indicator_data$iso_country_code!="EC",]

# indicator_data <- indicator_data[!is.na(indicator_data$hfias_status),]

indicator_data_geo <- st_as_sf(indicator_data, coords = c( "x_gps_longitude","x_gps_latitude"), 
                               crs = 4326, agr = "constant", remove = F)

country_conversions <- readr::read_csv("./data/01-raw-data/external-data/country_conversions.csv")
# FAO administrative data

# 
# fao_level_2 <- geojson_sf('data/raw-data/earth-engine/fao-gaul-level-2.geojson')
# fao_level_2 <- sf::st_as_sf(x = fao_level_2, wkt = "geometry")
# fao_level_2_geo <-st_set_crs(fao_level_2,'EPSG:4326')
# # fao_level_2_geo <- st_simplify(fao_level_2_geo, 10000)
# fao_level_2 <- tibble::as_tibble(fao_level_2)

gdl_data <- sf::read_sf("./data/01-raw-data/external-data/global-data-lab/GDL Shapefiles V6.1/")
gdl_data <- st_set_crs(gdl_data,'EPSG:4326')
# gdl_data <- gdl_data[gdl_data$continent=="Africa",]

gdl_data <- sf::st_simplify(gdl_data,dTolerance = 1000) 

world <- ne_countries(scale = "medium", returnclass = "sf")
world$highlight <- world$wb_a2 %in% indicator_data$iso_country_code


# Clustering GPS points ---------------------------------------------------

clusters <- dbscan::dbscan(indicator_data[c( "x_gps_longitude","x_gps_latitude")],eps = 4)
indicator_data$cluster <-clusters$cluster


per_group_summary <- indicator_data %>% group_by(cluster) %>% 
  summarise(
    gps_lat=mean(x_gps_latitude),
    gps_lon= mean(x_gps_longitude),
    number_of_hhs=n())



# Continent Plot ----------------------------------------------------------


ggplot(data = world[world$continent=="Africa",]) +
  geom_sf(aes(fill=highlight), color="black")+
  scale_fill_manual(values = c("white", "dodgerblue4"))+
  theme_void() + theme(legend.position="none")




# Country Plot ------------------------------------------------------------


# st_simplify(gdl_data, preserveTopology = FALSE, dTolerance = 1000)


world_plot <- ggplot() +
  # geom_sf(data = gdl_data,lwd=0.25, size=1,alpha=0,color="gray36")+
  geom_sf(data = world,lwd=0.4, alpha=0, color="gray36")+
  geom_point(data=per_group_summary,aes(x=gps_lon, y=gps_lat,size=number_of_hhs), pch=21,colour="black",fill="dodgerblue4")+
  
  # geom_sf(data=st_jitter(indicator_data_geo),size=0.1)+
  ylim(-40,40)+
  xlim(-85,110)+
  labs(title="RHoMIS Surveys 2015-2022", caption="Points with no GPS coordinates excluded")+
  scale_size(name = "Number of Surveys", range = c(0,10),breaks=c(100,500,1000,2000,4000),
             labels=c(100,500,1000,2000,4000))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 20),
        plot.caption = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text  = element_text(size = 15),
        
        panel.grid.major = element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        panel.background = element_rect(fill = "white"))+
  guides(size = guide_legend(override.aes = list(fill = "dodgerblue4", color = "black", stroke = .25), 
                             label.position = "right",
                             title.position = "top", 
                             order = 1))
# theme_void() 
world_plot

dir.create("./outputs/02-data-exploration")
dir.create("./outputs/02-data-exploration/data-coverage")

ggsave(filename = "./outputs/02-data-exploration/data-coverage/global_coverage.png",plot = world_plot, height=4000, width=4000, units = "px")



# Village Plot ------------------------------------------------------------

village_points <- indicator_data_geo %>% 
  group_by(village,iso_country_code,id_form) %>% 
  summarise(x_gps_latitude=mean(x_gps_latitude, na.rm=T),
            x_gps_longitude=mean(x_gps_longitude, na.rm=T),
            number_of_households=n())

village_points <- st_as_sf(village_points, coords = c( "x_gps_longitude","x_gps_latitude"), 
                           crs = 4326, agr = "constant", remove = F)


countries <- unique(indicator_data$iso_country_code)


dir.create("./outputs/02-data-exploration/data-coverage/per_country")

plots <- list()
i <- 1
for (country in countries){
  three_letter_code <- country_conversions$`alpha-3`[country_conversions$`alpha-2`==country & !is.na(country_conversions$`alpha-2`)]
  
  nproj <- length(unique(village_points[village_points$iso_country_code==country,"id_form"]))
  country_plot <- ggplot() +
    geom_sf(data = gdl_data[gdl_data$iso_code==three_letter_code,],lwd=0.25, size=1,alpha=0,color="gray36")+
    # geom_sf(data = world,lwd=0.4, alpha=0, color="gray36")+
    # geom_sf(data=indicator_data_geo[indicator_data_geo$iso_country_code==country,], pch=21,colour="black",fill="dodgerblue4")+
    geom_point(data=village_points[village_points$iso_country_code==country,], aes(y=x_gps_latitude,x=x_gps_longitude,size=number_of_households, shape=id_form),color="black")+
    
    scale_size_continuous(range = c(0,2),breaks = c(0,10,20,50,100,200), labels=c(0,10,20,50,100,200))+
    guides(size = guide_legend(override.aes = list(color = "black", stroke = .25),
                               label.position = "right",
                               title.position = "top",
                               order = 1))+
    theme_bw()+
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust=0.5),
          
          panel.grid = element_blank())+
    labs(title=paste0("RHoMIS Surveys ",country))
  
  
  ggsave(paste0("./outputs/02-data-exploration/data-coverage/per_country/",country,".png"),country_plot,height=2000, width=2000, units = "px")
  
  # GG arrange plot
  arrange_plot <- ggplot() +
    geom_sf(data = gdl_data[gdl_data$iso_code==three_letter_code,],lwd=0.25, size=1,alpha=0,color="gray36")+
     geom_point(data=village_points[village_points$iso_country_code==country,], aes(y=x_gps_latitude,x=x_gps_longitude),color="black", size=0.5)+

    theme_bw()+
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust=0.5),
          axis.ticks = element_blank(),
          panel.grid = element_blank())+
    labs(title=paste0(country))
  plots[[i]] <- arrange_plot
  i <- i + 1
  
  
}


plots_arranged  <- ggpubr::ggarrange(plotlist=plots,common.legend = T,legend="bottom")
plots_arranged <- annotate_figure(plots_arranged, top = text_grob("RHoMIS Surveyed Villages by Country", 
                                     face = "bold", size = 20))

# ggexport(filename = "./outputs/02-data-exploration/data-coverage/data_coverage_per_country.png",plots_arranged)
ggsave(filename = "./outputs/02-data-exploration/data-coverage/data_coverage_per_country.png",plots_arranged,width = 2000,height=2000,units="px")

# ggplot() +
#   geom_sf(data = fao_level_2_geo[fao_level_2_geo$ADM0_NAME=="Rwanda",])+
#   geom_sf(data=village_points)+
#   theme_void() + theme(legend.position="none")
# 
# 
# ggplot() +
#   geom_sf(data = gdl_data[gdl_data$iso_code=="RWA",])+
#   geom_sf(data=st_jitter(indicator_data_geo[indicator_data_geo$iso_country_code=="RW",], factor=0.01))+
#   theme_void() + theme(legend.position="none")
# 
# 
# 


# Adding Region Counts ----------------------------------------------------


dir.create("./outputs/02-data-exploration/data-coverage/geographical_counts",showWarnings = F)


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# Area Counts ------------------------------------------------------------
#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
per_country_summary <- indicator_data %>% 
  group_by(iso_country_code) %>% summarise(
    projects = n_distinct(id_form),
    subnational_areas = n_distinct(gdlcode),
    villages = n_distinct(village),
    households = n()
  )  

new_row <- colSums(per_country_summary[c("projects","subnational_areas","villages","households")]) %>% as.list()
new_row$iso_country_code <- "Total"
new_row <- tibble::as_tibble(new_row)
per_country_summary <- per_country_summary %>% bind_rows(new_row)

readr::write_csv(per_country_summary,"./outputs/02-data-exploration/data-coverage/geographical_counts/per_country_summary.csv")


per_country_summary <- per_country_summary %>% merge(country_conversions, by.x="iso_country_code", by.y="alpha-2", all.x=T, all.y=F)
per_country_summary$Region <- per_country_summary$`sub-region`
per_country_summary$Region[!is.na(per_country_summary$`intermediate-region`)] <- per_country_summary$`intermediate-region`[!is.na(per_country_summary$`intermediate-region`)]
per_country_summary$Region[per_country_summary$iso_country_code=="Total"] <- ""
per_country_summary$name[per_country_summary$iso_country_code=="Total"] <- "Total"

per_country_summary$Region <- factor(per_country_summary$Region, levels=c(
  "Western Africa",
  "Eastern Africa",
  "Northern Africa",
  "Middle Africa",
  "Southern Africa",
  
  "South-eastern Asia",
  "Southern Asia",
  "South America",
  ""
  
), ordered = T)

per_country_summary <- per_country_summary[order(per_country_summary$Region),]

 




std_border <- flextable::fp_border_default(color="grey")


per_country_summary <- per_country_summary %>% 
  select(Region,
         name,
         projects,
         subnational_areas,
         villages,
         households) %>% 
  rename("Projects"='projects') %>% 
  rename("Country"="name") %>% 
  rename("Subnational Areas"="subnational_areas") %>% 
  rename("Villages"="villages") %>% 
  rename("Households"="households") %>% 
  
  as_grouped_data("Region") 





per_country_ft <- per_country_summary %>% 
  as_flextable(hide_grouplabel=T) %>% 
  bold( bold = TRUE, part="header") %>% 
  # align(i = ~ !is.na(Region), align = "center") %>% 

  bold(i = ~ !is.na(Region)) %>%  
  bold(i = ~ Country=="Total") %>% 
  hline(i = ~ !is.na(Region), border = std_border)%>%  

  autofit()


save_as_image(per_country_ft, "./outputs/02-data-exploration/data-coverage/geographical_counts/per_country_summary.png")


# Areas_per_country -------------------------------------------------------
per_country_summary_plot_df <- per_country_summary[per_country_summary$iso_country_code!="Total",]
per_country_summary_plot_df <- pivot_longer(per_country_summary, cols = c("subnational_areas",
                                                                          "villages",
                                                                          "households"))
temp <- ggplot(per_country_summary[per_country_summary$iso_country_code!="Total",],
               aes(x=iso_country_code,y=subnational_areas))+
  geom_bar(stat = "identity", fill="dodgerblue4", colour="black")+
  labs(title="Total Number of Subnational Areas in per Country",x="Country", y="Subnational Areas")
ggsave("./outputs/02-data-exploration/data-coverage/geographical_countssubnation_areas_per_country.png",temp)


temp <- ggplot(per_country_summary[per_country_summary$iso_country_code!="Total",],
               aes(x=iso_country_code,y=villages))+
  geom_bar(stat = "identity", fill="dodgerblue4", colour="black")+
  labs(title="Total Number of Villages  per Country",x="Country", y="Number of Villages")
ggsave("./outputs/02-data-exploration/data-coverage/geographical_counts/villages_per_country.png",temp)

temp <- ggplot(per_country_summary[per_country_summary$iso_country_code!="Total",],
               aes(x=iso_country_code,y=households))+
  geom_bar(stat = "identity", fill="dodgerblue4", colour="black")+
  labs(title="Total Number of Surveys per Country",x="Country", y="Number of Surveys")
ggsave("./outputs/02-data-exploration/data-coverage/geographical_counts/surveys_per_country.png",temp)


# Villages_per_area -------------------------------------------------------
villages_per_area <- indicator_data %>% 
  group_by(iso_country_code,gdlcode) %>% summarise(count = n_distinct(village)) 

temp <- ggplot(villages_per_area,
               aes(x=iso_country_code,y=count))+
  geom_boxplot(fill="dodgerblue4", colour="black", outlier.shape = NA)+
  scale_y_continuous(limits = quantile(villages_per_area$count, c(0, 0.99)))+
  labs(title="Villages Per Subnational Area",x="Country", y="Number of Villages in Subnational Area")
ggsave("./outputs/02-data-exploration/data-coverage/geographical_counts/villages_per_subnational_region.png",temp)

#-------------------------------------------------------------------------
indicator_data$village[indicator_data$iso_country_code=="NG"]
#-------------------------------------------------------------------------
# People Counts ------------------------------------------------------------

villages_per_area <- indicator_data %>% 
  group_by(iso_country_code,gdlcode) %>% summarise(count = n_distinct(village)) 

ggplot(villages_per_area,
       aes(x=iso_country_code,y=count))+
  geom_boxplot(fill="dodgerblue4", colour="black", outlier.shape = NA)+
  scale_y_continuous(limits = quantile(villages_per_area$count, c(0, 0.99)))+
  labs(title="Villages Per Subnational Area",x="Country", y="Number of Villages in Subnational Area")
ggsave("./outputs/02-data-exploration/data-coverage/geographical_counts/villages_per_subnational_region.png",temp)

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# people_per_village ------------------------------------------------------


# people_per_county -------------------------------------------------------













