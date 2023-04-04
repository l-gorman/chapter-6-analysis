# Modelling Multi-Level Smallholder Heterogeneity

A significant body of research tries to model smallholder resource endowment,
productivity, or welfare in relation to farm characteristics, demographics,
local context. Many studies which do this are small in scale, and it is 
often difficult to build up a picture of the "power" of different drivers
in different locations. This is made even more difficult by imbalance in 
geographical coverage and noise associated with complex indicators.

In this analysis, I try to develop/present a procedure that can allow
researchers to combine multiple observational studies to better model
drivers of farmer heterogeneity. This procedure includes

Code for data preperation:

- Linking household survey data to administrative data
- Linking survey data to raster data

## Data preperation

Data comes from multiple sources. First we have
household survey data which comes from the Rural 
Household Multi-Indicator Survey:

* The [Rural Household Multi Indicator Survey](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TFXQJN):
* [Google Earth Engine](https://code.earthengine.google.com/)
* The FAO's [GAEZ](https://gaez.fao.org/pages/data-viewer) Portal

 

# Configuration


In addition, create a file `~/.R/Makevars` with the following contents:

See [here](https://github.com/stan-dev/rstan/issues/892)
for more details
