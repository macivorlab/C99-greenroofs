###############################################################################################################################################################
### load packages // user-defined functions ###
###############################################################################################################################################################



###############################################################################################################################################################
### load packages ###
###############################################################################################################################################################

### check if package is installed ###
if("stringr"%in%installed.packages()[,1]==F){install.packages("stringr",repos="https://cloud.r-project.org/",dependencies = T)}
if("lme4"%in%installed.packages()[,1]==F){install.packages("lme4",repos="https://cloud.r-project.org/",dependencies = T)}
if("lmerTest"%in%installed.packages()[,1]==F){install.packages("lmerTest",repos="https://cloud.r-project.org/",dependencies = T)}
if("visreg"%in%installed.packages()[,1]==F){install.packages("visreg",repos="https://cloud.r-project.org/",dependencies = T)}

### load packages ###
library(stringr)
library(lme4)
library(lmerTest)
library(visreg)



###############################################################################################################################################################
### user-defined functions ###
###############################################################################################################################################################

### prepare data for analysis ###
prep_data <- function(data) {
  
  # get site and mix ids
  data$site_id <- str_extract(data$module_id,".")
  data$mix <- str_extract(data$module_id,"(?<=[A-Z]).")
  
  # merge with covariate information (phylodiversity, site info, ambient climate info)
  data <- merge(data,phylo_div,by="mix")
  data <- merge(data,site,by="site_id")
  data <- merge(data,ambient_climate_seasonal,by="site_id")
  
  data
  
}