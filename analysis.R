###############################################################################################################################################################
### prep data for analysis// unscaled and scaled regression models ###
###############################################################################################################################################################

source("packages_functions.R")



###############################################################################################################################################################
### prep data for analysis ### 
###############################################################################################################################################################

### read data ###
ambient_climate_seasonal <- read.csv("ambient_climate_seasonal.csv")
phylo_div <- read.csv("phylo_div.csv")
soil_temp_seasonal <- read.csv("soil_temp_seasonal.csv")
water_seasonal <- read.csv("water_seasonal.csv")
site <- read.csv("site.csv")

### prepare data ###
soil_temp_seasonal <- prep_data(soil_temp_seasonal)
water_seasonal <- prep_data(water_seasonal)



###############################################################################################################################################################
### regression models ### 
###############################################################################################################################################################

### run models ###
mod_min_temp <- lmer(min_daily_soil_temp_c~elev_m+min_daily_ambient_temp_c+I(pd/100)+(1|site_name),soil_temp_seasonal)
mod_mean_temp <- lmer(mean_daily_soil_temp_c~elev_m+mean_daily_ambient_temp_c+I(pd/100)+(1|site_name),soil_temp_seasonal)
mod_max_temp <- lmer(max_daily_soil_temp_c~elev_m+max_daily_ambient_temp_c+I(pd/100)+(1|site_name),soil_temp_seasonal)
mod_range_temp <- lmer(range_daily_soil_temp_c~elev_m+range_daily_ambient_temp_c+I(pd/100)+(1|site_name),soil_temp_seasonal)
mod_capture_water <- lmer(mean_water_capture_kg~elev_m+mean_daily_ambient_temp_c+I(pd/100)+(1|site_name),water_seasonal)
mod_loss_water <- lmer(mean_water_loss_kg~elev_m+mean_daily_ambient_temp_c+I(pd/100)+(1|site_name),water_seasonal)



###############################################################################################################################################################
### contrast plots ###
###############################################################################################################################################################

p7 <- visreg(mod_min_temp,type="contrast",plot=F)
p8 <- visreg(mod_mean_temp,type="contrast",plot=F)
p9 <- visreg(mod_max_temp,type="contrast",plot=F)
p10 <- visreg(mod_range_temp,type="contrast",plot=F)
p11 <- visreg(mod_capture_water,type="contrast",plot=F)
p12 <- visreg(mod_loss_water,type="contrast",plot=F)

pdf(file = "contrast_plot.pdf",width=6,height=8)
par(mfrow=c(3,2))
plot(p7[[3]],xlab="",ylab = expression(Min*" "*Substrate*" "*Temp*" "*(C * degree)) )
plot(p8[[3]],xlab="",ylab = expression(Mean*" "*Substrate*" "*Temp*" "*(C * degree)) )
plot(p9[[3]],xlab="",ylab = expression(Max*" "*Substrate*" "*Temp*" "*(C * degree)) )
plot(p10[[3]],xlab="",ylab = expression(Substrate*" "*Diurnal*" "*Temp*" "*Range*" "*(C * degree)) )
plot(p11[[3]],xlab="Faith's Phylogenetic Diversity",ylab = "Water Capture (%)" )
plot(p12[[3]],xlab="Faith's Phylogenetic Diversity",ylab = "Water Loss (%)" )
dev.off()



###############################################################################################################################################################
### supplemental analysis  ###
###############################################################################################################################################################

lfilter <- list(All=c("A","B","C","D","E","F"),
                noAB=c("C","D","E","F"),
                noCF=c("A","B","D","E"),
                noDE=c("A","B","C","F"))

supplemental_analysis <- lapply(lfilter, function (x) {
  list(
    min=lmer(min_daily_soil_temp_c~elev_m+min_daily_ambient_temp_c+I(pd/100)+(1|site_name),soil_temp_seasonal[soil_temp_seasonal$mix%in%x,]),
    mean=lmer(mean_daily_soil_temp_c~elev_m+mean_daily_ambient_temp_c+I(pd/100)+(1|site_name),soil_temp_seasonal[soil_temp_seasonal$mix%in%x,]),
    max=lmer(max_daily_soil_temp_c~elev_m+max_daily_ambient_temp_c+I(pd/100)+(1|site_name),soil_temp_seasonal[soil_temp_seasonal$mix%in%x,]),
    dtr=lmer(range_daily_soil_temp_c~elev_m+range_daily_ambient_temp_c+I(pd/100)+(1|site_name),soil_temp_seasonal[soil_temp_seasonal$mix%in%x,]),
    capture=lmer(mean_water_capture_kg~elev_m+mean_daily_ambient_temp_c+I(pd/100)+(1|site_name),water_seasonal[water_seasonal$mix%in%x,]),
    loss=lmer(mean_water_loss_kg~elev_m+mean_daily_ambient_temp_c+I(pd/100)+(1|site_name),water_seasonal[water_seasonal$mix%in%x,])
  )
})
