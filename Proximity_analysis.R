#set up working space and load in required data using function_library
setwd("~/MSc project/data")
source("function_library.R")
key <- setup_wrkspc()
HD_home <- prepare_HES("DISTANCE_171814_08.csv")
LSOA_loc <- create_LSOAloc("LSOA_loc.csv")
GP_services <- create_GP("GP_services.csv")
Hosp_services <- create_hosp("Hosp_services.csv")
LS <- calc_dist(FALSE)
HEScombs <- comb_data(FALSE)
HES_comb <- as.data.frame(HEScombs[1])
HES_comb_w <- as.data.frame(HEScombs[2])

##Prduction of exploratory visualisations

#difference between HES calculation of distance to A&E and our calculation. Comparing in meters.
SITEDIST_diff <- HES_comb$SITEDIST - HES_comb$act_dist_hosp
Diff_hosp_dist <- ggplot(HES_comb, aes(x=SITEDIST, y=act_dist_hosp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="HES distance calculation in km", y="My distance calulation in km")
ggsave("Diff_act_HES_dist.png")

#relationship between distance to GP and hospital
Dist_GP_vs_hosp <- ggplot(HES_comb_w, aes(x=dist_hosp , y=dist_gp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Distance from nearest hospital in kilometers", y="Distance from nearest GP in kilometers")
ggsave("Dist_GP_vs_Hosp.png")
cor(HES_comb$dist_hosp, HES_comb$dist_gp)

#relationship between distance to hospital attended and closest hospital
Act_hosp_vs_close <- ggplot(HES_comb_w, aes(x=dist_hosp , y=act_dist_hosp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Distance from nearest hospital in kilometers", y="Distance from attended hospital in kilometers")
ggsave("Act_hosp_vs_close.png")
cor(HES_comb$dist_hosp, HES_comb$act_dist_hosp)

#Number of sites visited by people in an LSOA
LSOA_SITES <- HES_comb %>% group_by(LSOA11, UNNECESSARY) %>% summarize(distinct_sites = n_distinct(SITETRET))
no_hosps <- ggplot(LSOA_SITES, aes(y=distinct_sites)) + geom_boxplot(fill="steelblue") + theme_classic() + labs(y="Number of different sites attended by patients within an LSOA")
ggsave("Distinct_hosps.png")
No_hosps_un <- ggplot(LSOA_SITES, aes(x=UNNECESSARY, y=distinct_sites)) + geom_boxplot(fill="steelblue") + theme_classic() + labs(x="Attendance classification", y="Number of different sites attended by patients within an LSOA")
ggsave("Distinct_hosps_un.png")
LSOA_SITEST <- table(LSOA_SITES$distinct_sites)
plot(LSOA_SITEST, xlab="The number of different hospital sites attended by patients in the LSOA", 
     ylab="Number of LSOAs", main="The number of different hospital sites \npatients from the same LSOA visit.", col="steelblue3")
dev.copy(png,"Number of different sites per LSOAs.png")
dev.off()  

#Look at the range of distance travelled from one LSOA to tbe different hospital sites attended
LSOA_SITES <- HES_comb %>% group_by(LSOA11, UNNECESSARY, dist_hosp, dist_gp) %>% summarize(IQR_distance = IQR(act_dist_hosp))
LSOA_SITES_w <- HES_comb_w %>% group_by(LSOA11, Necessary, Unnecessary, Attendances, dist_hosp, dist_gp) %>% summarize(IQR_distance = IQR(act_dist_hosp))
LSOA_dist_IQR <- ggplot(LSOA_SITES, aes(y=IQR_distance)) + geom_boxplot(fill="steelblue") + theme_classic() + labs(y="IQR of distances travelled to attend site from each LSOA")
LSOA_dist_IQR <- ggplot(LSOA_SITES, aes(x=LSOA11, y=IQR_distance)) + geom_point(fill="steelblue") + theme_classic() + labs(y="IQR of distances travelled to attend site from each LSOA")

##relationship between distance to hospital attended and closest GP
Act_hosp_vs_GP <- ggplot(HES_comb_w, aes(x=dist_gp , y=act_dist_hosp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Distance from nearest GP in kilometers", y="Distance from attended hospital in kilometers")
ggsave("Act_hosp_vs_GP.png")

#Explore the range of unecessary attendances.
#wont be standardising since as LSOAs all cover same pop size basic standardisation has been done.
no_un <- boxplot(HES_comb_w[c("Unnecessary")], horizontal = TRUE, xlab="Number unnecessary attendances")
prop_un <- boxplot(HES_comb_w[c("prop_unnec")], horizontal = TRUE, xlab="Proportion of unnecessary attendances")

#Explore the range of distance travelled.
act_dist <- boxplot(HES_comb_w[c("act_dist_hosp")], horizontal = TRUE, xlab="Distance travelled to hospital")
close_hosp_dist <- boxplot(HES_comb_w[c("dist_hosp")], horizontal = TRUE, xlab="Distance to nearest hospital")
close_gp_dist <- boxplot(HES_comb_w[c("dist_gp")], horizontal = TRUE, xlab="Distance to nearest GP")

#boxplot of distance by classification of attendance
act_dist_un <- ggplot(HES_comb, aes(x=UNNECESSARY, y=act_dist_hosp)) + geom_boxplot(fill="steelblue") + labs(x="Classification of attendances", y="Distance to the hospital attended")
ggsave("box_act_dist_un.png")
near_dist_un <- ggplot(HES_comb, aes(x=UNNECESSARY, y=dist_hosp)) + geom_boxplot(fill="steelblue") + labs(x="Classification of attendances", y="Distance to the nearest hospital")
ggsave("box_near_dist_un.png")
near_dist_un <- ggplot(HES_comb, aes(x=UNNECESSARY, y=dist_gp)) + geom_boxplot(fill="steelblue") + labs(x="Classification of attendances", y="Distance to the nearest GP")
ggsave("box_near_dist_gp_un.png")

#explore relationship between distance and proportion of unnecessary attendances
#distance to attended hospital
act_dist_prop <- ggplot(HES_comb_w, aes(x=prop_unnec , y=act_dist_hosp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Proportion of attendances \nclassified as unnecessary", y="Distance to the hospital attended")
ggsave("scatter_act_dist_prop.png")
cor(HES_comb_w$prop_unnec, HES_comb_w$act_dist_hosp)
#distance to closest hospital
near_dist_prop <- ggplot(HES_comb_w, aes(x=prop_unnec , y=dist_hosp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Proportion of attendances \nclassified as unnecessary", y="Distance to the nearest hospital")
ggsave("scatter_near_dist_prop.png")
cor(HES_comb_w$prop_unnec, HES_comb_w$dist_hosp)
#distance to closest GP
near_dist_GP_prop <- ggplot(HES_comb_w, aes(x=prop_unnec , y=dist_gp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Proportion of attendances \nclassified as unnecessary", y="Distance to the nearest GP")
ggsave("scatter_near_dist _GP_prop.png")
cor(HES_comb_w$prop_unnec, HES_comb_w$dist_gp)

#chart to compare how far people travel split out by the unnecessary classifier.
Un_dist_comp <- ggplot(HES_comb, aes(SITEDIST)) + geom_histogram(breaks=bins, fill="steelblue") + facet_wrap(~UNNECESSARY) + theme_classic()
ggsave("hist_comp_unnecessary.png")
#chart to compare how far people travel split out by whether the GP is closer than the hospital.
GP_dist_comp <- ggplot(HES_comb, aes(SITEDIST)) + geom_histogram(breaks=bins, fill="steelblue") + facet_wrap(~GPcloser) + theme_classic()
ggsave("hist_comp_GPcloser.png")
#chart to compare the number split out by whether the GP is closer than the hospital.
GP_dist_comp <- ggplot(HES_comb, aes(SITEDIST)) + geom_histogram(breaks=bins, fill="steelblue") + facet_wrap(~GPcloser) + theme_classic()
ggsave("hist_comp_GPcloser.png")

#looking at unecessary attendances at a regional level based on LSOA location.
LSOA_loc$LSOA_LA <- gsub(' \\S*$', '', LSOA_loc$LSOA.name)
HES_comb$LSOA_LA <- LSOA_loc$LSOA_LA[match(HES_comb$LSOA11, LSOA_loc$LSOA.code)]
HC_region <- HES_comb %>% group_by(LSOA_LA, UNNECESSARY) %>% dplyr::summarise(Attendances=sum(Attendances), act_dist_hosp= median(act_dist_hosp), dist_hosp=median(dist_hosp), dist_gp = median(dist_gp))
dist_un_reg <- ggplot(HC_region, aes(x=UNNECESSARY, y=act_dist_hosp)) + geom_boxplot()
ggsave("dist_un_reg.png")

HES_comb_w$LSOA_LA <- LSOA_loc$LSOA_LA[match(HES_comb_w$LSOA11, LSOA_loc$LSOA.code)]
HC_region_w <- HES_comb_w %>% group_by(LSOA_LA) %>% dplyr::summarise(Unnecessary= sum(Unnecessary), Necessary=sum(Necessary), Attendances=sum(Attendances), act_dist_hosp= median(act_dist_hosp), dist_hosp=median(dist_hosp), dist_gp = median(dist_gp))
HC_region_w$prop_unnec <- HC_region_w$Unnecessary/HC_region_w$Attendances

no_un_reg <- boxplot(HC_region_w[c("Unnecessary")], horizontal = TRUE, xlab="Number unnecessary attendances")
prop_un_reg <- boxplot(HC_region_w[c("prop_unnec")], horizontal = TRUE, xlab="Proportion of unnecessary attendances")
act_dist_reg <- boxplot(HC_region_w[c("act_dist_hosp")], horizontal = TRUE, xlab="Distance travelled to hospital")
close_hosp_dist_reg <- boxplot(HC_region_w[c("dist_hosp")], horizontal = TRUE, xlab="Distance to nearest hospital")
close_gp_dist_reg <- boxplot(HC_region_w[c("dist_gp")], horizontal = TRUE, xlab="Distance to nearest GP")

#subsetting the data into distance buckets, keeping LSOA, hosp, nearestGP
Comb_buk <- HES_comb_w %>% group_by(DISTbucket, LSOA11, near_GPID, GPcloser, near_hospID, nearest_hosp_att) %>% dplyr::summarize(Unnecessary=sum(Unnecessary), Necessary=sum(Necessary), Attendances=sum(Attendances))

#aggregate to buckets of distance travelled
DIST1_bin <- HES_comb %>% group_by(DISTbucket) %>% dplyr::summarize(Attendances=sum(Attendances), act_dist_hosp=sum(act_dist_hosp))
DIST1_bin_w <- HES_comb_w %>% group_by(DISTbucket) %>% dplyr::summarize(Attendances=sum(Attendances), Unnecessary=sum(Unnecessary), Necessary=sum(Necessary), 
                                                                        act_dist_hosp=sum(act_dist_hosp), dist_gp=sum(dist_gp), dist_hosp=sum(dist_hosp))
hist_dist1 <- ggplot(HES_comb) + geom_histogram(aes(x=SITEDIST, weight=Attendances), breaks=bins, fill="steelblue")+ theme_classic() + labs(x="Distance travelled in KM", y="Frequency")
ggsave("hist_dist.png")
Dist1_buckets <- plot_ly(DIST1_bin, x=~DISTbucket, y=~Attendances, type="bar") %>% layout(xaxis=list(title="Distance travelled in m"), yaxis=list(title="frequency"))
htmlwidgets::saveWidget(Dist1_buckets, "DIST_buckets.html")

Dist1_buckets <- plot_ly(HES_comb, x=~DISTbucket, y=~Attendances, type="bar") %>% layout(xaxis=list(title="Distance travelled in km"), yaxis=list(title="frequency"))

#perform regression taing only distance to A&E attended looking at proportion of unnecessary attendances.
reg_dist <- lm(prop_unnec~act_dist_hosp, data=HES_comb_w)
reg_dist_sum <- summary(reg_dist)

#consider the above regression as a classification problem.
reg_dist <- lm(UNNECESSARY~act_dist_hosp, data=HES_comb)
reg_dist_sum <- summary(reg_dist)

#addition of distance to nearest hospital
reg_dist1 <- lm(prop_unnec~act_dist_hosp + dist_hosp, data=HES_comb_w)
reg_dist1_sum <- summary(reg_dist1)

#comparison of the two regression models
anova(reg_dist, reg_dist1)

#creation of regression model considering all avaialble variables
reg_dist_ext <- lm(prop_unnec~act_dist_hosp+dist_gp+dist_hosp+GPcloser+nearest_hosp_att, data=HES_comb_w)
reg_dist_ext_sum <- summary(reg_dist_ext)

#comparison of base distance to hospital attended regression and full version
anova(reg_dist, reg_dist_ext)

#refinement of above model based on significance levels.
reg_dist2 <- lm(prop_unnec~act_dist_hosp + dist_hosp + nearest_hosp_att, data=HES_comb_w)
reg_dist2_sum <- summary(reg_dist2)



