#Creating LSOA context data
LSOA_context <- as.data.frame(unique(HDGP_comb_g$LSOA11.x))
colnames(LSOA_context) <- "LSOA"

install.packages("readxl")
library(readxl)
setwd("~/MSc project/data/")

#age data
Age <- read_excel("./Additional_data/LSOA_age_gender.xlsx", sheet="Mid-2016-Persons", skip=4)
Gender <- read_excel("./Additional_data/LSOA_age_gender.xlsx", sheet="Mid-2016-Females", skip=4)
#create over 65 and over 85 cateogries and proportion of population
Age$over65 <- rowSums(Age[,c(70:95)], na.rm=TRUE)
Age$over85 <- rowSums(Age[,90:95], na.rm=TRUE)
Age$prop_over65 <- Age$over65/Age$`All Ages`
Age$prop_over85 <- Age$over85/Age$`All Ages`

#add new variables to LSOA data.
LSOA_context$Total_pop <- Age$`All Ages`[match(LSOA_context$LSOA, Age$`Area Codes`)]
LSOA_context$prop_over65 <- Age$prop_over65[match(LSOA_context$LSOA, Age$`Area Codes`)]
LSOA_context$prop_over85 <- Age$prop_over85[match(LSOA_context$LSOA, Age$`Area Codes`)]
LSOA_context$female_pop <- Gender$`All Ages`[match(LSOA_context$LSOA, Gender$`Area Codes`)]
LSOA_context$prop_female <- LSOA_context$female_pop/LSOA_context$Total_pop

#read in IMD data
IMD <- read.csv("./Additional_data/IMD_indices.csv", header=TRUE)
LSOA_context$IMD_rank <- IMD$Index.of.Multiple.Deprivation..IMD..Rank..where.1.is.most.deprived.[match(LSOA_context$LSOA, IMD$LSOA.code..2011.)]
LSOA_context$HealthDep_rank <- IMD$Health.Deprivation.and.Disability.Rank..where.1.is.most.deprived.[match(LSOA_context$LSOA, IMD$LSOA.code..2011.)]
LSOA_context$Employ_rank <- IMD$Employment.Rank..where.1.is.most.deprived.[match(LSOA_context$LSOA, IMD$LSOA.code..2011.)]
LSOA_context$LivEnv_rank <- IMD$Living.Environment.Rank..where.1.is.most.deprived.[match(LSOA_context$LSOA, IMD$LSOA.code..2011.)]

#read in travel time GP data
TT_GP <- read_excel("./Additional_data/Travel time GPS.xlsx", sheet="ACS0505-2011rev", skip=6)
TT_hosp <- read_excel("./Additional_data/Travel time hospitals.xlsx", sheet="ACS0506-2011rev", skip=6)
LSOA_context$GPprop_pop_reasCar <- TT_GP$`%Allcont_carnew1,2` [match(LSOA_context$LSOA, TT_GP$LSOA_code1)]
LSOA_context$GPprop_atRisk_reasPT_Walk <- TT_GP$`%RISKcont_PT/walk1,3` [match(LSOA_context$LSOA, TT_GP$LSOA_code1)]
LSOA_context$atRisk_users <- TT_GP$`0car_households1` [match(LSOA_context$LSOA, TT_GP$LSOA_code1)]
LSOA_context$all_users <- TT_GP$All_households1 [match(LSOA_context$LSOA, TT_GP$LSOA_code1)]
LSOA_context$prop_atRisk <- LSOA_context$atRisk_users/LSOA_context$all_users
LSOA_context$GPpublicTrans_access <- TT_GP$`gpPTfrequency1,3` [match(LSOA_context$LSOA, TT_GP$LSOA_code1)]
LSOA_context$Hosp_prop_pop_reasCar <- TT_hosp$`%Allcont_carnew1,2` [match(LSOA_context$LSOA, TT_hosp$LSOA_code1)]
LSOA_context$Hosp_prop_atRisk_reasPT_Walk <- TT_hosp$`%RISKcont_walk/PT1,2` [match(LSOA_context$LSOA, TT_hosp$LSOA_code1)]
LSOA_context$Hosp_publicTrans_access <- TT_hosp$`hospPTfrequency1,2` [match(LSOA_context$LSOA, TT_hosp$LSOA_code1)]

#read in rural urban data
rural_urban <- read.csv("./Additional_data/Rural_urban.csv", header = TRUE)
LSOA_context$rural_urban <- rural_urban$RUC11[match(LSOA_context$LSOA, rural_urban$LSOA11CD)]

#read in health_levels data
health_levels <- read.csv("./Additional_data/Health_levels.csv", header=TRUE)
health_levels$prop_bad_vbad <- (health_levels$General.Health..Bad.health..measures..Value + health_levels$General.Health..Very.bad.health..measures..Value)/health_levels$General.Health..All.categories..General.health..measures..Value
LSOA_context$prop_badvbad_health <- health_levels$prop_bad_vbad[match(LSOA_context$LSOA, health_levels$geography.code)]

#read in GP registration data
reg_gp <- read.csv("./Additional_data/gp-reg-patients-04-2014-totals-lsoa-alt.csv", header=TRUE)
reg_gp <- reg_gp %>% group_by(LSOA_CODE) %>% summarise(patients=sum(All.Patients))
LSOA_context$GPreg_patients <- reg_gp$patients[match(LSOA_context$LSOA, reg_gp$LSOA_CODE)]
LSOA_context$prop_pop_regGP <- LSOA_context$GPreg_patients/LSOA_context$Total_pop

#unnecessary data at LSOA level
LSOA_unnec <- HES_comb_w %>% group_by(LSOA11) %>% summarise(Unnecessary=sum(Unnecessary), Necessary=sum(Necessary))
LSOA_unnec$prop_unnec <- LSOA_unnec$Unnecessary/(LSOA_unnec$Necessary + LSOA_unnec$Unnecessary)
LSOA_context$prop_unnec <- LSOA_unnec$prop_unnec[match(LSOA_context$LSOA, LSOA_unnec$LSOA11)]

LSOA_context <- merge(x=LSOA_context, y=HDGP_comb_g[,c(1,15,16)], by.x="LSOA", by.y="LSOA11.x", all.x=TRUE)

#create variables from combined data at a LSOA level
LSOA_var <- HDGP_comb_g %>% group_by(LSOA11.x) %>% dplyr::summarise(Attendances=sum(Attendances.x), reg_GP_dist=median(reg_GP_dist), act_dist_hosp=median(act_dist_hosp))
LSOA_GPcloser <- HDGP_comb_g %>% group_by(LSOA11.x, GPcloser) %>% dplyr::summarise(Attendances=sum(Attendances.x))

LSOA_context$act_dist_hosp <- LSOA_var$act_dist_hosp[match(LSOA_context$LSOA, LSOA_var$LSOA11.x)]
LSOA_context$reg_GP_dist <- LSOA_var$reg_GP_dist[match(LSOA_context$LSOA, LSOA_var$LSOA11.x)]

#Combine LSOA context data with final LSOA level data
LSOA_context1 <- merge(x=LSOA_context, y=LSOA_w[,c(1:11,17)], by.x="LSOA", by.y="LSOA11.x")
LSOA_context <- merge(x=LSOA_context, y=LSOA_w[,c(1,16)], by.x="LSOA", by.y="LSOA11.x")


#split data into training, test and validation
Mboro_LSOAs <- LSOA_loc[grepl("Middlesbrough", LSOA_loc[["LSOA.name"]]),2]

LSOA_val_context <- LSOA_context[LSOA_context$LSOA %in% Mboro_LSOAs,]
LSOA_tt_context <- LSOA_context[!LSOA_context$LSOA %in% Mboro_LSOAs,]
n <- nrow(LSOA_tt_context)
train_indices <- 1:round(0.7 * n)
LSOA_train_context <- LSOA_tt_context[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
LSOA_test_context <- LSOA_tt_context[test_indices, ]

#prepare a logisitic regression including all contextual predictors
reg_clas1_con <- lm(prop_unnec~., data=LSOA_context[,-c(1)])
reg_clas1_con_sum <- summary(reg_clas1_con)

#refine based on initial models results
reg_clas1_con1 <- lm(prop_unnec~ IMD_rank + prop_atRisk + Hosp_publicTrans_access
                    + rural_urban + prop_badvbad_health, data=LSOA_context[,-c(1)])
reg_clas1_con1_sum <- summary(reg_clas1_con1)

#produce a logisitic regression including all predictors (context and distance)
reg_clas1_con <- lm(prop_unnec.x~., data=LSOA_context1[,-c(1)])
reg_clas1_con_sum <- summary(reg_clas1_con)

#refine based on initial models results
reg_clas2_con <- lm(prop_unnec.x~IMD_rank + prop_atRisk + Hosp_prop_pop_reasCar  
                    + Hosp_prop_atRisk_reasPT_Walk + Hosp_publicTrans_access  + 
                      rural_urban + prop_badvbad_health  + dist_hosp  + Attendances 
                    + reg_GP_closer + act_dist_hosp + GP_closer_att , data=LSOA_context1[,-c(1)])
reg_clas2_con_sum <- summary(reg_clas2_con)

reg_clas2_cong <- glm(prop_unnec.x~IMD_rank + prop_atRisk + Hosp_prop_pop_reasCar  
                     + Hosp_prop_atRisk_reasPT_Walk + Hosp_publicTrans_access  + 
                       rural_urban + prop_badvbad_health  + dist_hosp  + Attendances 
                     + reg_GP_closer + act_dist_hosp + GP_closer_att , data=LSOA_context1[,-c(1)])
reg_clas2_cong_sum <- summary(reg_clas2_cong)