#function to read in the HES data which includes the registered GP field.
setwd("~/MSc project/data")
source("function_library.R")
key<- setup_wrkspc()

HDGP <- prepare_HES_regGP("HDGP.csv")
HDGP_c <- create_GPLoc("HDGP_c.csv")
HDGP_comb_g <- create_dist_comb("HDGP_comb_g.csv")
HDGP_comb_g_w <- create_wide_elements("HDGP_comb_g_w.csv")
LSOAs <- Agg_data(TRUE)
LSOA <- as.data.frame(LSOAs[1])
LSOA_w <- as.data.frame(LSOAs[2])
LSOA_loc <- create_LSOAloc("LSOA_loc.csv")

##Prduction of exploratory visualisations

#proportion of patients not registered at a GP
prop_unreg <- ggplot(LSOA, aes(y=prop_unreg)) + geom_boxplot(fill="steelblue") + theme_classic() + labs(y="Proportion of patients not registerd at a GP within an LSOA")
ggsave("prop_unreg.png")

#distance from registered GP
dist_reg_gp <- ggplot(LSOA, aes(y=reg_GP_dist)) + geom_boxplot(fill="steelblue") + theme_classic() + labs(y="Distance from registered GP")
ggsave("dist_reg_gp.png")

#relationship between distance to registered GP and closest hospital
Dist_regGP_vs_hosp <- ggplot(HDGP_comb_g_w, aes(y=dist_hosp , x=reg_GP_dist)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Distance from registered\n GP in kilometers", y="Distance from nearest hospital in kilometers")
ggsave("regGP_vs_Hosp.png")
cor(HDGP_comb_g_w$dist_hosp, HDGP_comb_g_w$reg_GP_dist)

#relationship between distance to hospital attended and registered GP
Dist_regGP_vs_Act_hosp <- ggplot(HDGP_comb_g_w, aes(x=reg_GP_dist , y=act_dist_hosp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Distance from registered\n GP in kilometers", y="Distance from attended hospital in kilometers") 
ggsave("Act_hosp_vs_regGP.png")
cor(HDGP_comb_g_w$reg_GP_dist, HDGP_comb_g_w$act_dist_hosp)

#relationship between distance to hospital attended and registered GP (cropped)
Dist_regGP_vs_Act_hosp_crop <- ggplot(HDGP_comb_g_w, aes(x=reg_GP_dist , y=act_dist_hosp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Distance from registered\n GP in kilometers", y="Distance from attended hospital in kilometers") + scale_x_continuous(name="Distance from registered\n GP in kilometers", limits=c(0, 1))
ggsave("Act_hosp_vs_regGP_crop.png")
cor(HDGP_comb_g_w$reg_GP_dist, HDGP_comb_g_w$act_dist_hosp)

#relationship between distance to registered GP and closest hospital
Dist_regGP_vs_GP <- ggplot(HDGP_comb_g_w, aes(y=dist_gp , x=reg_GP_dist)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Distance from registered\n GP in kilometers", y="Distance from nearest GP in kilometers")
ggsave("regGP_vs_GP.png")
cor(HDGP_comb_g_w$dist_gp, HDGP_comb_g_w$reg_GP_dist)

#relationship between distance to registered GP and unnec attendances
Dist_regGP_vs_prop_unnec <- ggplot(HDGP_comb_g_w, aes(y=reg_GP_dist, x=prop_unnec)) + geom_point(colour="steelblue") + theme_classic() + labs(y="Distance from registered GP in kilometers", x="Number of \nunnecessary attendances") 
ggsave("regGP_vs_unnec.png")
cor(HDGP_comb_g_w$prop_unnec, HDGP_comb_g_w$reg_GP_dist)

#relationship between distance to registered GP and unnec attendances (cropped)
Dist_regGP_vs_prop_unnec <- ggplot(HDGP_comb_g_w, aes(y=reg_GP_dist, x=prop_unnec)) + geom_point(colour="steelblue") + theme_classic() + labs(y="Distance from registered GP in kilometers", x="Number of \nunnecessary attendances") + scale_y_continuous(name="Distance from registered\n GP in kilometers", limits=c(0, 1))
ggsave("regGP_vs_unnec_crop.png")
cor(HDGP_comb_g_w$prop_unnec, HDGP_comb_g_w$reg_GP_dist)

##hypothesis testing

#split data into training, test and validation
Mboro_LSOAs <- LSOA_loc[grepl("Middlesbrough", LSOA_loc[["LSOA.name"]]),2]

LSOA_val_w <- LSOA_w[LSOA_w$LSOA11.x %in% Mboro_LSOAs,]
LSOA_tt_w <- LSOA_w[!LSOA_w$LSOA11.x %in% Mboro_LSOAs,]
n <- nrow(LSOA_tt_w)
train_indices <- 1:round(0.7 * n)
LSOA_train_w <- LSOA_tt_w[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
LSOA_test_w <- LSOA_tt_w[test_indices, ]

LSOA_val <- LSOA[LSOA$LSOA11.x %in% Mboro_LSOAs,]
LSOA_tt <- LSOA[!LSOA$LSOA11.x %in% Mboro_LSOAs,]
n <- nrow(LSOA_tt)
train_indices <- 1:round(0.7 * n)
LSOA_train <- LSOA_tt[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
LSOA_test <- LSOA_tt[test_indices, ]

#prepare data for classification looking to classify attendances as unnecessary
LSOA_train1 <- LSOA_train[,-c(1,6,13,14,15)]
reg_clas1 <- glm(UNNECESSARY.x~., data=LSOA_train1, family="binomial")
reg_clas1_sum <- summary(reg_clas1)

#we presume that the attendance is unnecessary if the predicted probablity is greater than 0.5
proba <- predict(reg_clas1, type="response")
predict<- rep("Necessary", length(proba))
predict[proba > 0.5] <- "Unecessary"
actual <- LSOA_train$UNNECESSARY.x
table(predict,actual)
mean(predict==actual)

#refine model based on significance results and refit
reg_clas2 <- glm(UNNECESSARY.x~dist_hosp + dist_gp + GPcloser 
                 + reg_GP_closer + act_dist_hosp + GP_closer_att, 
                 data=LSOA_train1, family="binomial")
reg_clas2_sum <- summary(reg_clas2)

#we presume that the attendance is unnecessary if the predicted probablity is greater than 0.5
proba <- predict(reg_clas2, type="response")
predict<- rep("Necessary", length(proba))
predict[proba > 0.5] <- "Unnecessary"
actual <- LSOA_train$UNNECESSARY.x
table(predict,actual)
mean(predict == actual)

#test removing reg_GP_dist which had the lowest significance of remaining variables
reg_clas3 <- glm(UNNECESSARY.x~dist_hosp + dist_gp + GPcloser
                 + reg_GP_closer + act_dist_hosp + GP_closer_att, 
                 data=LSOA_train1, family="binomial")
reg_clas3_sum <- summary(reg_clas3)

#we presume that the attendance is unnecessary if the predicted probablity is greater than 0.5
proba <- predict(reg_clas3, type="response")
predict<- rep("Necessary", length(proba))
predict[proba > 0.5] <- "Unnecessary"
actual <- LSOA_train$UNNECESSARY.x
table(predict,actual)
mean(predict == actual)

#apply this model to the test data
proba <- predict(reg_clas3, newdata=LSOA_test, type="response")
predict<- rep("Necessary", length(proba))
predict[proba > 0.5] <- "Unnecessary"
actual <- LSOA_test$UNNECESSARY.x
table(predict,actual)
mean(predict == actual)

#apply this model to the validation data
proba <- predict(reg_clas3, newdata=LSOA_val, type="response")
predict<- rep("Necessary", length(proba))
predict[proba > 0.5] <- "Unnecessary"
actual <- LSOA_val$UNNECESSARY.x
table(predict,actual)
mean(predict == actual)


##Exploration of decision trees and random forest models, not considered in the report.
install.packages("rpart")
library(rpart)

LSOA_train_w1<- LSOA_train_w[,-c(1,12,13,14,15)]
rpart.unnec_reg <- rpart(prop_unnec~ dist_hosp + dist_gp + GPcloser + reg_GP_dist
                     + reg_GP_closer + nearest_GP_reg + act_dist_hosp + GP_closer_att
                     + nearest_hosp_att + prop_unreg, 
                     data=LSOA_train_w1, method="anova", weights = Attendances)
plotcp(rpart.unnec_reg)
summary(rpart.unnec_reg)
plot(rpart.unnec_reg, uniform=TRUE, main="Regression tree for prop_unnec")
text(rpart.unnec_reg, use.n=TRUE, all=TRUE, cex=.8)


LSOA_train1<- LSOA_train[,-c(1,13,14)]
rpart.unnec_class <- rpart(UNNECESSARY.x~ dist_hosp + dist_gp + GPcloser + reg_GP_dist
                     + reg_GP_closer + nearest_GP_reg + act_dist_hosp + GP_closer_att
                     + nearest_hosp_att + prop_unreg, 
                     data=LSOA_train1, method="class", weights = Attendances)
plotcp(rpart.unnec_class)
summary(rpart.unnec_class)
plot(rpart.unnec_class, uniform=TRUE, main="Classification tree for prop_unnec")
text(rpart.unnec_class, use.n=TRUE, all=TRUE, cex=.8)

rf.unnec_reg <- randomForest(prop_unnec~ dist_hosp + dist_gp + GPcloser + reg_GP_dist
                     + reg_GP_closer + nearest_GP_reg + act_dist_hosp + GP_closer_att
                     + nearest_hosp_att + prop_unreg, 
                     data=LSOA_train_w1, method="anova", weights = Attendances)

rf.unnec_class <- randomForest(UNNECESSARY.x~ dist_hosp + dist_gp + GPcloser + reg_GP_dist
                           + reg_GP_closer + nearest_GP_reg + act_dist_hosp + GP_closer_att
                           + nearest_hosp_att + prop_unreg, 
                           data=LSOA_train1, method="class", weights = Attendances)
