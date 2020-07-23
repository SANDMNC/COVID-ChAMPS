
# covid_data_child_scored.csv for robust lmm
covid_data_child <- read.csv("scored_data/covid_data_child_scored_for_MI.csv", header=TRUE, stringsAsFactors = FALSE)
# miData created from covid_missing.R script for MI analyses

library(lmerTest)
library(robustlmm)
library(reghelper)
library(ggplot2)
library(lmeresampler)
library(lme4)
library(nlme)
library(boot)
library(mice)
library(mitml)

#Remove the X column
covid_data_child$X <- NULL

#Create separate datasets for males and females for any post hoc tests
data_female <- subset(covid_data_child, covid_data_child$ch_gender_num==1)
data_male <- subset(covid_data_child, covid_data_child$ch_gender_num==2)

#Create num code for other parent in the home
covid_data_child$other_par_num<- ifelse(covid_data_child$other_par == "No", 0,1)
    
#check parent past disorder association with key variables
model <- lme(totalSDQ2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lmer(totalSDQ ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lmer(totalPARQ ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(PARQhostile ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(PARQwarmth ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(totalcov_dist ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(totalPTSD ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(DASSDep ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(DASSAnx ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(DASSStress ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(facts_comm ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(emotion_comm ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(self_comm ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(SDQemo2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(SDQhyp2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(SDQcon2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(SDQpeer2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(totalPTSD ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
#parent past disorder is associated with SDQ, PTSD, DASS and covid distress, but not PARQ, comm or SDQ2 vbls
#consider adding as a covariate in models


#check if knowing someone hospitalised from COVID is related to key vbls
model <- lme(totalSDQ2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(totalSDQ ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(totalPARQ ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(PARQhostile ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(PARQwarmth ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(totalcov_dist ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(covid_pos_num ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(DASSDep ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(DASSAnx ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(DASSStress ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(facts_comm ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(emotion_comm ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(self_comm ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(SDQemo2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(SDQhyp2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(SDQcon2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(SDQpeer2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
model <- lme(totalPTSD ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model)
#knowing someone hospitalised (n=57) associated with DASSDep, DASSAnx and total_cov_distress


model1 <- lme(SDQemo2 ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(SDQhyp2 ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(SDQcon2 ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(SDQpeer2 ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(totalPTSD ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(PARQhostile ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(PARQwarmth ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(totalFES ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(facts_comm ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(emotion_comm ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(self_comm ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(totalcov_dist ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(covid_pos_num ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(DASSDep ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(DASSAnx ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)
model1 <- lme(DASSStress ~ as.factor(as.factor(country_cat)), random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(model1)


#Series of mixed models below - a number of predictors & 5 outcome variables for each (SDQchange total, emo, con, hyp, peer subscales; PTSD)
#Models 1-3 test main effects (#1), 2-way cinteractions (#2), 3-way interactions (#3)
#Model 4 tests 4 way interaction only for SDQchange outcomes 

#PARQ hostile as predictor of SDQ emo change since COVID
model1 <- lme(SDQemo2 ~ PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
summary(model1)

# test normality of resisuals
r<-residuals(model1, type = c("response", "pearson", "normalized"))
shapiro.test(r)
# Note that residuals are not normally distributed. I have checked a number of others and same deal. As such, procede with robust lmm,
# as a supplement to results from multiple imputation

#ROBUST ESTIMATOR
model1.robust <- rlmer(SDQemo2 ~ PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
summary(model1.robust)

# p values as described here: https://stats.stackexchange.com/questions/430490/obtaining-p-values-in-a-robustlmm-mixed-model-via-satterthwaite-approximated-dfs
# get coefficients from non-robust model to extract Satterthwaite approximated DFs
coefs <- data.frame(coef(summary(model1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# MI
model1 <- with(miData,lme4::lmer(SDQemo2 ~ PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1|id), REML = FALSE))
summary(pool(model1))

#bootstrap function for lmer (boot)
#model1 <- lmer(SDQemo2 ~ PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num + (1|id), data = covid_data_child, REML = FALSE, na.action = na.exclude)
#summary(pool(model1)
#model1boot <- bootMer(model1, FUN = fixef, nsim = 100, verbose = T)
#boot.ci(model1boot, index = 2, type=c("norm", "basic", "perc"), conf = c(0.95, 0.99, 0.999))

model2 <- with(miData,lme4::lmer(SDQemo2 ~ PARQhostile.c*SDQemo.c+PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ PARQhostile.c*ch_age.c*ch_gender_num+PARQhostile.c*ch_age.c*SDQemo.c+PARQhostile.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ PARQhostile.c*ch_age.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

#Model 1, PARQhostile p .056

#PARQ hostile as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
#rlmer
model1.1 <- lme(SDQhyp2 ~ PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


model2 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+PARQhostile.c*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQhostile.c*ch_age.c*SDQhyp.c+PARQhostile.c*ch_gender_num*SDQhyp.c+PARQhostile.c*ch_gender_num*ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQhostile.c*ch_age.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model1 best fit - PARQhostile significant main effect 2.749229e-02

# Control par mental health

model1 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQhostile.c+SDQhyp.c+par_past_mh_num+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Moderating role of SES
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQhostile.c*income_famsize.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQhostile.c*as.factor(country_cat)+SDQhyp.c+par_age+ch_age.c+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))


#PARQ hostile as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ PARQhostile.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ PARQhostile.c*SDQcon.c+PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ PARQhostile.c*ch_age.c*SDQcon.c+PARQhostile.c*ch_gender_num*SDQcon.c+PARQhostile.c*ch_gender_num*ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ PARQhostile.c*ch_age.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 2 best fit MI p 0.001928347

#Moderating role of pare MH, SES, country
model2 <- with(miData,lme4::lmer(SDQcon2 ~ income_famsize.c*PARQhostile.c*SDQcon.c+PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ as.factor(country_cat)*PARQhostile.c*SDQcon.c+PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))

# Robust lmm for model 2
model2.1 <- lme(SDQcon2 ~ PARQhostile.c*SDQcon.c+PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQcon2 ~ PARQhostile.c*SDQcon.c+PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#PARQ hostile as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ PARQhostile.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+as.factor(country_cat)+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQpeer.c*PARQhostile.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ PARQhostile.c*ch_age.c*ch_gender_num+PARQhostile.c*ch_age.c*SDQpeer.c+PARQhostile.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ PARQhostile.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

#Model 1 best fit, PARQhostile not sig p 0.8

#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ PARQhostile.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

#model 1 best fit p 9.79E-12

# Moderating role of SES, country
model1 <- with(miData,lme4::lmer(totalPTSD ~ income_famsize.c*PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ as.factor(country_cat)*PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ other_par_num*PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm for model 1
model1.1 <- lme(totalPTSD.c ~ PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

lme.dscore(model1.1,data = covid_data_child, type = "nlme")

#DASS DASSDep as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ DASSDep.c*SDQemo.c+DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ DASSDep.c*ch_age.c*ch_gender_num+DASSDep.c*ch_age.c*SDQemo.c+DASSDep.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ DASSDep.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit p 7.846860e-03

#Moderating effect of SES
model1 <- with(miData,lme4::lmer(SDQemo2 ~ income_famsize.c*DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQemo2 ~ as.factor(country_cat)*DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))


model1.1 <- lme(SDQemo2 ~ DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#DASS DASSDep as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSDep.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSDep.c*SDQhyp.c+DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSDep.c*ch_age.c*ch_gender_num+DASSDep.c*ch_age.c*SDQhyp.c+DASSDep.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSDep.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit 6.734982e-02


#DASS DASSDep as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ DASSDep.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ DASSDep.c*SDQcon.c+DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ DASSDep.c*ch_age.c*ch_gender_num+DASSDep.c*ch_age.c*SDQcon.c+DASSDep.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ DASSDep.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p .07


#DASS DASSDep as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSDep.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSDep.c*SDQpeer.c+DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSDep.c*ch_age.c*ch_gender_num+DASSDep.c*ch_age.c*SDQpeer.c+DASSDep.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSDep.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 0.0577645


#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ DASSDep.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 2.216370e-07

#Moderating effect of SES, country
model1 <- with(miData,lme4::lmer(totalPTSD ~ income_famsize.c*DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ par_ed_ord*DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ as.factor(country_cat)*DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#DASS DASSAnx as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ DASSAnx.c*SDQemo.c+DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ DASSAnx.c*ch_age.c*ch_gender_num+DASSAnx.c*ch_age.c*SDQemo.c+DASSAnx.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ DASSAnx.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 1.55E-04

#Moderating role of SES, country
model1 <- with(miData,lme4::lmer(SDQemo2 ~ income_famsize.c*DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQemo2 ~ par_ed_ord*DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQemo2 ~ as.factor(country_cat)*DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(SDQemo2 ~ DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#DASS DASSAnx as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSAnx.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSAnx.c*SDQhyp.c+DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSAnx.c*ch_age.c*ch_gender_num+DASSAnx.c*ch_age.c*SDQhyp.c+DASSAnx.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSAnx.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 1.583413e-01

#DASS DASSAnx as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ DASSAnx.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ DASSAnx.c*SDQcon.c+DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ DASSAnx.c*ch_age.c*ch_gender_num+DASSAnx.c*ch_age.c*SDQcon.c+DASSAnx.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ DASSAnx.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 2.686398e-01

#DASS DASSAnx as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSAnx.c*SDQpeer.c+DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSAnx.c*ch_age.c*ch_gender_num+DASSAnx.c*ch_age.c*SDQpeer.c+DASSAnx.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSAnx.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 0.04

#Moderating role of SES, country
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ income_famsize.c*DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ as.factor(country_cat)*DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ other_par_num*DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ DASSAnx.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 1.63E-06

#Moderating role of SES, country
model1 <- with(miData,lme4::lmer(totalPTSD ~ income_famsize.c*DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ other_par_num*DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ as.factor(country_cat)*DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#DASS DASSStress as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ DASSStress.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ DASSStress.c*SDQemo.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ DASSStress.c*ch_age.c*ch_gender_num+DASSStress.c*ch_age.c*SDQemo.c+DASSStress.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ DASSStress.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 2 best fit, p for interaction 1.22E-02

#Moderating role of SES, country
model2 <- with(miData,lme4::lmer(SDQemo2 ~ income_famsize.c*DASSStress.c*SDQemo.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ other_par_num*DASSStress.c*SDQemo.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ as.factor(country_cat)*DASSStress.c*SDQemo.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))

# Robust lmm
model2.1 <- lme(SDQemo2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+DASSStress.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQemo2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+DASSStress.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#DASS DASSStress as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSStress.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSStress.c*SDQhyp.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSStress.c*ch_age.c*ch_gender_num+DASSStress.c*ch_age.c*SDQhyp.c+DASSStress.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSStress.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 2 best fit, Stress*age p 5.83E-03

#Moderating role of SES, country
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSStress.c*SDQhyp.c+income_famsize.c*DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSStress.c*SDQhyp.c+other_par_num*DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ DASSStress.c*SDQhyp.c+as.factor(country_cat)*DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))

# Robust lmm
model2.1 <- lme(SDQhyp2 ~ DASSStress.c*ch_gender_num+DASSStress.c*SDQhyp.c+DASSStress.c*ch_age.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQhyp2 ~ DASSStress.c*ch_gender_num+DASSStress.c*SDQhyp.c+DASSStress.c*ch_age.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#DASS DASSStress as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ DASSStress.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ DASSStress.c*SDQcon.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ DASSStress.c*ch_age.c*ch_gender_num+DASSStress.c*ch_age.c*SDQcon.c+DASSStress.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ DASSStress.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 2 best, p 0.0036

# Moderating effect of SES, country
model2 <- with(miData,lme4::lmer(SDQcon2 ~ income_famsize.c*DASSStress.c*SDQcon.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ other_par_num*DASSStress.c*SDQcon.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ as.factor(country_cat)*DASSStress.c*SDQcon.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))


# Robust lmm
model2.1 <- lme(SDQcon2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+DASSStress.c*SDQcon.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQhyp2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+DASSStress.c*SDQcon.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#DASS DASSStress as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSStress.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSStress.c*SDQpeer.c+DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSStress.c*ch_age.c*ch_gender_num+DASSStress.c*ch_age.c*SDQpeer.c+DASSStress.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ DASSStress.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 0.018

#Moderating role of SES, country
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ income_famsize.c*DASSStress.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ other_par_num*DASSStress.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ as.factor(country_cat)*DASSStress.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))


# Robust lmm
model1.1 <- lme(SDQpeer2 ~ DASSStress.c+SDQpeer.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ DASSStress.c+SDQpeer.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ DASSStress.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Robust lmm
model1.1 <- lme(totalPTSD ~ DASSStress.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD ~ DASSStress.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Model 1 best fit, p 1.09E-07

#Moderating effect of SES, country
model1 <- with(miData,lme4::lmer(totalPTSD ~ income_famsize.c*DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ other_par_num*DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ as.factor(country_cat)*DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))


#totalcov_dist as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ totalcov_dist.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ totalcov_dist.c*SDQemo.c+totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ totalcov_dist.c*ch_age.c*ch_gender_num+totalcov_dist.c*ch_age.c*SDQemo.c+totalcov_dist.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ totalcov_dist.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 2 best fit, interaction p 1.611520e-03

# Moderating effect of SES, country
model2 <- with(miData,lme4::lmer(SDQemo2 ~ income_famsize.c*totalcov_dist.c*SDQemo.c+totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ par_ed_ord*totalcov_dist.c*SDQemo.c+totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ as.factor(country_cat)*totalcov_dist.c*SDQemo.c+totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))

# Robust lmm
model2.1 <- lme(SDQemo2 ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+totalcov_dist.c*SDQemo.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQemo2 ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+totalcov_dist.c*SDQemo.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#Controlling for DASS Stress, sig 7.354175e-04
model2.Stress <- with(miData,lme4::lmer(SDQemo2 ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+totalcov_dist.c*SDQemo.c+DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2.Stress))

#totalcov_dist as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ totalcov_dist.c*SDQhyp.c+totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ totalcov_dist.c*ch_age.c*ch_gender_num+totalcov_dist.c*ch_age.c*SDQhyp.c+totalcov_dist.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ totalcov_dist.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 0.0001

#Moderating effect of SES, country
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ income_famsize.c*totalcov_dist.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ par_ed_ord*totalcov_dist.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ as.factor(country_cat)*totalcov_dist.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+ch_age.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# controlling for DASS Stress, sig 7.444949e-03
model1.Stress <- with(miData,lme4::lmer(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))


#totalcov_dist as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ totalcov_dist.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ totalcov_dist.c*SDQcon.c+totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ totalcov_dist.c*ch_age.c*ch_gender_num+totalcov_dist.c*ch_age.c*SDQcon.c+totalcov_dist.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ totalcov_dist.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best. p .0138

#Moderating role of SES, country
model1 <- with(miData,lme4::lmer(SDQcon2 ~ income_famsize.c*totalcov_dist.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQcon2 ~ par_ed_ord*totalcov_dist.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQcon2 ~ as.factor(country_cat)*totalcov_dist.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(SDQcon2 ~ totalcov_dist.c+ch_age.c+SDQcon.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQcon2 ~ totalcov_dist.c+ch_age.c+SDQcon.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Controlling for DASS Stress, model 1 not sig
model1.Stress <- with(miData,lme4::lmer(SDQcon2 ~ totalcov_dist.c+SDQcon.c+DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))


#totalcov_dist as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ totalcov_dist.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ totalcov_dist.c*SDQpeer.c+totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ totalcov_dist.c*ch_age.c*ch_gender_num+totalcov_dist.c*ch_age.c*SDQpeer.c+totalcov_dist.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ totalcov_dist.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 0.009

# Moderating role of SES, country
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ income_famsize.c*totalcov_dist.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ other_par_num*totalcov_dist.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ as.factor(country_cat)*totalcov_dist.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ totalcov_dist.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ totalcov_dist.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Controlling for DASS Stress, not sig 0.08017983
model1.Stress <- with(miData,lme4::lmer(SDQpeer2 ~ totalcov_dist.c+DASSStress.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))


#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ totalcov_dist.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ totalcov_dist.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 1.355686e-09

#Moderating role of SES, country
model1 <- with(miData,lme4::lmer(totalPTSD ~ income_famsize.c*totalcov_dist.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ other_par_num*totalcov_dist.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ as.factor(country_cat)*totalcov_dist.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ totalcov_dist.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ totalcov_dist.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Controlling for DASS Stress, sig 1.874548e-04
model1.Stress <- with(miData,lme4::lmer(totalPTSD ~ totalcov_dist.c+DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))

#covid_pos_num as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ covid_pos_num.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ covid_pos_num.c*SDQemo.c+covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ covid_pos_num.c*ch_age.c*ch_gender_num+covid_pos_num.c*ch_age.c*SDQemo.c+covid_pos_num.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ covid_pos_num.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 0.04, not sig with robust

# Robust lmm
model1.1 <- lme(SDQemo2 ~ covid_pos_num.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ covid_pos_num.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#covid_pos_num as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ covid_pos_num.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ covid_pos_num.c*SDQhyp.c+covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ covid_pos_num.c*ch_age.c*ch_gender_num+covid_pos_num.c*ch_age.c*SDQhyp.c+covid_pos_num.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ covid_pos_num.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 3.909657e-02

# Robust lmm
model1.1 <- lme(SDQhyp2 ~ covid_pos_num.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ covid_pos_num.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#covid_pos_num as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ covid_pos_num.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ covid_pos_num.c*SDQcon.c+covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ covid_pos_num.c*ch_age.c*ch_gender_num+covid_pos_num.c*ch_age.c*SDQcon.c+covid_pos_num.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ covid_pos_num.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 2 best fit, p .04

# Robust lmm
model2.1 <- lme(SDQhyp2 ~ covid_pos_num.c*SDQcon.c+covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQhyp2 ~ covid_pos_num.c*SDQcon.c+covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#covid_pos_num as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ covid_pos_num.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ covid_pos_num.c*SDQpeer.c+covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ covid_pos_num.c*ch_age.c*ch_gender_num+covid_pos_num.c*ch_age.c*SDQpeer.c+covid_pos_num.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ covid_pos_num.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)

# Model 1 best fit, covid_pos p 0.008

# Moderating role of SES, country
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ income_famsize.c*covid_pos_num.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ other_par_num*covid_pos_num.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ as.factor(country_cat)*covid_pos_num.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ covid_pos_num.c+ch_age.c+ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ covid_pos_num.c+ch_age.c+ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)++par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ covid_pos_num.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ covid_pos_num.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 3.192478e-02

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ covid_pos_num.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ covid_pos_num.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#PARQwarmth as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ PARQwarmth.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ PARQwarmth.c*SDQemo.c+PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ PARQwarmth.c*ch_age.c*ch_gender_num+PARQwarmth.c*ch_age.c*SDQemo.c+PARQwarmth.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ PARQwarmth.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit but warmth not sig

#PARQwarmth as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQwarmth.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQwarmth.c*SDQhyp.c+PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQwarmth.c*ch_age.c*ch_gender_num+PARQwarmth.c*ch_age.c*SDQhyp.c+PARQwarmth.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ PARQwarmth.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit but warmth not sig

#PARQwarmth as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ PARQwarmth.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ PARQwarmth.c*SDQcon.c+PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ PARQwarmth.c*ch_age.c*ch_gender_num+PARQwarmth.c*ch_age.c*SDQcon.c+PARQwarmth.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ PARQwarmth.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best, not sig


#PARQwarmth as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ PARQwarmth.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ PARQwarmth.c*SDQpeer.c+PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ PARQwarmth.c*ch_age.c*ch_gender_num+PARQwarmth.c*ch_age.c*SDQpeer.c+PARQwarmth.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ PARQwarmth.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, not sig

#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ PARQwarmth.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ PARQwarmth.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 6.87E-07

#Moderating effect of SES, country
model1 <- with(miData,lme4::lmer(totalPTSD ~ income_famsize.c*PARQwarmth.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ par_ed_ord*PARQwarmth.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ as.factor(country_cat)*PARQwarmth.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ PARQwarmth.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ PARQwarmth.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

ggplot(covid_data_child, aes(x = PARQwarmth.c, y = totalPTSD) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#totalFES as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ totalFES.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ totalFES.c*SDQemo.c+totalFES.c*ch_age.c+totalFES.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ totalFES.c*ch_age.c*ch_gender_num+totalFES.c*ch_age.c*SDQemo.c+totalFES.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ totalFES.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best, p 4.691009e-02, Roust not sig p .08

# Robust lmm
model1.1 <- lme(SDQemo2 ~ totalFES.c+ch_age.c+SDQemo.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ totalFES.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#totalFES as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ totalFES.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ totalFES.c*SDQhyp.c+totalFES.c*ch_age.c+totalFES.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ totalFES.c*ch_age.c*ch_gender_num+totalFES.c*ch_age.c*SDQhyp.c+totalFES.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ totalFES.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best, not sig

#totalFES as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ totalFES.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ totalFES.c*SDQcon.c+totalFES.c*ch_age.c+totalFES.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ totalFES.c*ch_age.c*ch_gender_num+totalFES.c*ch_age.c*SDQcon.c+totalFES.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ totalFES.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best, p 0.0273, not sig w robust

# Robust lmm
model1.1 <- lme(SDQcon2 ~ totalFES.c+ch_age.c+SDQcon.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQcon2 ~ totalFES.c+SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#totalFES as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ totalFES.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ totalFES.c*SDQpeer.c+totalFES.c*ch_age.c+totalFES.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ totalFES.c*ch_age.c*ch_gender_num+totalFES.c*ch_age.c*SDQpeer.c+totalFES.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ totalFES.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 sig p .017

# Moderating effect of SES, country
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ income_famsize.c*totalFES.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ other_par_num*totalFES.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ as.factor(country_cat)*totalFES.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ totalFES.c+ch_age.c+SDQpeer.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ totalFES.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ totalFES.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ totalFES.c*ch_age.c+totalFES.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ totalFES.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 2 best fit, p 0.02, but robust not sig, so go back to model 1.
# Model 1, p 1.80E-05

#Moderating role of SES, country
model1 <- with(miData,lme4::lmer(totalPTSD ~ income_famsize.c*totalFES.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ other_par_num*totalFES.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ as.factor(country_cat)*totalFES.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ totalFES.c+ch_age.c+totalFES.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ totalFES.c+ch_age.c+totalFES.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#COVID communication variables as predictors: communication about facts, emotions and self-focussed

#facts_comm as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ facts_comm.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ facts_comm.c*SDQemo.c+facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ facts_comm.c*ch_age.c*ch_gender_num+facts_comm.c*ch_age.c*SDQemo.c+facts_comm.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ facts_comm.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

#facts_comm as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ facts_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ facts_comm.c*SDQhyp.c+facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ facts_comm.c*ch_age.c*ch_gender_num+facts_comm.c*ch_age.c*SDQhyp.c+facts_comm.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ facts_comm.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

#facts_comm as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ facts_comm.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ facts_comm.c*SDQcon.c+facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ facts_comm.c*ch_age.c*ch_gender_num+facts_comm.c*ch_age.c*SDQcon.c+facts_comm.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ facts_comm.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

#facts_comm as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ facts_comm.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ facts_comm.c*SDQpeer.c+facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ facts_comm.c*ch_age.c*ch_gender_num+facts_comm.c*ch_age.c*SDQpeer.c+facts_comm.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ facts_comm.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)


#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ facts_comm.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ facts_comm.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

#Communication about emotional content (child, self, others) + reassurance

#emotion_comm as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*SDQemo.c+emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*ch_age.c*ch_gender_num+emotion_comm.c*ch_age.c*SDQemo.c+emotion_comm.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

#model 1 best fit p 8.75E-03

#Moderating effect of SES, country
model1 <- with(miData,lme4::lmer(SDQemo2 ~ income_famsize.c*emotion_comm.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQemo2 ~ other_par_num*emotion_comm.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQemo2 ~ as.factor(country_cat)*emotion_comm.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(SDQemo2 ~ emotion_comm.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ emotion_comm.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.hos <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*PARQwarmth.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*totalFES.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*DASSStress.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Distress <- with(miData,lme4::lmer(SDQemo2 ~ emotion_comm.c*totalcov_dist.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Distress))

#emotion_comm as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*SDQhyp.c+emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*ch_age.c*ch_gender_num+emotion_comm.c*ch_age.c*SDQhyp.c+emotion_comm.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best, p 1.45E-02

# Moderating role of SES, country
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ income_famsize.c*emotion_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ other_par_num*emotion_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ as.factor(country_cat)*emotion_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))

# Robust lmm
model1.1 <- lme(SDQhyp2 ~ emotion_comm.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ emotion_comm.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Check moderating influence of family enviro
model1.hos <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*PARQwarmth.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*totalFES.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*DASSStress.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*DASSDep.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*DASSAnx.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Distress <- with(miData,lme4::lmer(SDQhyp2 ~ emotion_comm.c*totalcov_dist.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Distress))

#emotion_comm as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ emotion_comm.c*totalFES.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ emotion_comm.c*SDQcon.c+emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ emotion_comm.c*ch_age.c*ch_gender_num+emotion_comm.c*ch_age.c*SDQcon.c+emotion_comm.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ emotion_comm.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1, p 0.02, Robust not sig

# Robust lmm
model1.1 <- lme(SDQcon2 ~ emotion_comm.c+SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQcon2 ~ emotion_comm.c+SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#emotion_comm as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*SDQpeer.c+emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*ch_age.c*ch_gender_num+emotion_comm.c*ch_age.c*SDQpeer.c+emotion_comm.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 0.03527664

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ emotion_comm.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ emotion_comm.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Check moderating influence of family enviro, SES, country
model1.hos <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*PARQhostile.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*PARQwarmth.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*totalFES.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*DASSStress.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*DASSDep.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*income_famsize.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*par_ed_ord+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(SDQpeer2 ~ emotion_comm.c*as.factor(country_cat)+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))

#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 2.13E-03

# Robust lmm
model1.1 <- lme(totalPTSD ~ emotion_comm.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD ~ emotion_comm.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Check moderating influence of family enviro, SES, country
model1.hos <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*PARQhostile.c*+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*PARQwarmth.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*totalFES.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*income_famsize.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*par_ed_ord+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Stress <- with(miData,lme4::lmer(totalPTSD ~ emotion_comm.c*as.factor(country_cat)+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))

#self_comm as predictor of SDQ emo change since COVID
model1 <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*SDQemo.c+self_comm.c*ch_age.c+self_comm.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*ch_age.c*ch_gender_num+self_comm.c*ch_age.c*SDQemo.c+self_comm.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best, p 2.05E-04

model1.hos <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*PARQwarmth.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*totalFES.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*DASSStress.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Distress <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*totalcov_dist.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Distress))
model1.Distress <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*income_famsize.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Distress))
model1.Distress <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*par_ed_ord+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Distress))
model1.Distress <- with(miData,lme4::lmer(SDQemo2 ~ self_comm.c*as.factor(country_cat)+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Distress))

# Robust lmm
model1.1 <- lme(SDQemo2 ~ self_comm.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ self_comm.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#self_comm as predictor of SDQ hyp change since COVID
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ self_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQhyp2 ~ self_comm.c*SDQhyp.c+self_comm.c*ch_age.c+self_comm.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQhyp2 ~ self_comm.c*ch_age.c*ch_gender_num+self_comm.c*ch_age.c*SDQhyp.c+self_comm.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQhyp2 ~ self_comm.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 4.33E-03

#Moderating role of SES, country
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ income_famsize.c*self_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ par_ed_ord*self_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQhyp2 ~ as.factor(country_cat)*self_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1.hos <- with(miData,lme4::lmer(SDQhyp2 ~ self_comm.c*PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lme4::lmer(SDQhyp2 ~ self_comm.c*PARQwarmth.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lme4::lmer(SDQhyp2 ~ self_comm.c*totalFES.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lme4::lmer(SDQhyp2 ~ self_comm.c*DASSStress.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Distress <- with(miData,lme4::lmer(SDQhyp2 ~ self_comm.c*totalcov_dist.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Distress))

# Robust lmm
model1.1 <- lme(SDQemo2 ~ self_comm.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ self_comm.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#self_comm as predictor of SDQ con change since COVID
model1 <- with(miData,lme4::lmer(SDQcon2 ~ self_comm.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQcon2 ~ self_comm.c*SDQcon.c+self_comm.c*ch_age.c+self_comm.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQcon2 ~ self_comm.c*ch_age.c*ch_gender_num+self_comm.c*ch_age.c*SDQcon.c+self_comm.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQcon2 ~ self_comm.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1 best fit, p 0.006

#Moderating effect of SES, country
model1 <- with(miData,lme4::lmer(SDQcon2 ~ income_famsize.c*self_comm.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQcon2 ~ par_ed_ord*self_comm.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(SDQcon2 ~ as.factor(country_cat)*self_comm.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1.hos <- with(miData,lme4::lmer(SDQcon2 ~ self_comm.c*PARQhostile.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lme4::lmer(SDQcon2 ~ self_comm.c*PARQwarmth.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lme4::lmer(SDQcon2 ~ self_comm.c*totalFES.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lme4::lmer(SDQcon2 ~ self_comm.c*DASSStress.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Distress <- with(miData,lme4::lmer(SDQcon2 ~ self_comm.c*totalcov_dist.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Distress))

# Robust lmm
model1.1 <- lme(SDQcon2 ~ self_comm.c+SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQcon2 ~ self_comm.c+SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#self_comm as predictor of SDQ peer change since COVID
model1 <- with(miData,lme4::lmer(SDQpeer2 ~ self_comm.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(SDQpeer2 ~ self_comm.c*SDQpeer.c++self_comm.c*ch_age.c+self_comm.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(SDQpeer2 ~ self_comm.c*ch_age.c*ch_gender_num+self_comm.c*ch_age.c*SDQpeer.c+self_comm.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lme4::lmer(SDQpeer2 ~ self_comm.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))

anova(model1,model2)
anova(model2,model3)
anova(model3,model4)

# Model 1, not sig


#PTSD as outcome
model1 <- with(miData,lme4::lmer(totalPTSD ~ self_comm.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lme4::lmer(totalPTSD ~ self_comm.c*ch_age.c+self_comm.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lme4::lmer(totalPTSD ~ self_comm.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best, p 4.06E-12

#Moderating role of SES, country
model1 <- with(miData,lme4::lmer(totalPTSD ~ income_famsize.c*self_comm.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ par_ed_ord*self_comm.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1 <- with(miData,lme4::lmer(totalPTSD ~ as.factor(country_cat)*self_comm.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model1.hos <- with(miData,lme4::lmer(totalPTSD ~ self_comm.c*PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lme4::lmer(totalPTSD ~ self_comm.c*PARQwarmth.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lme4::lmer(totalPTSD ~ self_comm.c*totalFES.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lme4::lmer(totalPTSD ~ self_comm.c*DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))
model1.Distress <- with(miData,lme4::lmer(totalPTSD ~ self_comm.c*totalcov_dist.c+par_age+ch_age.c+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Distress))

# Robust LMM
model1.1 <- lme(totalPTSD ~ self_comm.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD ~ self_comm.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize.c+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

