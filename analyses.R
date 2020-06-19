#set to appropriate working directory
setwd("/Users/sarah/Dropbox/2020/COVID_parenting/COVID_script/")

# covid_data_child_scored.csv for robust lmm
covid_data_child <- read.csv("covid_data_child_scored.csv", header=TRUE, stringsAsFactors = FALSE)
# miData created from covid_missing.R script for MI analyses


#install.packages("robustlmm")
#install.packages("lmerTest")
#install.packages("lmeresampler")
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

#Create separate datasets for males and females for any post hoc tests
data_female <- subset(covid_data_child, covid_data_child$ch_gender_num==1)
data_male <- subset(covid_data_child, covid_data_child$ch_gender_num==2)
    
#check parent past disorder association with key variables
model <- with(miData,lmer(totalSDQ2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(totalSDQ ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(totalPARQ ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(PARQhostile ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(PARQwarmth ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(totalcov_dist ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(totalPTSD ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(DASSDep ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(DASSAnx ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(DASSStress ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(facts_comm ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(emotion_comm ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(self_comm ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(SDQemo2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(SDQhyp2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(SDQcon2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(SDQpeer2 ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(totalPTSD ~ par_past_mh_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
#parent past disorder is associated with SDQ, SDQ2, PTSD, DASS and covid diDASSStress, but not PARQ vbls
#consider adding as a covariate in models


#check if knowing someone hospitalised from COVID is related to key vbls
model <- with(miData,lmer(totalSDQ2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(totalSDQ ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(totalPARQ ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(PARQhostile ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(PARQwarmth ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(totalcov_dist ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(covid_pos_num ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(DASSDep ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(DASSAnx ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(DASSStress ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(facts_comm ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(emotion_comm ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(self_comm ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(SDQemo2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(SDQhyp2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(SDQcon2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(SDQpeer2 ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
model <- with(miData,lmer(totalPTSD ~ other_hosp_num , random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model)
#knowing someone hospitalised (n=57) associated with DASSDep, DASSAnx and total_cov_diDASSStress


model1 <- with(miData,lmer(SDQemo2 ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(SDQhyp2 ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(SDQcon2 ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(SDQpeer2 ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(totalPTSD ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(PARQhostile ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(PARQwarmth ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(totalFES ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(facts_comm ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(emotion_comm ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(self_comm ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(totalcov_dist ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(covid_pos_num ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(DASSDep ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(DASSAnx ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)
model1 <- with(miData,lmer(DASSStress ~ country_cat, random=~1 | id, method="ML", data = covid_data_child, na.action = na.exclude)
summary(pool(model1)


#Series of mixed models below - a number of predictors & 5 outcome variables for each (SDQchange total, emo, con, hyp, peer subscales; PTSD)
#Models 1-3 test main effects (#1), 2-way child age/gender moderation (#2), and 3-way age/gender moderation (#3)
#Models 4-6 only for SDQchange outcomes and test the moderating effect of 'regular' SDQ scores

#PARQ hostile as predictor of SDQ emo change since COVID
model1 <- lme(SDQemo2 ~ PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
summary(model1)

# test normality of resisuals
r<-residuals(model1, type = c("response", "pearson", "normalized"))
shapiro.test(r)
# Note that residuals are not normally distributed. I have checked a number of others and same deal. As such, procede with robust lmm,
# as a supplement to results from multiple imputation

#ROBUST ESTIMATOR
model1.robust <- rlmer(SDQemo2 ~ PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
summary(model1.robust)

# p values as described here: https://stats.stackexchange.com/questions/430490/obtaining-p-values-in-a-robustlmm-mixed-model-via-satterthwaite-approximated-dfs
# get coefficients from non-robust model to extract Satterthwaite approximated DFs
coefs <- data.frame(coef(summary(model1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# MI
model1 <- with(miData,lmer(SDQemo2 ~ PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1|id), REML = FALSE))
summary(pool(model1))

#bootstrap function for lmer (boot)
#model1 <- lmer(SDQemo2 ~ PARQhostile.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num + (1|id), data = covid_data_child, REML = FALSE, na.action = na.exclude)
#summary(pool(model1)
#model1boot <- bootMer(model1, FUN = fixef, nsim = 100, verbose = T)
#boot.ci(model1boot, index = 2, type=c("norm", "basic", "perc"), conf = c(0.95, 0.99, 0.999))

model2 <- with(miData,lmer(SDQemo2 ~ PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ PARQhostile.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ PARQhostile.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ PARQhostile.c*ch_age.c*SDQemo.c+PARQhostile.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ PARQhostile.c*ch_age.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

#Model 1, PARQhostile p 6.453058e-02

#PARQ hostile as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
#rlmer
model1.1 <- lme(SDQhyp2 ~ PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model2 <- with(miData,lmer(SDQhyp2 ~ PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ PARQhostile.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ PARQhostile.c*SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ PARQhostile.c*ch_age.c*SDQhyp.c+PARQhostile.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ PARQhostile.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model1 best fit - PARQhostile significant main effect 3.969642e-02

ggplot(covid_data_child, aes(x = PARQhostile.c, y = SDQhyp) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#PARQ hostile as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ PARQhostile.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ PARQhostile.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ PARQhostile.c*SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ PARQhostile.c*ch_age.c*SDQcon.c+PARQhostile.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ PARQhostile.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 4 best fit MI p 0.001065497

simple_slopes(model4)
graph_model(model4, y=SDQcon2, x=PARQhostile.c, lines=SDQcon.c)

# Robust lmm for model 4
model4.1 <- lme(SDQcon2 ~ PARQhostile.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model4.robust <- rlmer(SDQcon2 ~ PARQhostile.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model4.1)))
coefs.robust <- coef(summary(model4.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#PARQ hostile as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ PARQhostile.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+country_cat+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ PARQhostile.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ PARQhostile.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ PARQhostile.c*ch_age.c*SDQpeer.c+PARQhostile.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ PARQhostile.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

#Model 1 best fit, PARQhostile not sig p 0.6114275

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ PARQhostile.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

#model 1 best fit p 6.971046e-11, plot association
ggplot(covid_data_child, aes(x = PARQhostile.c, y = totalPTSD.c) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Robust lmm for model 1
model1.1 <- lme(totalPTSD.c ~ PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#DASS DASSDep as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ DASSDep.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ DASSDep.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ DASSDep.c*ch_age.c*SDQemo.c+DASSDep.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ DASSDep.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit p 3.627651e-03

model1.1 <- lme(SDQemo2 ~ DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#DASS DASSDep as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ DASSDep.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ DASSDep.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ DASSDep.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ DASSDep.c*ch_age.c*SDQhyp.c+DASSDep.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ DASSDep.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit p 0.037058

# Robust lmm
model1.1 <- lme(SDQhyp2 ~ DASSDep.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ DASSDep.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#DASS DASSDep as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ DASSDep.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ DASSDep.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ DASSDep.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ DASSDep.c*ch_age.c*SDQcon.c+DASSDep.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ DASSDep.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 1.333369e-02

# Robust lmm
model1.1 <- lme(SDQcon2 ~ DASSDep.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQcon2 ~ DASSDep.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#DASS DASSDep as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ DASSDep.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ DASSDep.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ DASSDep.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ DASSDep.c*ch_age.c*SDQpeer.c+DASSDep.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ DASSDep.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 0.0208791

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ DASSDep.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ DASSDep.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ DASSDep.c*ch_age.c+DASSDep.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ DASSDep.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 2.666524e-07

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

ggplot(covid_data_child, aes(x = DASSDep.c, y = totalPTSD) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#DASS DASSAnx as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ DASSAnx.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ DASSAnx.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ DASSAnx.c*ch_age.c*SDQemo.c+DASSAnx.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ DASSAnx.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 4 best fit, p for interaction 0.04066

# Robust lmm
model4.1 <- lme(SDQemo2 ~ DASSAnx.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model4.robust <- rlmer(SDQemo2 ~ DASSAnx.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model4.1)))
coefs.robust <- coef(summary(model4.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

simple_slopes(model4)

graph_model(model6, y=SDQemo2, x=DASSAnx.c, lines=SDQemo.c)

#DASS DASSAnx as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ DASSAnx.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ DASSAnx.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ DASSAnx.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ DASSAnx.c*ch_age.c*SDQhyp.c+DASSAnx.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ DASSAnx.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 0.12734

#DASS DASSAnx as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ DASSAnx.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ DASSAnx.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ DASSAnx.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ DASSAnx.c*ch_age.c*SDQcon.c+DASSAnx.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ DASSAnx.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 1.385058e-01

#DASS DASSAnx as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ DASSAnx.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ DASSAnx.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ DASSAnx.c*ch_age.c*SDQpeer.c+DASSAnx.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ DASSAnx.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 0.01311029

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ DASSAnx.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ DASSAnx.c*ch_age.c+DASSAnx.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ DASSAnx.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 0.0000006

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

ggplot(covid_data_child, aes(x = DASSAnx.c, y = totalPTSD) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#DASS DASSStress as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ DASSStress.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ DASSStress.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ DASSStress.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ DASSStress.c*ch_age.c*SDQemo.c+DASSStress.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ DASSStress.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 4 best fit, p for interaction 8.953841e-03

# Robust lmm
model4.1 <- lme(SDQemo2 ~ DASSStress.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model4.robust <- rlmer(SDQemo2 ~ DASSStress.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model4.1)))
coefs.robust <- coef(summary(model4.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

simple_slopes(model4)

graph_model(model4, y=SDQemo2, x=DASSStress.c, lines=SDQemo.c)

#DASS DASSStress as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ DASSStress.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ DASSStress.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ DASSStress.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ DASSStress.c*ch_age.c*SDQhyp.c+DASSStress.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ DASSStress.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 2 best fit, Stress*age p 3.318461e-03

# Robust lmm
model2.1 <- lme(SDQhyp2 ~ DASSStress.c*ch_age.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQhyp2 ~ DASSStress.c*ch_age.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

simple_slopes(model2.1)

graph_model(model2.1, y=SDQemo2, x=DASSStress.c, lines=ch_age.c)


#DASS DASSStress as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ DASSStress.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ DASSStress.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ DASSStress.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ DASSStress.c*ch_age.c*SDQcon.c+DASSStress.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ DASSStress.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 4 best, p 0.0017819492

# Robust lmm
model4.1 <- lme(SDQcon2 ~ DASSStress.c*SDQcon.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model4.robust <- rlmer(SDQhyp2 ~ DASSStress.c*SDQcon.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model4.1)))
coefs.robust <- coef(summary(model4.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

simple_slopes(model4)

graph_model(model4, y=SDQcon2, x=DASSStress.c, lines=SDQcon.c)

#DASS DASSStress as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ DASSStress.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ DASSStress.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ DASSStress.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ DASSStress.c*ch_age.c*SDQpeer.c+DASSStress.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ DASSStress.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 0.006766733

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ DASSStress.c+SDQpeer.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ DASSStress.c+SDQpeer.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ DASSStress.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ DASSStress.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ DASSStress.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Model 1 best fit, p 4.036408e-08

ggplot(covid_data_child, aes(x = DASSStress.c, y = totalPTSD) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#totalcov_dist as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ totalcov_dist.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ totalcov_dist.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ totalcov_dist.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ totalcov_dist.c*ch_age.c*SDQemo.c+totalcov_dist.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ totalcov_dist.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 4 best fit, interaction p 1.094840e-03

# Robust lmm
model4.1 <- lme(SDQemo2 ~ totalcov_dist.c*SDQemo.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model4.robust <- rlmer(SDQemo2 ~ totalcov_dist.c*SDQemo.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model4.1)))
coefs.robust <- coef(summary(model4.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#Controlling for DASS Stress, sig 7.354175e-04
model4.Stress <- with(miData,lmer(SDQemo2 ~ totalcov_dist.c*SDQemo.c+DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4.Stress))

simple_slopes(model4.1)

graph_model(model4.1, y=SDQemo2, x=totalcov_dist.c, lines=SDQemo.c)

#totalcov_dist as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ totalcov_dist.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ totalcov_dist.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ totalcov_dist.c*ch_age.c*SDQhyp.c+totalcov_dist.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ totalcov_dist.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 1.414464e-05

# Robust lmm
model1.1 <- lme(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# controlling for DASS Stress, sig 7.444949e-03
model1.Stress <- with(miData,lmer(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))

ggplot(covid_data_child, aes(x = totalcov_dist.c, y = SDQhyp2) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#totalcov_dist as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ totalcov_dist.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ totalcov_dist.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ totalcov_dist.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ totalcov_dist.c*ch_age.c*SDQcon.c+totalcov_dist.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ totalcov_dist.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 5 best. distress*con*gender p 0.0046098819

# Robust lmm
model5.1 <- lme(SDQcon2 ~ totalcov_dist.c*ch_age.c*SDQcon.c+totalcov_dist.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model5.robust <- rlmer(SDQcon2 ~ totalcov_dist.c*ch_age.c*SDQcon.c+totalcov_dist.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model5.1)))
coefs.robust <- coef(summary(model5.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Robust for 5 not sig, Robust lmm for 4
model4.1 <- lme(SDQcon2 ~ totalcov_dist.c*SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model4.robust <- rlmer(SDQcon2 ~ totalcov_dist.c*SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model4.1)))
coefs.robust <- coef(summary(model4.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Controlling for DASS Stress, both model 4 and 5 remain sig
model4.Stress <- with(miData,lmer(SDQcon2 ~ totalcov_dist.c*SDQcon.c+DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4.Stress))
model5.Stress <- with(miData,lmer(SDQcon2 ~ totalcov_dist.c*ch_age.c*SDQcon.c+DASSStress.c+totalcov_dist.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5.Stress))

simple_slopes(model4.1)
graph_model(model4.1, y=SDQcon2, x=totalcov_dist.c, lines=SDQcon.c)


#totalcov_dist as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ totalcov_dist.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ totalcov_dist.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ totalcov_dist.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ totalcov_dist.c*ch_age.c*SDQpeer.c+totalcov_dist.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ totalcov_dist.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 0.001923232

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ totalcov_dist.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ totalcov_dist.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Controlling for DASS Stress, not sig 0.05635777
model1.Stress <- with(miData,lmer(SDQpeer2 ~ totalcov_dist.c+DASSStress.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))

ggplot(covid_data_child, aes(x = totalcov_dist.c, y = SDQpeer2) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ totalcov_dist.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ totalcov_dist.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 1.968611e-09

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ totalcov_dist.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ totalcov_dist.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Controlling for DASS Stress, sig 1.874548e-04
model1.Stress <- with(miData,lmer(totalPTSD.c ~ totalcov_dist.c+DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))

ggplot(covid_data_child, aes(x = totalcov_dist.c, y = totalPTSD) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#covid_pos_num as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ covid_pos_num.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ covid_pos_num.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ covid_pos_num.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ covid_pos_num.c*ch_age.c*SDQemo.c+covid_pos_num.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ covid_pos_num.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 0.0345

# Robust lmm
model1.1 <- lme(SDQemo2 ~ covid_pos_num.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ covid_pos_num.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

ggplot(covid_data_child, aes(x = covid_pos_num.c, y = SDQemo2) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#covid_pos_num as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ covid_pos_num.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ covid_pos_num.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ covid_pos_num.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ covid_pos_num.c*ch_age.c*SDQhyp.c+covid_pos_num.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ covid_pos_num.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 2.978049e-02

# Robust lmm
model1.1 <- lme(SDQhyp2 ~ covid_pos_num.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ covid_pos_num.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

ggplot(covid_data_child, aes(x = covid_pos_num.c, y = SDQhyp2) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#covid_pos_num as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ covid_pos_num.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ covid_pos_num.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ covid_pos_num.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ covid_pos_num.c*ch_age.c*SDQcon.c+covid_pos_num.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ covid_pos_num.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, but covid_pos not sig

ggplot(covid_data_child, aes(x = covid_pos_num.c, y = SDQcon2) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#covid_pos_num as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ covid_pos_num.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ covid_pos_num.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ covid_pos_num.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ covid_pos_num.c*ch_age.c*SDQpeer.c+covid_pos_num.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ covid_pos_num.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 2 best fit, covid_pos * age p 0.01372671

# Robust lmm
model2.1 <- lme(SDQpeer2 ~ covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQpeer2 ~ covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Probe interaction
simple_slopes(model2.1)

graph_model(model2.1, y=SDQpeer2, x=covid_pos_num.c, lines=ch_age.c)

# Plot simple association for model 1
ggplot(covid_data_child, aes(x = covid_pos_num.c, y = SDQpeer2) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ covid_pos_num.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ covid_pos_num.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 3.394400e-02

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ covid_pos_num.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ covid_pos_num.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


#PARQwarmth as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ PARQwarmth.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ PARQwarmth.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ PARQwarmth.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ PARQwarmth.c*ch_age.c*SDQemo.c+PARQwarmth.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ PARQwarmth.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit but warmth not sig

#PARQwarmth as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ PARQwarmth.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ PARQwarmth.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ PARQwarmth.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ PARQwarmth.c*ch_age.c*SDQhyp.c+PARQwarmth.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ PARQwarmth.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit but warmth not sig

#PARQwarmth as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ PARQwarmth.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ PARQwarmth.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ PARQwarmth.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ PARQwarmth.c*ch_age.c*SDQcon.c+PARQwarmth.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ PARQwarmth.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 4 best, p 0.0235260682

# Robust lmm
model4.1 <- lme(SDQcon2 ~ PARQwarmth.c*SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model4.robust <- rlmer(SDQcon2 ~ PARQwarmth.c*SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model4.1)))
coefs.robust <- coef(summary(model4.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

simple_slopes(model4.1)

graph_model(model4.1, y=SDQcon2, x=PARQwarmth.c, lines=SDQcon.c)

#PARQwarmth as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ PARQwarmth.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ PARQwarmth.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ PARQwarmth.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ PARQwarmth.c*ch_age.c*SDQpeer.c+PARQwarmth.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ PARQwarmth.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, not sig

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ PARQwarmth.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ PARQwarmth.c*ch_age.c+PARQwarmth.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ PARQwarmth.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 1.225682e-06

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ PARQwarmth.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ PARQwarmth.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

ggplot(covid_data_child, aes(x = PARQwarmth.c, y = totalPTSD) ) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


#totalFES as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ totalFES.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ totalFES.c*ch_age.c+totalFES.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ totalFES.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ totalFES.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ totalFES.c*ch_age.c*SDQemo.c+totalFES.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ totalFES.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best, p 4.691009e-02, Roust not sig p .08

# Robust lmm
model1.1 <- lme(SDQemo2 ~ totalFES.c+ch_age.c+SDQemo.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ totalFES.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#totalFES as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ totalFES.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ totalFES.c*ch_age.c+totalFES.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ totalFES.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ totalFES.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ totalFES.c*ch_age.c*SDQhyp.c+totalFES.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ totalFES.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

#totalFES as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ totalFES.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ totalFES.c*ch_age.c+totalFES.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ totalFES.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ totalFES.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ totalFES.c*ch_age.c*SDQcon.c+totalFES.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ totalFES.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best, p 0.0273, not sig w robust

# Robust lmm
model1.1 <- lme(SDQcon2 ~ totalFES.c+ch_age.c+SDQcon.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQcon2 ~ totalFES.c+SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#totalFES as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ totalFES.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ totalFES.c*ch_age.c+totalFES.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ totalFES.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ totalFES.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ totalFES.c*ch_age.c*SDQpeer.c+totalFES.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ totalFES.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ totalFES.c+ch_age.c+SDQpeer.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ totalFES.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ totalFES.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ totalFES.c*ch_age.c+totalFES.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ totalFES.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 3 best fit, p 0.0000012

# Robust lmm
model3.1 <- lme(totalPTSD.c ~ totalFES.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model3.robust <- rlmer(totalPTSD.c ~ totalFES.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model3.1)))
coefs.robust <- coef(summary(model3.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Check FES * age in males and females
model2.f <- lme(totalPTSD.c ~ totalFES.c*ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+par_gender_num, random=~1 | id, data = data_female, method="ML", na.action = na.omit)
summary(model2.f)
model2.m <- lme(totalPTSD.c ~ totalFES.c*ch_age.c+par_age+par_ed_ord+income_famsize+country_cat+par_gender_num, random=~1 | id, data = data_male, method="ML", na.action = na.omit)
summary(model2.m)

# Probe interaction in males
simple_slopes(model2.m)
graph_model(model2.m, y=totalPTSD.c, x=totalFES.c, lines=ch_age.c)

#COVID communication variables as predictors: communication about facts, emotions and self-focussed


#facts_comm as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ facts_comm.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ facts_comm.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ facts_comm.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ facts_comm.c*ch_age.c*SDQemo.c+facts_comm.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ facts_comm.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

#facts_comm as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ facts_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ facts_comm.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ facts_comm.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ facts_comm.c*ch_age.c*SDQhyp.c+facts_comm.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ facts_comm.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

#facts_comm as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ facts_comm.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ facts_comm.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ facts_comm.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ facts_comm.c*ch_age.c*SDQcon.c+facts_comm.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ facts_comm.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

#facts_comm as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ facts_comm.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ facts_comm.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ facts_comm.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ facts_comm.c*ch_age.c*SDQpeer.c+facts_comm.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ facts_comm.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD ~ facts_comm.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD ~ facts_comm.c*ch_age.c+facts_comm.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD ~ facts_comm.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

#Communication about emotional content (child, self, others) + reassurance

#emotion_comm as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ emotion_comm.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ emotion_comm.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ emotion_comm.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ emotion_comm.c*ch_age.c*SDQemo.c+emotion_comm.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ emotion_comm.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

#model 4 best fit

# Robust lmm
model4.1 <- lme(SDQemo2 ~ emotion_comm.c*SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model4.robust <- rlmer(SDQemo2 ~ emotion_comm.c*SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model4.1)))
coefs.robust <- coef(summary(model4.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

simple_slopes(model4.1)

#more communications about emotion associated with increase in emo problems for those with high levels to begin with
graph_model(model4.1, y=SDQemo2, x=emotion_comm.c, lines=SDQemo.c)

#emotion_comm as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c*ch_age.c*SDQhyp.c+emotion_comm.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best, p .008

# Robust lmm
model1.1 <- lme(SDQhyp2 ~ emotion_comm.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ emotion_comm.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Check moderating influence of family enviro
model1.hos <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c*PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c*PARQwarmth.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c*totalFES.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lmer(SDQhyp2 ~ emotion_comm.c*DASSStress.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))

#emotion_comm as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ emotion_comm.c*totalFES.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ emotion_comm.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ emotion_comm.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ emotion_comm.c*ch_age.c*SDQcon.c+emotion_comm.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ emotion_comm.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 5, con * age p 0.02628524, Robust not sig, so bet fit is model 1, but not sig

# Robust lmm
model5.1 <- lme(SDQcon2 ~ emotion_comm.c*SDQcon.c*ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model5.robust <- rlmer(SDQcon2 ~ emotion_comm.c*SDQcon.c*ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model5.1)))
coefs.robust <- coef(summary(model5.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#emotion_comm as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c*ch_age.c*SDQpeer.c+emotion_comm.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 0.01700765

# Robust lmm
model1.1 <- lme(SDQpeer2 ~ emotion_comm.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ emotion_comm.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Check moderating influence of family enviro
model1.hos <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c*PARQhostile.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c*PARQwarmth.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c*totalFES.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lmer(SDQpeer2 ~ emotion_comm.c*DASSStress.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ emotion_comm.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ emotion_comm.c*ch_age.c+emotion_comm.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ emotion_comm.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best fit, p 9.86E-04

# Robust lmm
model1.1 <- lme(totalPTSD.c ~ emotion_comm.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ emotion_comm.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

# Check moderating influence of family enviro
model1.hos <- with(miData,lmer(totalPTSD.c ~ emotion_comm.c*PARQhostile.c*+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.hos))
model1.war <- with(miData,lmer(totalPTSD.c ~ emotion_comm.c*PARQwarmth.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.war))
model1.fes <- with(miData,lmer(totalPTSD.c ~ emotion_comm.c*totalFES.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.fes))
model1.Stress <- with(miData,lmer(totalPTSD.c ~ emotion_comm.c*DASSStress.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1.Stress))

#self_comm as predictor of SDQ emo change since COVID
model1 <- with(miData,lmer(SDQemo2 ~ self_comm.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQemo2 ~ self_comm.c*ch_age.c+self_comm.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQemo2 ~ self_comm.c*ch_age.c*ch_gender_num+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQemo2 ~ self_comm.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQemo2 ~ self_comm.c*ch_age.c*SDQemo.c+self_comm.c*ch_gender_num*SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQemo2 ~ self_comm.c*ch_age.c*ch_gender_num*SDQemo.c+SDQemo.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best, p 1.862165e-05

# Robust lmm
model1.1 <- lme(SDQemo2 ~ self_comm.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ self_comm.c+SDQemo.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#self_comm as predictor of SDQ hyp change since COVID
model1 <- with(miData,lmer(SDQhyp2 ~ self_comm.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQhyp2 ~ self_comm.c*ch_age.c+self_comm.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQhyp2 ~ self_comm.c*ch_age.c*ch_gender_num+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQhyp2 ~ self_comm.c*SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQhyp2 ~ self_comm.c*ch_age.c*SDQhyp.c+self_comm.c*ch_gender_num*SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQhyp2 ~ self_comm.c*ch_age.c*ch_gender_num*SDQhyp.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 1 best fit, p 4.507472e-03

# Robust lmm
model1.1 <- lme(SDQemo2 ~ self_comm.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ self_comm.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#self_comm as predictor of SDQ con change since COVID
model1 <- with(miData,lmer(SDQcon2 ~ self_comm.c+SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQcon2 ~ self_comm.c*ch_age.c+self_comm.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQcon2 ~ self_comm.c*ch_age.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQcon2 ~ self_comm.c*SDQcon.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQcon2 ~ self_comm.c*ch_age.c*SDQcon.c+self_comm.c*ch_gender_num*SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQcon2 ~ self_comm.c*ch_age.c*ch_gender_num*SDQcon.c+SDQcon.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

# Model 4 best fit, p 0.007167291, Robust not sig

# Robust lmm
model4.1 <- lme(SDQcon2 ~ self_comm.c*SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model4.robust <- rlmer(SDQcon2 ~ self_comm.c*SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model4.1)))
coefs.robust <- coef(summary(model4.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQcon2 ~ self_comm.c+SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQcon2 ~ self_comm.c+SDQcon.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

#self_comm as predictor of SDQ peer change since COVID
model1 <- with(miData,lmer(SDQpeer2 ~ self_comm.c+SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(SDQpeer2 ~ self_comm.c*ch_age.c+self_comm.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(SDQpeer2 ~ self_comm.c*ch_age.c*ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))
model4 <- with(miData,lmer(SDQpeer2 ~ self_comm.c*SDQpeer.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model4))
model5 <- with(miData,lmer(SDQpeer2 ~ self_comm.c*ch_age.c*SDQpeer.c+self_comm.c*ch_gender_num*SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model5))
model6 <- with(miData,lmer(SDQpeer2 ~ self_comm.c*ch_age.c*ch_gender_num*SDQpeer.c+SDQpeer.c+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model6))

anova(model1,model2)
anova(model2,model3)
anova(model1,model4)
anova(model4,model5)
anova(model5,model6)

#PTSD as outcome
model1 <- with(miData,lmer(totalPTSD.c ~ self_comm.c+par_age+ch_age.c+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model1))
model2 <- with(miData,lmer(totalPTSD.c ~ self_comm.c*ch_age.c+self_comm.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model2))
model3 <- with(miData,lmer(totalPTSD.c ~ self_comm.c*ch_age.c*ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE))
summary(pool(model3))

anova(model1,model2)
anova(model2,model3)

# Model 1 best, p 1.92E-12

# Robust LMM
model1.1 <- lme(totalPTSD.c ~ self_comm.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ self_comm.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+country_cat+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values


