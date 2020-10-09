##Runs significant analyses (robust lmm) with only those participants currently under stay-at-home orders.

covid_data_child_all <- read.csv("scored_data/covid_data_child_scored.csv", header=TRUE, stringsAsFactors = FALSE)

library(lmerTest) #v3.1-2
library(robustlmm) #v2.3
library(reghelper) #v0.3.6
library(ggplot2) #v3.3.1
library(lme4) #v1.1-23
library(nlme) #v3.1-147

#Create dataset with only those partcipants currently under stay at home orders
covid_data_child_all$home<- covid_data_child_all$stay_home == "Yes" & covid_data_child_all$leave_home == "No"
covid_data_child_all<-covid_data_child_all[!(covid_data_child_all$home=="FALSE"),]

model1.1 <- lme(SDQhyp2 ~ PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num, random=~1 | id, method="ML", data = covid_data_child_all, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ PARQhostile.c+SDQhyp.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num + (1 | id), REML = FALSE, data = covid_data_child_all, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model2.1 <- lme(SDQcon2 ~ PARQhostile.c*SDQcon.c+PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child_all, na.action = na.omit)
model2.robust <- rlmer(SDQcon2 ~ PARQhostile.c*SDQcon.c+PARQhostile.c*ch_age.c+PARQhostile.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child_all, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(totalPTSD.c ~ PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child_all, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ PARQhostile.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child_all, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQemo2 ~ DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child_all, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ DASSDep.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child_all, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(totalPTSD.c ~ DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child_all, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ DASSDep.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child_all, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQemo2 ~ DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQemo2 ~ DASSAnx.c+SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(totalPTSD.c ~ DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ DASSAnx.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model2.1 <- lme(SDQemo2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+DASSStress.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQemo2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+DASSStress.c*SDQemo.c+par_age+ch_age.c+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model2.1 <- lme(SDQhyp2 ~ DASSStress.c*ch_gender_num+DASSStress.c*SDQhyp.c+DASSStress.c*ch_age.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQhyp2 ~ DASSStress.c*ch_gender_num+DASSStress.c*SDQhyp.c+DASSStress.c*ch_age.c+SDQhyp.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model2.1 <- lme(SDQcon2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+DASSStress.c*SDQcon.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQhyp2 ~ DASSStress.c*ch_age.c+DASSStress.c*ch_gender_num+DASSStress.c*SDQcon.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQpeer2 ~ DASSStress.c+SDQpeer.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ DASSStress.c+SDQpeer.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(totalPTSD ~ DASSStress.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD ~ DASSStress.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model2.1 <- lme(SDQemo2 ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+totalcov_dist.c*SDQemo.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQemo2 ~ totalcov_dist.c*ch_age.c+totalcov_dist.c*ch_gender_num+totalcov_dist.c*SDQemo.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ totalcov_dist.c+SDQhyp.c+ch_age.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQcon2 ~ totalcov_dist.c+ch_age.c+SDQcon.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQcon2 ~ totalcov_dist.c+ch_age.c+SDQcon.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQpeer2 ~ totalcov_dist.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ totalcov_dist.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(totalPTSD.c ~ totalcov_dist.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ totalcov_dist.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQhyp2 ~ covid_pos_num.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQhyp2 ~ covid_pos_num.c+SDQhyp.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model2.1 <- lme(SDQhyp2 ~ covid_pos_num.c*SDQcon.c+covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model2.robust <- rlmer(SDQhyp2 ~ covid_pos_num.c*SDQcon.c+covid_pos_num.c*ch_age.c+covid_pos_num.c*ch_gender_num+SDQcon.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model2.1)))
coefs.robust <- coef(summary(model2.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQpeer2 ~ covid_pos_num.c+ch_age.c+ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ covid_pos_num.c+ch_age.c+ch_gender_num+SDQpeer.c+par_age+par_ed_ord+income_famsize+as.factor(country_cat)++par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(totalPTSD.c ~ covid_pos_num.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ covid_pos_num.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(totalPTSD.c ~ PARQwarmth.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ PARQwarmth.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(SDQpeer2 ~ totalFES.c+ch_age.c+SDQpeer.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(SDQpeer2 ~ totalFES.c+SDQpeer.c+ch_age.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values

model1.1 <- lme(totalPTSD.c ~ totalFES.c+ch_age.c+totalFES.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num, random=~1 | id, method="ML", data = covid_data_child, na.action = na.omit)
model1.robust <- rlmer(totalPTSD.c ~ totalFES.c+ch_age.c+totalFES.c+ch_gender_num+par_age+par_ed_ord+income_famsize+as.factor(country_cat)+ch_gender_num+par_gender_num + (1 | id), REML = FALSE, data = covid_data_child, na.action = na.omit)
coefs <- data.frame(coef(summary(model1.1)))
coefs.robust <- coef(summary(model1.robust))
p.values <- 2*pt(abs(coefs.robust[,3]), coefs$DF, lower=FALSE)
p.values