#covid_data_child <- read.csv("covid_data_child_scored.csv", header=TRUE, stringsAsFactors = FALSE)

install.packages("interactions")
library(interactions)
library(ggplot2)

#theme_set(
# theme_bw() 
#)

#Change settings regarding theme here
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        strip.background = element_rect(fill="white"))
#, legend.title=element_blank())

#covid distress

ggplot(covid_data_child, aes(x = totalcov_dist, y = SDQhyp2)) +
geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Total COVID Negative Impact", y = "Change in Inattention-Hyperactivity") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = totalcov_dist, y = SDQcon2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Total COVID Negative Impact", y = "Change in Conduct Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = totalcov_dist, y = totalPTSD)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Total COVID Negative Impact", y = "Trauma Symptoms")  + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = totalcov_dist, y = SDQemo2, color = SDQemo_cat, fill = SDQemo_cat)) +
  geom_jitter(size = 1.5, shape = 23) +
  geom_smooth(method = lm) +
  labs(x = "Total COVID Negative Impact", y = "Emotional problems") +
  scale_color_discrete(name="Baseline SDQ\nEmotional Problems",
                           breaks=c("Average", "High", "S_raised"),
                           labels=c("Average", "High", "Slightly Raised")) +
  scale_fill_discrete(name="Baseline SDQ\nEmotional Problems",
                          breaks=c("Average", "High", "S_raised"),
                          labels=c("Average", "High", "Slightly Raised"))

ggplot(covid_data_child, aes(x = covid_pos_num, y = SDQpeer2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Total COVID Positive Impact", y = "Change in Peer Problems")+ apatheme + theme(aspect.ratio = 1)

# DASS

ggplot(covid_data_child, aes(x = DASSDep, y = SDQemo2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Parent Depressive Symptoms", y = "Change in Emotional Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = DASSDep, y = totalPTSD)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Parent Depressive Symptoms", y = "Trauma Symptoms") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = DASSAnx, y = SDQemo2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Parent Anxiety Symptoms", y = "Change in Emotional Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = DASSDep, y = SDQpeer2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Parent Anxiety Symptoms", y = "Change in Peer Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = DASSDep, y = totalPTSD)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Parent Depressive Symptoms", y = "Trauma Symptoms") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = DASSStress, y = SDQpeer2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Parent Stress", y = "Change in Peer Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = DASSStress, y = totalPTSD)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Parent Stress", y = "Trauma Symptoms") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = DASSStress, y = SDQemo2, color = SDQemo_cat, fill = SDQemo_cat)) +
  geom_jitter(size = 1.5, shape = 23) +
  geom_smooth(method = lm) +
  labs(x = "Parent Stress", y = "Change in Emotional problems") +
  scale_color_discrete(name="Baseline SDQ\nEmotional Problems",
                       breaks=c("Average", "High", "S_raised"),
                       labels=c("Average", "High", "Slightly Raised")) +
  scale_fill_discrete(name="Baseline SDQ\nEmotional Problems",
                      breaks=c("Average", "High", "S_raised"),
                      labels=c("Average", "High", "Slightly Raised"))

ggplot(covid_data_child, aes(x = DASSStress, y = SDQcon2, color = SDQcon_cat, fill = SDQcon_cat)) +
  geom_jitter(size = 1.5, shape = 23) +
  geom_smooth(method = lm) +
  labs(x = "Parent Stress", y = "Change in Conduct problems") +
  scale_color_discrete(name="Baseline SDQ\nConduct Problems",
                       breaks=c("Average", "High", "S_raised"),
                       labels=c("Average", "High", "Slightly Raised")) +
  scale_fill_discrete(name="Baseline SDQ\nConduct Problems",
                      breaks=c("Average", "High", "S_raised"),
                      labels=c("Average", "High", "Slightly Raised"))


covid_data_child$ch_age_cat <- as.factor(car::recode(covid_data_child$ch_age, "NA=NA; 0:8='5-8';9:13='9-13'; else='14-17'"))
table(covid_data_child$ch_age_cat, useNA = "ifany")

ggplot(covid_data_child, aes(x = DASSStress, y = SDQhyp2, color = ch_age_cat, fill = ch_age_cat)) +
  geom_jitter(size = 1.5, shape = 23) +
  geom_smooth(method = lm) +
  labs(x = "Parent Stress", y = "Change in Inattention-Hyperactivity Problems") +
  scale_color_discrete(name="Child Age",
                       breaks=c("14-17", "9-13", "5-8"),
                       labels=c("14-17", "9-13", "5-8")) +
  scale_fill_discrete(name="Child Age",
                      breaks=c("14-17", "9-13", "5-8"),
                      labels=c("14-17", "9-13", "5-8"))

# Parenting

ggplot(covid_data_child, aes(x = PARQhostile, y = SDQhyp2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Parental Hostility", y = "Change in Inattention-Hyperactivity") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = PARQhostile, y = totalPTSD)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Parental Hostility", y = "Trauma Symptoms") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = PARQhostile, y = SDQcon2, color = SDQcon_cat, fill = SDQcon_cat)) +
  geom_jitter(size = 1.5, shape = 23) +
  geom_smooth(method = lm) +
  labs(x = "Parental Hostility", y = "Change in Conduct problems") +
  scale_color_discrete(name="Baseline SDQ\nConduct Problems",
                       breaks=c("Average", "High", "S_raised"),
                       labels=c("Average", "High", "Slightly Raised")) +
  scale_fill_discrete(name="Baseline SDQ\nConduct Problems",
                      breaks=c("Average", "High", "S_raised"),
                      labels=c("Average", "High", "Slightly Raised"))

ggplot(covid_data_child, aes(x = PARQwarmth, y = totalPTSD)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Low Parental Warmth", y = "Trauma Symptoms") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = totalFES, y = totalPTSD)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Family Cohesion", y = "Trauma Symptoms") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = totalFES, y = SDQpeer2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Family Cohesion", y = "Change in Peer Problems") + apatheme + theme(aspect.ratio = 1)

# Communication

ggplot(covid_data_child, aes(x = emotion_comm, y = totalPTSD)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Emotion-focused Communication", y = "Trauma Symptoms") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = emotion_comm, y = SDQhyp2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Emotion-focused Communication", y = "Change in Inattention-Hyperactivity Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = emotion_comm, y = SDQemo2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Emotion-focused Communication", y = "Change in Emotional Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = emotion_comm, y = SDQpeer2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Emotion-focused Communication", y = "Change in Peer Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = self_comm, y = SDQemo2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Self-focused Communication", y = "Change in Emotional Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = self_comm, y = SDQcon2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Self-focused Communication", y = "Change in Conduct Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = self_comm, y = SDQhyp2)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Self-focused Communication", y = "Change in Inattention-Hyperactivity Problems") + apatheme + theme(aspect.ratio = 1)

ggplot(covid_data_child, aes(x = self_comm, y = totalPTSD)) +
  geom_jitter(color = "#00CC99", size = 1.5, shape = 23) +
  geom_smooth(color = "#00AFBB", fill = "#00AFBB", method = lm) +
  labs(x = "Self-focused Communication", y = "Trauma Symptoms") + apatheme + theme(aspect.ratio = 1)


