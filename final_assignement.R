setwd ("D://R_course_PhD/Final_project_Horvath_Kata")


library(tidyverse)
library(dplyr)

session_1 <- read.csv("session_1_raw.csv", header = TRUE, sep = ";", na = "NA")
session_2 <- read.csv("session_2_raw.csv", header = TRUE, sep = ";", na = "NA")

#a trial is a predicitve (or anticipatory) error, if the uncorrect response
#equals to the very next stimulus, which is highly frequent based on the sequence.
# not a predictive error: the subject pressed any other button than the correct or
#the coming stimulus, responded too slowly or the next stimulus is a low frequency
#trial. this case is not a real anticipation (you can't learn random events)
#marking of predictive errors were made in excel, can be checked based on the
#retained original values.



data_stim_raw <- bind_rows(session_1, session_2) %>% 
  drop_na()

#EXPLORATORY PART - calculating means by subject and session, plotting trial by trial data, etc.

correct_rt_by_trial <- data_stim_raw %>% 
  filter(first_acc == 1)
ggplot(correct_rt_by_trial) + geom_histogram(aes(x = first_rt, fill = stimulation), binwidth = 2, position = "dodge")+
  labs(x = "Reaction time of individual trials in ms", y = "Distribution of RTs")+
  theme(legend.position = "top")+
  facet_grid(ID~.)
#the plot shows that RT distribution is similar in the two experimental conditions.
#and ressembles the standard ex-gaussian distribution of RTs in all subjects.
#outlier trials are not excluded, however median instead of mean is calculated to
#correct these outlier trials.

pred_error_num <- data_stim_raw %>% 
  filter(first_acc == 0) 
ggplot(pred_error_num) + geom_bar(aes(x = pred_error), position = "dodge")+
  labs(x = "Participants" , y = "Number of predictive errors")+
  facet_grid(.~ID)
#everybody made some predictive errors and that's something!


#calculating means by individuals & stimulation type
means_rt_high <- data_stim_raw %>% 
  group_by(ID, stimulation) %>%
  filter(first_acc == 1) %>% 
  filter(triplet_type == "H") %>% 
  summarise(median_rt = median(first_rt)) %>% 
  spread(stimulation, median_rt, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  rename(tacs_rt_h = tacs) %>% 
  rename(sham_rt_h = sham)
means_rt_low <- data_stim_raw %>% 
  group_by(ID, stimulation) %>%
  filter(first_acc == 1) %>% 
  filter(triplet_type == "L") %>% 
  summarise(median_rt = median(first_rt)) %>% 
  spread(stimulation, median_rt, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  rename(tacs_rt_l = tacs) %>% 
  rename(sham_rt_l = sham)
means_acc_high <- data_stim_raw %>% 
  group_by(ID, stimulation) %>%
  filter(triplet_type == "H") %>% 
  summarise(avg_acc = mean(first_acc)) %>% 
  spread(stimulation, avg_acc, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  rename(tacs_acc_h = tacs) %>% 
  rename(sham_acc_h = sham)
means_acc_low <- data_stim_raw %>% 
  group_by(ID, stimulation) %>%
  filter(triplet_type == "L") %>% 
  summarise(avg_acc = mean(first_acc)) %>% 
  spread(stimulation, avg_acc, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  rename(tacs_acc_l = tacs) %>% 
  rename(sham_acc_l = sham)
means_pred_error <- data_stim_raw %>% 
  group_by(ID, stimulation) %>%
  filter(first_acc == 0) %>% 
  summarise(avg_pred_e = mean(pred_error)) %>% 
  spread(stimulation, avg_pred_e, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  rename(tacs_pred_e = tacs) %>% 
  rename(sham_pred_e = sham)
means_avg_rt <- data_stim_raw %>% 
  group_by(ID, stimulation) %>%
  filter(first_acc == 1) %>% 
  summarise(avg_rt = mean(first_rt)) %>% 
  spread(stimulation, avg_rt, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  rename(tacs_avg_rt = tacs) %>% 
  rename(sham_avg_rt = sham)
means_avg_acc <- data_stim_raw %>% 
  group_by(ID, stimulation) %>%
  summarise(avg_acc = mean(first_rt)) %>% 
  spread(stimulation, avg_acc, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  rename(tacs_avg_acc = tacs) %>% 
  rename(sham_avg_acc = sham)


#put these together

for_analysis <- bind_cols(means_acc_low, means_acc_high, means_rt_low, 
                          means_rt_high, means_pred_error, means_avg_acc, means_avg_rt) %>% 
  mutate(ID1 = NULL, ID2 = NULL, ID3 = NULL, ID4 = NULL, ID5 = NULL, ID6 = NULL) %>% 
  mutate(tacs_rt_ls = tacs_rt_l - tacs_rt_h) %>% 
  mutate(sham_rt_ls = sham_rt_l - sham_rt_h) %>%
  mutate(tacs_acc_ls = tacs_acc_h - tacs_acc_l) %>%
  mutate(sham_acc_ls = sham_acc_h - sham_acc_l)

rm(means_acc_high, means_acc_low, means_avg_acc, means_avg_rt, means_pred_error,
   means_rt_high, means_rt_low, correct_rt_by_trial, pred_error_num)

#\o/ ready for some STATISTICAL ANALYIS

#but first, PLOTS

#boxplots for outlier

ggplot(for_analysis)+
  geom_qq(mapping = aes(sample = sham_rt_h), color = "black")+
  geom_qq(mapping = aes(sample = sham_rt_l), color = "blue")+
  geom_qq(mapping = aes(sample = tacs_rt_h), color = "red")+
  geom_qq(mapping = aes(sample = tacs_rt_l), color = "green")+
  labs(y = "Reaction time in ms" , x = "Participants")

ggplot(for_analysis)+
  geom_qq(mapping = aes(sample = sham_acc_h), color = "black")+
  geom_qq(mapping = aes(sample = sham_acc_l), color = "blue")+
  geom_qq(mapping = aes(sample = tacs_acc_h), color = "red")+
  geom_qq(mapping = aes(sample = tacs_acc_l), color = "green")+
  labs(y = "Accuracy" , x = "Participants") # there are some participants who has 
#lower accuracy, good to know, but sofar they remain in the sample, they have also quality data

#and now, the "plots to publish"

install.packages("wesanderson")
library(wesanderson)

plot_rt <- data_stim_raw %>% 
  filter(triplet_type == "H" | triplet_type == "L") %>% 
  filter(first_acc == 1) %>% 
  group_by(stimulation, triplet_type) %>% 
  summarise(median_rt = median(first_rt))

ggplot(plot_rt) +
  geom_line(aes(x = triplet_type, y = median_rt, group = stimulation, color = stimulation), size = 3) +
  scale_color_manual(values=wes_palette(n=2, name="Moonrise3"))+
  geom_point(aes(x = triplet_type, y = median_rt, group = stimulation, fill = stimulation))+
  scale_fill_manual(values=wes_palette(n=2, name="Moonrise3"))+
  theme_minimal()+
  labs(caption = "RTs for high frequency and low frequency events separately in the two experimental conditions")+
  labs(title = "Reaction times")+
  labs(x = "Frequency of trial" , y ="Reaction time in ms")+
  theme(legend.position = "top")
ggsave("rt_plot.jpg", width = 10, height = 7)

plot_acc <- data_stim_raw %>% 
  filter(triplet_type == "H" | triplet_type == "L") %>% 
  group_by(stimulation, triplet_type) %>% 
  summarise(mean_acc = mean(first_acc))

ggplot(plot_acc) +
  geom_line(aes(x = triplet_type, y = mean_acc, group = stimulation, color = stimulation), size = 3) +
  scale_color_manual(values=wes_palette(n=2, name="GrandBudapest"))+
  geom_point(aes(x = triplet_type, y = mean_acc, group = stimulation, fill = stimulation))+
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest"))+
  theme_minimal()+
  labs(caption = "ACCs for high frequency and low frequency events separately in the two experimental conditions")+
  labs(title = "Accuracy")+
  labs(x = "Frequency of trial" , y ="Accuracy")+
  theme(legend.position = "top")
ggsave("acc_plot.jpg", width = 10, height = 7)

plot_pred_error <- data_stim_raw %>% 
  filter(triplet_type == "H" | triplet_type == "L") %>% 
  filter(first_acc == 0) %>% 
  group_by(stimulation) %>% 
  summarise(avg_pred_e = mean(pred_error))

ggplot(data = plot_pred_error)+
  geom_col(aes(x = stimulation, y = avg_pred_e , fill = stimulation), position = "dodge")+
  scale_fill_manual(values=wes_palette(n=2, name="Cavalcanti"))+
  scale_y_continuous(limits = c(0, 1))+
  theme_minimal()+
  labs(x = "Stimulation conditions" , y = "Percentage of predicitve errors")+
  labs(title = "Predictive errors")+
  theme(legend.position = "top")
ggsave("pred_error_plot.jpg", width = 10, height = 7)


rm(plot_rt, plot_acc, plot_pred_error)
  
#assumption checking: normality (for the paired sample t-test)

install.packages("stats")
library(stats)

shapiro.test(for_analysis$sham_pred_e)
shapiro.test(for_analysis$tacs_pred_e)
shapiro.test(for_analysis$sham_acc_ls)
shapiro.test(for_analysis$tacs_acc_ls)
shapiro.test(for_analysis$sham_rt_ls)
shapiro.test(for_analysis$tacs_rt_ls)

mean(for_analysis$tacs_rt_ls)
max(for_analysis$tacs_rt_ls)
min(for_analysis$tacs_rt_ls)
sd(for_analysis$tacs_rt_ls)

mean(for_analysis$sham_rt_ls)
max(for_analysis$sham_rt_ls)
min(for_analysis$sham_rt_ls)
sd(for_analysis$sham_rt_ls)

mean(for_analysis$tacs_acc_ls)
max(for_analysis$tacs_acc_ls)
min(for_analysis$tacs_acc_ls)
sd(for_analysis$tacs_acc_ls)

mean(for_analysis$sham_acc_ls)
max(for_analysis$sham_acc_ls)
min(for_analysis$sham_acc_ls)
sd(for_analysis$sham_acc_ls)

mean(for_analysis$tacs_pred_e)
max(for_analysis$tacs_pred_e)
min(for_analysis$tacs_pred_e)
sd(for_analysis$tacs_pred_e)

mean(for_analysis$sham_pred_e)
max(for_analysis$sham_pred_e)
min(for_analysis$sham_pred_e)
sd(for_analysis$sham_pred_e)


#testing Hypothesis 1: theta band tACS disrupts learning which results in less predictive errors

t.test(x = for_analysis$sham_pred_e, y = for_analysis$tacs_pred_e,
         alternative = c("two.sided"),
         mu = 0, paired = TRUE, var.equal = TRUE,
         conf.level = 0.95) #well, sad lfe is sad :(

#testing Hypothesis 2: theta band tacs disrupts learning which results in smaller learning scores

t.test(x = for_analysis$sham_rt_ls, y = for_analysis$tacs_rt_ls,
       alternative = c("two.sided"),
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)

t.test(x = for_analysis$sham_acc_ls, y = for_analysis$tacs_acc_ls,
       alternative = c("two.sided"),
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)

#testing Hypothesis 3: stimulation per se does not affect average performance.

t.test(x = for_analysis$sham_avg_rt, y = for_analysis$tacs_avg_rt,
       alternative = c("two.sided"),
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)

t.test(x = for_analysis$sham_avg_acc, y = for_analysis$tacs_avg_acc,
       alternative = c("two.sided"),
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)


#Finally check whether N of predictive errors and learning scores are correleted in
#the two experimental conditions.

cor.test(for_analysis$sham_pred_e, for_analysis$sham_rt_ls, method = "spearman")
cor.test(for_analysis$tacs_pred_e, for_analysis$tacs_rt_ls, method = "spearman")

cor.test(for_analysis$sham_pred_e, for_analysis$sham_acc_ls, method = "spearman")
cor.test(for_analysis$tacs_pred_e, for_analysis$tacs_acc_ls, method = "spearman")


