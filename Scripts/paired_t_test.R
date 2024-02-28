# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)

# Load data --------------------------------------------------------------------
active_data <- read_csv("active.csv") %>%
  drop_na()

passive_data <- read_csv("passive.csv") %>%
  drop_na()

# There was a data entry error and participant 16 anmd 17 were duplicated for the passive data
# Therefore removing both these participants from the active and passive data

active_data <- active_data %>%
  filter(Participant != 16,
         Participant != 17)

passive_data <- passive_data %>%
  filter(Participant != 16,
         Participant != 17)

# Prepare data ----------------
# The original study summed the total sprint time - "...time was summed across the entire protocol"

active_data_sum <- active_data %>%
  mutate(active = rowSums(select(active_data, sprint1_total_time, sprint2_total_time,
                                             sprint3_total_time, sprint4_total_time, sprint5_total_time,
                                             sprint6_total_time, sprint7_total_time, sprint8_total_time,
                                             sprint9_total_time, sprint10_total_time))) %>%
  select(Participant, active) 

passive_data_sum <- passive_data %>%
  mutate(passive = rowSums(select(passive_data, sprint1_total_time, sprint2_total_time,
                                                    sprint3_total_time, sprint4_total_time, sprint5_total_time,
                                                    sprint6_total_time, sprint7_total_time, sprint8_total_time,
                                                    sprint9_total_time, sprint10_total_time))) %>%
  select(Participant, passive) 

combined_data <- inner_join(active_data_sum, passive_data_sum, by= "Participant") %>%
  mutate(difference = active - passive) 

paired_data <- combined_data %>% 
  pivot_longer(cols = c("active", "passive"),
               names_to = "intervention",
               values_to = "total_sprint_time")

## Calculate descriptives for data -------------------------------------


# Demographic descriptives

summary_dem <- passive_data %>%
  summarise(count = n(),
            mean_height = mean(Height),
            sd_height = sd(Height),
            mean_weight = mean(Weight),
            sd_weight = sd(Weight),
            mean_age = mean(Age),
            sd_age = sd(Age)
  )
summary_dem

# Replication descriptives

summary_rep <- paired_data %>%
  group_by(intervention) %>%
  summarise(count = n(),
            mean = mean(total_sprint_time),
            sd = sd(total_sprint_time)) %>%
  mutate(mean_diff = mean(combined_data$difference), 
         sd_diff = sd(combined_data$difference)
  )
summary_rep

#Original descriptives

summary_ori <- list(
  active_mean = 37.73,
  active_sd = 2.5,
  passive_mean = 35.02,
  passive_sd = 2.10,
  n = 9) %>%
  as.data.frame() %>%
  mutate(mean_diff = active_mean - passive_mean)
# need to add SD difference

## Resolving assumptions  ------------------------------------
### Checking distribution ---------------------------------------

ggplot(paired_data, aes(total_sprint_time)) +
  geom_histogram(color="black", fill="white", 
                 bins = 10) +
  facet_wrap(~ intervention,
          labeller = label_both)

ggplot(paired_data, aes(intervention, total_sprint_time, color = intervention)) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal()

  ggplot(paired_data, aes(intervention, total_sprint_time, color = intervention)) +  
  geom_violin(fill = "light gray") +
  geom_boxplot(width = .07,
               fill = "white") +
  geom_jitter(position = position_jitter(0.21)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 12,
               color = "black",
               size = 5) +
  theme_bw()

### Checking for outliers on difference score -----------------------------------

combined_data %>%
  identify_outliers(difference)

### Checking normality ----------------------------------------------------------
  combined_data %>% shapiro_test(difference) 

# T test ---------------------------------------------------

results <- t.test(total_sprint_time ~ intervention, paired_data, 
                  alternative = "two.sided", paired = TRUE, conf.level = 0.95) %>%
  tidy()
results

# Analyse the replication ------

## Calculate Original ES --------

# Estimating the t-value

pval = 0.002/2 # for two-tailed
quantile = 1 - pval

ori_tval <- qt(quantile, df = 8, lower.tail = FALSE)

ori_tval <- abs(ori_tval)
ori_tval

# Confirming the reported effect size

ori_dz <- d.dep.t.diff.t(t = ori_tval, n = 9, a = 0.05)
ori_dz

ori_dav <- d.dep.t.avg(m1=37.73, m2=35.02, sd1=2.5, sd2=2.1, n=9, a = 0.05)
ori_dav
# original study reported d = 1.17 and Cohen's dav is closer than calculations of dz

## Calculate replication ES ------

rep_dz <- d.dep.t.diff(mdiff = summary_rep$mean_diff[1], sddiff = summary_rep$sd_diff[1], 
                       n = summary_rep$count[1], a = 0.05)
rep_dz

rep_dav <- d.dep.t.avg(m1=summary_rep$mean[1], m2=summary_rep$mean[2], 
                       sd1=summary_rep$sd[1], sd2=summary_rep$sd[2], 
                       n=summary_rep$count[1], a = 0.05)
rep_dav


## Z-test (dav) --------

rep_test <- compare_smd(
  smd1 = 1.17,
  n1 = 9,
  smd2 = rep_dav$d,
  n2 = summary_rep$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test


## Z-test (dz) --------

rep_test <- compare_smd(
  smd1 = ori_dz$d,
  n1 = 9,
  smd2 = rep_dz$d,
  n2 = summary_rep$count[1],
  paired = TRUE,
  alternative = "greater")
rep_test
