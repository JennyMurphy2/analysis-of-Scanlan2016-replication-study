# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)
library(emmeans)

# Load data 

active_data <- read_csv("active.csv") %>%
  drop_na()

passive_data <- read_csv("passive.csv") %>%
  drop_na()

# There was a data entry error and participant 16 and 17 were duplicated for the passive data
# Therefore removing both these participants from the active and passive data

active_data <- active_data %>%
  filter(Participant != 16,
         Participant != 17)

passive_data <- passive_data %>%
  filter(Participant != 16,
         Participant != 17)

# Active data descriptives --------------------------------------------------------------------

active_rpe <- active_data %>%
  select(Participant, sprint1_RPE,sprint2_RPE, sprint3_RPE, sprint4_RPE, sprint5_RPE, sprint6_RPE,
        sprint7_RPE, sprint8_RPE, sprint9_RPE, sprint10_RPE) %>%
  mutate(condition = "active")

passive_rpe <- passive_data %>%
  select(Participant, sprint1_RPE,sprint2_RPE, sprint3_RPE, sprint4_RPE, sprint5_RPE, sprint6_RPE,
         sprint7_RPE, sprint8_RPE, sprint9_RPE, sprint10_RPE) %>%
  mutate(condition = "passive")


rpe_data <- rbind(active_rpe, passive_rpe)

anova_data <- rpe_data %>%
  pivot_longer(cols = sprint1_RPE:sprint10_RPE,
               names_to = "time",
               values_to = "rpe") 

# ANOVA Test-----
# 1 x 3 ANOVA 

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_results <- afex::aov_4(rpe ~ (condition*time|Participant), 
                             data = anova_data,
                             anova_table = list(es = "pes"))
anova_results

summary(anova_results)


### Post hoc contrasts ----------------------------------------------------------------------------

emm_data <-
  emmeans::emmeans(anova_results, ~ rpe, model = "multivariate")
emm_data

posthocresults <- pairs(emm_data, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthocresults


## Resolving assumptions --------

### Normality test -------

anova_data %>% 
  dplyr::group_by(condition, time) %>% 
  rstatix::shapiro_test(rpe) # shapiro-wilk test on individual groups

### Outliers check --------

anova_data %>%
  group_by(condition) %>%
  identify_outliers(rpe)

anova_data %>%
  group_by(time) %>%
  identify_outliers(rpe)

## Plots ---------

anova_data %>% 
  ggplot(aes(condition, time, rpe)) +  
  geom_violin(fill = "gray") +
  geom_boxplot(width = .07,
               fill = "white") +
  geom_jitter(position = position_jitter(0.21)) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18,
               color = "red",
               size = 5) +
  theme_bw()

# Summary descriptives with outlier removed ------

summary <- anova_data %>% 
  group_by(condition,time) %>%
  summarize(overall_mean = mean(rpe,na.rm=TRUE),
            overall_sd = sd(rpe, na.rm = TRUE),
            count = n())

summary_conditions <- anova_data %>% 
  group_by(condition) %>%
  summarize(overall_mean = mean(rpe,na.rm=TRUE),
            overall_sd = sd(rpe, na.rm = TRUE),
            count = n())

