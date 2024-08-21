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

active_lac <- active_data %>%
  select(Participant, lactate_warmup, lactate_tests, lactate_fivemin_rest) %>%
  mutate(condition = "active")

passive_lac <- passive_data %>%
  select(Participant, lactate_warmup, lactate_tests, lactate_fivemin_rest) %>%
  mutate(condition = "passive")


lactate_data <- rbind(active_lac, passive_lac)

anova_data <- lactate_data %>%
  pivot_longer(cols = lactate_warmup:lactate_fivemin_rest,
               names_to = "time",
               values_to = "lactate") 

# ANOVA Test-----
# 1 x 3 ANOVA 

##afex::aov_4(continuous_var ~ group_var + (RM_var|id_var)

anova_results <- afex::aov_4(lactate ~ (condition*time|Participant), 
                             data = anova_data,
                             anova_table = list(es = "pes"))
anova_results

summary(anova_results)


### Post hoc contrasts ----------------------------------------------------------------------------

emm_data <-
  emmeans::emmeans(anova_results, ~ lactate, model = "multivariate")
emm_data

posthocresults <- pairs(emm_data, adjust = "bon") %>%
  broom::tidy(conf.int = T)
posthocresults


## Resolving assumptions --------

### Normality test -------

anova_data %>% 
  dplyr::group_by(condition, time) %>% 
  rstatix::shapiro_test(lactate) # shapiro-wilk test on individual groups

### Outliers check --------

anova_data %>%
  group_by(condition) %>%
  identify_outliers(lactate)

anova_data %>%
  group_by(time) %>%
  identify_outliers(lactate)

## Plots ---------

anova_data %>% 
  ggplot(aes(condition, time, lactate)) +  
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
  summarize(overall_mean = mean(lactate,na.rm=TRUE),
            overall_sd = sd(lactate, na.rm = TRUE),
            count = n())

