# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)

# Load data 
active_data <- read_csv("active.csv")
passive_data <- read_csv("passive.csv")

# Active data descriptives --------------------------------------------------------------------

# Get column means for RPE for each sprint for the active protocol data

active_RPE_mean <- colMeans(select(active_data, sprint1_RPE, sprint2_RPE, sprint3_RPE, sprint4_RPE, 
                                       sprint5_RPE, sprint6_RPE, sprint7_RPE,
                                       sprint8_RPE, sprint9_RPE, sprint10_RPE)) %>%
  as.data.frame() %>%
  rownames_to_column('sprint') %>%
  rename(active_mean = ".") %>%
  mutate_if(is.numeric, round)

# Calculate the sd for each sprint for the active protocol data

active_RPE_sd <- active_data %>%
  select(sprint1_RPE, sprint2_RPE, sprint3_RPE, sprint4_RPE, 
         sprint5_RPE, sprint6_RPE, sprint7_RPE,
         sprint8_RPE, sprint9_RPE, sprint10_RPE) %>%
  sapply(sd, na.rm = TRUE) %>%
  as.data.frame() %>%
  rownames_to_column('sprint') %>%
  rename(active_sd = ".")

# Passive data descriptives --------------------------------------------------------------------

# Get column means for RPE for each sprint for the passive protocol data

passive_RPE_mean <- colMeans(select(passive_data, sprint1_RPE, sprint2_RPE, sprint3_RPE, sprint4_RPE, 
                              sprint5_RPE, sprint6_RPE, sprint7_RPE,
                              sprint8_RPE, sprint9_RPE, sprint10_RPE)) %>%
  as.data.frame() %>%
  rownames_to_column('sprint') %>%
  rename(passive_mean = ".") %>%
  mutate_if(is.numeric, round)

# Calculate the sd for each sprint for the passive protocol data

passive_RPE_sd <- 
  select(passive_data, sprint1_RPE, sprint2_RPE, sprint3_RPE, sprint4_RPE, 
         sprint5_RPE, sprint6_RPE, sprint7_RPE,
         sprint8_RPE, sprint9_RPE, sprint10_RPE) %>%
  sapply(sd, na.rm = TRUE) %>%
  as.data.frame() %>%
  rownames_to_column('sprint') %>%
  rename(passive_sd = ".") 

# Combine for overall descriptives ----------------

RPE_mean <- left_join(active_RPE_mean, passive_RPE_mean, by = "sprint")
RPE_sd <- left_join(active_RPE_sd, passive_RPE_sd, by = "sprint")

overall_RPE_descriptives <- left_join(RPE_mean, RPE_sd, by = "sprint") 
overall_RPE_descriptives
