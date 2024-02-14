# Load packages 
library(stats)
library(rstatix)
library(TOSTER)
library(MOTE)
library(tidyverse)

# Load data 

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
# Active data ---------------
## Prepare active data --------------------------------------------------------------------

active_data1 <- active_data %>%
  select(Participant, sprint1_total_time, sprint2_total_time,
         sprint3_total_time, sprint4_total_time, sprint5_total_time,
         sprint6_total_time, sprint7_total_time, sprint8_total_time,
         sprint9_total_time, sprint10_total_time)

## Create variables ------------
### Total time  -------------

# Compute the total time variable 
active_data1 <- active_data1 %>%
  mutate(total_time = rowSums(select(active_data1, sprint1_total_time, sprint2_total_time,
                                 sprint3_total_time, sprint4_total_time, sprint5_total_time,
                                 sprint6_total_time, sprint7_total_time, sprint8_total_time,
                                 sprint9_total_time, sprint10_total_time))) 

### Best sprint time ----------

# Create a variable for the best sprint time - needed to compute the total sprint decrement

active_data1 <- active_data1 %>% 
  rowwise() %>% 
  mutate(best_sprint = min(sprint1_total_time, sprint2_total_time,
                           sprint3_total_time, sprint4_total_time, sprint5_total_time,
                           sprint6_total_time, sprint7_total_time, sprint8_total_time,
                           sprint9_total_time, sprint10_total_time))

  
### Sprint decrement --------------

# Calculate the sprint decrement for each 

active_decrement <- (((active_data1$total_time/(active_data1$best_sprint*10)) * 100) - 100) %>%
  as.data.frame() %>%
  rename(sprint_decrement = ".") %>%
  mutate(Participant = c(1:15, 18))

# join the datasets
active_sprint_decrement <- left_join(active_data1, active_decrement, by = "Participant") %>%
  as.data.frame()

## Total sprint decrement descriptives --------------

# Get the overall means and sd 

active_means <- active_sprint_decrement %>%
  select(where(is.numeric)) %>%
  select(-Participant) %>%
  colMeans(active_sprint_decrement$total_time) %>%
  as.data.frame() %>%
  rownames_to_column('variable') %>%
  rename(mean = ".") %>%
sapply(sd, na.rm = TRUE) 

active_sd <- active_sprint_decrement %>%
  select(where(is.numeric)) %>%
  select(-Participant) %>%
  sapply(sd, na.rm = TRUE)  %>%
  as.data.frame() %>%
  rownames_to_column('variable') %>%
  rename(sd = ".") 


# Passive data ---------------
## Prepare passive data --------------------------------------------------------------------

passive_data1 <- passive_data %>%
  select(Participant, sprint1_total_time, sprint2_total_time,
         sprint3_total_time, sprint4_total_time, sprint5_total_time,
         sprint6_total_time, sprint7_total_time, sprint8_total_time,
         sprint9_total_time, sprint10_total_time)

## Create variables ------------
### Total time  -------------

# Compute the total time variable 
passive_data1 <- passive_data1 %>%
  mutate(total_time = rowSums(select(passive_data1, sprint1_total_time, sprint2_total_time,
                                     sprint3_total_time, sprint4_total_time, sprint5_total_time,
                                     sprint6_total_time, sprint7_total_time, sprint8_total_time,
                                     sprint9_total_time, sprint10_total_time))) 

### Best sprint time ----------

# Create a variable for the best sprint time - needed to compute the total sprint decrement

passive_data1 <- passive_data1 %>% 
  rowwise() %>% 
  mutate(best_sprint = min(sprint1_total_time, sprint2_total_time,
                           sprint3_total_time, sprint4_total_time, sprint5_total_time,
                           sprint6_total_time, sprint7_total_time, sprint8_total_time,
                           sprint9_total_time, sprint10_total_time))


### Sprint decrement --------------

# Calculate the sprint decrement for each 

passive_decrement <- (((passive_data1$total_time/(passive_data1$best_sprint*10)) * 100) - 100) %>%
  as.data.frame() %>%
  rename(sprint_decrement = ".") %>%
  mutate(Participant = c(1:15, 18))

# join the datasets
passive_sprint_decrement <- left_join(passive_data1, passive_decrement, by = "Participant") %>%
  as.data.frame()

## Total sprint decrement descriptives --------------
# Get the overall means and sd for total sprint decrement 
passive_means <- passive_sprint_decrement %>%
  select(where(is.numeric)) %>%
  select(-Participant) %>%
  colMeans(passive_sprint_decrement$total_time) %>%
  as.data.frame() %>%
  rownames_to_column('variable') %>%
  rename(mean = ".")

passive_sd <- passive_sprint_decrement %>%
  select(where(is.numeric)) %>%
  select(-Participant) %>%
  sapply(sd, na.rm = TRUE)  %>%
  as.data.frame() %>%
  rownames_to_column('variable') %>%
  rename(sd = ".") 



