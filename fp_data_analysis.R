## Prajwal Bhandari, Jack Mariani, Sam Johnson
# data analysis script


# import required libraries and the data
library(tidyverse)
library(rvest)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rvest)
library(readr)
library(googlesheets4)

library(esquisse)
# getting the data
gs4_deauth()
data <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1GT8UOmD4l2j88wCLfCAlh_R7r0jWCtsmoL84kVvDOlc/edit?usp=sharing"
)


data %>%
  group_by(state) %>%
  summarize(
    mean_r = mean(r_percent),
    ) %>%
  ggplot(aes(x = state, y = mean_r)) +
  geom_point() +
  coord_flip()
