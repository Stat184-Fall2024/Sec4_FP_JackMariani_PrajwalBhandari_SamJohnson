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

# viewing the data on the web
# esquisser(data = data, viewer = 'browser')

rep_dem_other <- data %>%
  group_by(state) %>%
  summarize(
    mean_r = mean(r_percent),
    mean_d = mean(d_percent),
    mean_o = mean(other_percent)
  ) %>%
  mutate(state = fct_reorder(state, mean_r))

# visualization of historical vote share for each state and DC
  ggplot() +
    geom_point(data = rep_dem_other, aes(x = state, y = mean_r, color = 'blue')) +
    geom_point(data = rep_dem_other, aes(x = state, y = mean_d, color = 'red')) +
    geom_point(data = rep_dem_other, aes(x = state, y = mean_o, color = 'green')) +
    theme(axis.text.x = element_text(angle = 80, vjust = 0.95, hjust = 1)) +
    scale_color_manual(
       name = 'Party',
       values = c("blue" = "navy", "green" = "darkgreen", "red" = "darkred"),
       labels = c("blue" = "Democratic", "green" = "Other", "red" = "Republican")) +
    labs(
      x = 'State',
      y = 'Mean Historical Vote Percentage',
      title = "Mean Historical Vote Share by Party and State"
      )

data %>%
  group_by(year) %>%
  summarize(d_ev = sum(d_ev),
            r_ev = sum(r_ev),
            o_ev = sum(other_ev)
  )  %>%
  ggplot(
    aes(x = year, y = d_ev))
  geom_line()
