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
library(knitr)
library(janitor)
library(esquisse)
library(infer)

# getting the data
gs4_deauth()
data <- read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1GT8UOmD4l2j88wCLfCAlh_R7r0jWCtsmoL84kVvDOlc/edit?usp=sharing"
)

# viewing the data on the web
# esquisser(data = data, viewer = 'browser')


## visualize historical vote share by state and party ----
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
    geom_point(data = rep_dem_other, aes(x = state, y = mean_r, color = 'red')) +
    geom_point(data = rep_dem_other, aes(x = state, y = mean_d, color = 'blue')) +
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



## table on three key blue wall states ----

# States: MI, PA, WI.
# We wish to see what party usually wins when they vote in a block
# Create two new columns, all_together and national_winner for this table.

blue_wall <-  data %>%
  group_by(year) %>% # have an election winner for every year
  mutate(
    election_winner = as.factor(case_when(
      sum(r_ev) > sum(d_ev) + sum(other_ev) ~ "Republican",
      sum(d_ev) > sum(r_ev) + sum(other_ev) ~ 'Democratic',
      sum(other_ev) > (r_ev) + (d_ev) ~ 'Other'
        )
      )
    ) %>%
  ungroup() %>%
  filter(
    state %in% c('Michigan', 'Pennsylvania', 'Wisconsin')  # filter for three main blue wall states
  ) %>%
  group_by(year) %>%
  mutate(
    all_together = if_else(n_distinct(winning_party) == 1, TRUE, FALSE)  # if they all voted for the same party
    ) %>%
  ungroup() %>%
  group_by(
    year
    ) %>%
  summarize(
  all_together = first(all_together),
  state_winner = first(winning_party),
  national_winner = first(election_winner)
  ) %>%
  mutate(
    correct_prediction = if_else(all_together & state_winner == national_winner, TRUE, FALSE)
    )
# the table itself
initial_table <- tabyl(blue_wall, all_together , correct_prediction) %>%  # make it look nice with adorn
  adorn_title(
    placement = 'combined',
    row_name = "Correct Prediction",
    col_name = 'Voted Together'
  )
# nice formatting with kable
kable(
 initial_table,
 caption = 'Frequency of National Winner Predictions when MI, PA, WI Vote together',
 align = 'c'
) %>% kableExtra::kable_classic()




## bootstrapping and numerical results ----

# for the following analysis, we wish to bootstrap the data to get an insight into the following question
# What can we expect the electoral value (in terms of EV) of a single vote given that you vote in a a competitive state ?
# Significance:
# Many elections are decided by very slim margins, and voters, especially in battleground and swing states are inundated with
# messaging about the power of their vote to sway their state results. If so, the voters in a close state that make up the winning
# would constitute a high electoral value for each given vote. Via bootstrapping, we can see the average electoral vote of margin voters
# We separate the data into two groups: margins less than 8% and margins greater than 8% and compare the bootstrap distributions.

# use a sample size of 100 and 2000 repetitions for each
# for graph quality, we assign signed margins for a party. negative: democrat, positive: republican
bstrap_high_margin <- data.frame(
  data %>%
    filter(win_margin_percent > 8) %>%
    rep_sample_n(size = 100, reps = 2000, replace = TRUE)
  )

bstrap_low_margin <- data.frame(
  data %>%
    filter(win_margin_percent < 8) %>%
    rep_sample_n(size = 100, reps = 2000, replace = TRUE)
 )
# we now plot a histogram  and also review the numerical results
ggplot() +
  geom_histogram(aes((win_margin_votes/total_ev)), data = bstrap_high_margin, bins = 80, alpha = 0.4, fill = 'red') +
  geom_histogram(aes((win_margin_votes/total_ev)), data = bstrap_low_margin, bins = 80, alpha = 0.4, fill = 'blue') +
  geom_vline(xintercept = mean(bstrap_high_margin$win_margin_votes/bstrap_high_margin$total_ev), color = 'red') +
  geom_vline(xintercept = mean(bstrap_low_margin$win_margin_votes/bstrap_low_margin$total_ev), color = 'blue') +
  labs(
    x = 'Margin Voters Per Electoral Vote',
    y = 'Number of Outcomes in Simulation',
    title = 'Distribution of Margin Voters per Electoral Vote (20,000 Bootstrapped Values)'
    ) +
  annotate(
    'text',
    x = mean(bstrap_high_margin$win_margin_votes/bstrap_high_margin$total_ev) + 7500,
    y = 15000,
    label = paste0('High\n Margin\n Mean\n',
                   round(mean(bstrap_high_margin$win_margin_votes/bstrap_high_margin$total_ev),1)),
    color = 'red',
    angle = 0
    ) +
  annotate(
    'text',
    x = mean(bstrap_low_margin$win_margin_votes/bstrap_low_margin$total_ev) + 10000,
    y = 15000,
    label = paste0('Low\n Margin\n Mean\n',
                   round(mean(bstrap_low_margin$win_margin_votes/bstrap_low_margin$total_ev), 1)),
    color = 'blue',
    angle = 0,
    vjust = 0.5
    )

# review of numerical results

init_table_bstrap_num_results <- rbind(bstrap_high_margin, bstrap_low_margin) %>%
  mutate(
    high_margin = win_margin_percent > 0
    ) %>%
  group_by(high_margin) %>%
  summarize(
    mean_vote_per_ev = mean(win_margin_votes/total_ev),
    sd_vote_per_ev = mean(win_margin_votes/total_ev),
    # confidence intervals
    lower_ci = mean(win_margin_votes / total_ev) - qt(0.975, df = n() - 1) *
    (sd(win_margin_votes / total_ev) / sqrt(n())),
    upper_ci = mean(win_margin_votes / total_ev) + qt(0.975, df = n() - 1) *
      (sd(win_margin_votes / total_ev) / sqrt(n()))
  )

# bstrap_res_table <-; TODO: polish the table


## incorporating a machine learning model (SVM); TODO: predict the party that wins the state with SVM model