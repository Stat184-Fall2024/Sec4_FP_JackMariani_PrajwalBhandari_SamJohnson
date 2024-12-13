# Prajwal Bhandari, Jack Mariani, Sam Johnson

# load required packages
library(tidyverse)
library(rvest)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rvest)
library(readr)

# collecting and cleaning the data

# clean the data from 1980,92,96
clean_80_92_96 <- function(y){
  str <- paste0("https://www.presidency.ucsb.edu/statistics/elections/",y)
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table() %>%
    slice(-c(1:9)) %>%
    slice(-c(52:n()))
  if (y %in% c(1992,1996)) {

    table <- table[, (1:11)] %>%
      mutate(
        X5 = ifelse(X5 == "", '0', X5), # replace blanks with 0
        X8 = ifelse(X8 == "", '0', X8),
        X11 = ifelse(X11 == "",'0', X11),
        # convert character cols to numeric by using readr::parse_number
        X2 = readr::parse_number(X2),
        X3 = readr::parse_number(X3),
        X6 = readr::parse_number(X6),
        X5 = readr::parse_number(X5),
        X8 = readr::parse_number(X8),
        X9 = readr::parse_number(X9),
        X11 = readr::parse_number(X11),
        X4 = readr::parse_number(gsub("%", "", X4)),
        X7 = readr::parse_number(gsub("%", "", X7)),
        X10 = readr::parse_number(gsub("%", "", X10)),
        year = y # add year col
      ) %>%
        rename(
          state = X1,
          total_votes = X2,
          d_votes = X3,
          d_percent = X4,
          d_ev = X5,
          r_votes = X6,
          r_percent = X7,
          r_ev = X8,
          other_votes = X9,
          other_percent = X10,
          other_ev = X11,
          year = year
        ) %>% # rearrange the order of cols
        select(
          year, state, total_votes,
          d_votes, d_percent, d_ev,
          r_votes, r_percent, r_ev,
          other_votes, other_percent, other_ev
        )
    }
  else{
      table <- table[, (1:11)] %>%
        mutate(
          X5 = ifelse(X5 == "", '0', X5), # replace blanks with 0
          X8 = ifelse(X8 == "", '0', X8),
          X11 = ifelse(X11 == "",'0', X11),
          # convert character cols to numeric by using readr::parse_number
          X2 = readr::parse_number(X2),
          X3 = readr::parse_number(X3),
          X6 = readr::parse_number(X6),
          X5 = readr::parse_number(X5),
          X8 = readr::parse_number(X8),
          X9 = readr::parse_number(X9),
          X11 = readr::parse_number(X11),
          X4 = readr::parse_number(gsub("%", "", X4)),
          X7 = readr::parse_number(gsub("%", "", X7)),
          X10 = readr::parse_number(gsub("%", "", X10)),
          year = y # add year col
        ) %>%
        rename(
          state = X1,
          total_votes = X2,
          r_votes = X3,
          r_percent = X4,
          r_ev = X5,
          d_votes = X6,
          d_percent = X7,
          d_ev = X8,
          other_votes = X9,
          other_percent = X10,
          other_ev = X11,
          year = year
        ) %>% # rearrange the order of cols
        select(
          year, state, total_votes,
          d_votes, d_percent, d_ev,
          r_votes, r_percent, r_ev,
          other_votes, other_percent, other_ev
        )
    }
  return(table)
}
# clean the data from 1984, 88
clean_84_88 <- function(y){
  str <-  paste0("https://www.presidency.ucsb.edu/statistics/elections/",y)
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table() %>%
    slice(-(1:10)) %>%
    slice(-(52:n()))

  table <- table[, (1:11)] %>%
    mutate(
      X5 = ifelse(X5 == "", '0', X5), # replace blanks with 0
      X8 = ifelse(X8 == "", '0', X8),
      X11 = ifelse(X11 == "",'0', X11),
      # convert character cols to numeric by using readr::parse_number
      X2 = readr::parse_number(X2),
      X3 = readr::parse_number(X3),
      X6 = readr::parse_number(X6),
      X5 = readr::parse_number(X5),
      X8 = readr::parse_number(X8),
      X4 = readr::parse_number(gsub("%", "", X4)),
      X7 = readr::parse_number(gsub("%", "", X7)),
      X9 = X2 - X3 - X6, # other votes = total - d - r
      X11 = 0, #third parties don't get any Electoral Votes
      year = y # add year col
    ) %>%
    mutate(
      X10 = 100 - X4 - X7 #mutate again for percentage, can use X4 and X7 since they are numeric
    ) %>%
    rename(
      state = X1,
      total_votes = X2,
      r_votes = X3,
      r_percent = X4,
      r_ev = X5,
      d_votes = X6,
      d_percent = X7,
      d_ev = X8,
      other_votes = X9,
      other_percent = X10,
      other_ev = X11,
      year = year
    ) %>% # rearrange the order of cols
    select(
      year, state, total_votes,
      d_votes, d_percent, d_ev,
      r_votes, r_percent, r_ev,
      other_votes, other_percent, other_ev
    )
  if (y == 1988) {
    table[49,12] <- 1 # a faithless elector gave a vote to Lloyd Bernstein
    table[49,2] <- "West Virginia"
  }

  return(table)
}
# clean the data from 2000
clean_2000 <- function(){
  str <-  paste0("https://www.presidency.ucsb.edu/statistics/elections/2000")
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table() %>%
    slice(-(1:9)) %>%
    slice(-(52:n()))
  table <- table[, (1:11)] %>%
    mutate(
      X5 = ifelse(X5 == "", '0', X5), # replace blanks with 0
      X8 = ifelse(X8 == "", '0', X8),
      X11 = ifelse(X11 == "",'0', X11),
      # convert character cols to numeric by using readr::parse_number
      X2 = readr::parse_number(X2),
      X3 = readr::parse_number(X3),
      X6 = readr::parse_number(X6),
      X5 = readr::parse_number(X5),
      X8 = readr::parse_number(X8),
      X9 = X2 - X3 - X6, # other votes = total - d - r
      X11 = 0, #third parties dont get any Electoral Votes
      X4 = readr::parse_number(gsub("%", "", X4)),
      X7 = readr::parse_number(gsub("%", "", X7)),
      year = 2000, # add year col
      X10 = 100 - X4 - X7
    ) %>%
    rename(
      state = X1,
      total_votes = X2,
      r_votes = X3,
      r_percent = X4,
      r_ev = X5,
      d_votes = X6,
      d_percent = X7,
      d_ev = X8,
      other_votes = X9,
      other_percent = X10,
      other_ev = X11,
      year = year
    ) %>% # rearrange the order of cols
    select(
      year, state, total_votes,
      d_votes, d_percent, d_ev,
      r_votes, r_percent, r_ev,
      other_votes, other_percent, other_ev
    )
  table[9,2] <- 'Dist. of Col.' #account for asterisk. We mention this in the report
  # An elector from the District of Columbia abstained, which is why there are  only two votes for the year

  return(table)
}
# clean the data from 2004
clean_2004 <- function(){
  str <-  "https://www.presidency.ucsb.edu/statistics/elections/2004"
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table() %>%
    slice(-(1:9)) %>%
    slice(-(52:n()))
  table <- table[, (1:11)] %>%
    mutate(
      X5 = ifelse(X5 == "", '0', X5), # replace blanks with 0
      X8 = ifelse(X8 == "", '0', X8),
      X11 = ifelse(X11 == "",'0', X11),
      # convert character cols to numeric by using readr::parse_number
      X2 = readr::parse_number(X2),
      X3 = readr::parse_number(X3),
      X6 = readr::parse_number(X6),
      X5 = readr::parse_number(X5),
      X8 = readr::parse_number(X8),
      X9 = X2 - X3 - X6, # other votes = total - d - r
      X11 = 0, #third parties dont get any Electoral Votes
      X4 = readr::parse_number(gsub("%", "", X4)),
      X7 = readr::parse_number(gsub("%", "", X7)),
      X10 = 100 - X4 - X7, # other % = total - d - r
      year = 2004, # add year col
    ) %>%
    rename(
      state = X1,
      total_votes = X2,
      r_votes = X3,
      r_percent = X4,
      r_ev = X5,
      d_votes = X6,
      d_percent = X7,
      d_ev = X8,
      other_votes = X9,
      other_percent = X10,
      other_ev = X11,
      year = year
    ) %>% # rearrange the order of cols
    select(
      year, state, total_votes,
      d_votes, d_percent, d_ev,
      r_votes, r_percent, r_ev,
      other_votes, other_percent, other_ev
    )

  table[24,2] <- 'Minnesota'  # remove the asterisk
  table[24,12] <- 1  # John Edwards got a vote from a faithless elector from MN
  return(table)
}
# clean the data from 2008
clean_2008 <- function(){
  str <-  "https://www.presidency.ucsb.edu/statistics/elections/2008"
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table() %>%
    slice(-(1:9)) %>%
    slice(-(52:n()))
  table <- table[, (1:11)] %>%
    mutate(
      X5 = ifelse(X5 == "", '0', X5), # replace blanks with 0
      X8 = ifelse(X8 == "", '0', X8),
      X11 = ifelse(X11 == "",'0', X11),
      # convert character cols to numeric by using readr::parse_number
      X2 = readr::parse_number(X2),
      X3 = readr::parse_number(X3),
      X6 = readr::parse_number(X6),
      X5 = readr::parse_number(X5),
      X8 = readr::parse_number(X8),
      X9 = X2 - X3 - X6, # other votes = total - d - r
      X11 = 0, #third parties dont get any Electoral Votes
      X4 = readr::parse_number(gsub("%", "", X4)),
      X7 = readr::parse_number(gsub("%", "", X7)),
      X10 = 100 - X4 - X7, # other % = total - d - r
      year = 2008, # add year col
    )  %>%
    rename(
      state = X1,
      total_votes = X2,
      d_votes = X3,
      d_percent = X4,
      d_ev = X5,
      r_votes = X6,
      r_percent = X7,
      r_ev = X8,
      other_votes = X9,
      other_percent = X10,
      other_ev = X11,
      year = year
    ) %>% # rearrange the order of cols
    select(
      year, state, total_votes,
      d_votes, d_percent, d_ev,
      r_votes, r_percent, r_ev,
      other_votes, other_percent, other_ev
    )

  return(table)
}
#clean the data from 2012
clean_2012 <- function(){
  str <- "https://www.presidency.ucsb.edu/statistics/elections/2012"
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table() %>%
    slice(-(1:11)) %>%
    slice(-(52:n()))

  table <- table[, (1:11)] %>%
    mutate(
      X5 = ifelse(X5 == "", '0', X5), # replace blanks with 0
      X8 = ifelse(X8 == "", '0', X8),
      X11 = ifelse(X11 == "",'0', X11),
      # convert character cols to numeric by using readr::parse_number
      X2 = readr::parse_number(X2),
      X3 = readr::parse_number(X3),
      X6 = readr::parse_number(X6),
      X5 = readr::parse_number(X5),
      X8 = readr::parse_number(X8),
      X9 = X2 - X3 - X6, # other votes = total - d - r
      X11 = 0, #third parties dont get any Electoral Votes
      X4 = readr::parse_number(gsub("%", "", X4)),
      X7 = readr::parse_number(gsub("%", "", X7)),
      X10 = 100 - X4 - X7, # other % = total - d - r
      year = 2012, # add year col
    )  %>%
    rename(
      state = X1,
      total_votes = X2,
      d_votes = X3,
      d_percent = X4,
      d_ev = X5,
      r_votes = X6,
      r_percent = X7,
      r_ev = X8,
      other_votes = X9,
      other_percent = X10,
      other_ev = X11,
      year = year
    )

  return(table)
}
# clean the data from 2016
clean_2016 <- function(){
  str <- "https://www.presidency.ucsb.edu/statistics/elections/2016"
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table() %>%
    slice(-c(1:14)) %>%
    slice(-c(55:n()))

  # to drop cols beyond 11 , reassign the table
  table <- table[, (1:11)] %>%
    mutate(
      X5 = ifelse(X5 == "", '0', X5), # replace blanks with 0
      X8 = ifelse(X8 == "", '0', X8),
      X11 = ifelse(X11 == "",'0', X11),
      # convert character cols to numeric by using readr::parse_number
      X2 = readr::parse_number(X2),
      X3 = readr::parse_number(X3),
      X6 = readr::parse_number(X6),
      X5 = readr::parse_number(X5),
      X8 = readr::parse_number(X8),
      X9 = readr::parse_number(X9),
      X11 = readr::parse_number(X11),
      X4 = readr::parse_number(gsub("%", "", X4)),
      X7 = readr::parse_number(gsub("%", "", X7)),
      X10 = readr::parse_number(gsub("%", "", X10)),
      year = 2016 # add year col
    )

  # drop nebraska congressional district data. combine into the state data.
  #- replace electoral vote counts
  table[28,'X8'] <- 5
  #- drop CD-{1,2,3)
  table <- table[-c(29,30,31),]

  # finally, rename the cols of the table

  table <- table %>%
    rename(
      state = X1,
      total_votes = X2,
      d_votes = X3,
      d_percent = X4,
      d_ev = X5,
      r_votes = X6,
      r_percent = X7,
      r_ev = X8,
      other_votes = X9,
      other_percent = X10,
      other_ev = X11,
      year = year
    ) %>% # rearrange the order of cols
    select(
      year, state, total_votes,
      d_votes, d_percent, d_ev,
      r_votes, r_percent, r_ev,
      other_votes, other_percent, other_ev
    )

  # Washington had 4 faithless electors, hawaii had 1, texas had 2 for a total of 7 faithless electors
  table[48,6] <- 8 # wa
  table[48,12] <- 4
  table[12,6] <- 3  #hi
  table[12,12] <- 1
  table[44, 9] <- 36 #tx
  table[44,12] <- 2

  return(table)
}
# clean the data from 2020
clean_2020 <- function(){
  str <- "https://www.presidency.ucsb.edu/statistics/elections/2020"
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table() %>%
    slice(-(1:13)) %>%
    slice(-(57:n()))
  table <- table[,(1:11)] %>%
    mutate(
      X5 = ifelse(X5 == "", '0', X5), # replace blanks with 0
      X8 = ifelse(X8 == "", '0', X8),
      X11 = ifelse(X11 == "",'0', X11),
      # convert character cols to numeric by using readr::parse_number
      X2 = readr::parse_number(X2),
      X3 = readr::parse_number(X3),
      X6 = readr::parse_number(X6),
      X5 = readr::parse_number(X5),
      X8 = readr::parse_number(X8),
      X9 = readr::parse_number(X9),
      X11 = readr::parse_number(X11),
      X4 = readr::parse_number(gsub("%", "", X4)),
      X7 = readr::parse_number(gsub("%", "", X7)),
      X10 = readr::parse_number(gsub("%", "", X10)),
      year = 2020 # add year col
    ) %>%
    rename(
      state = X1,
      total_votes = X2,
      d_votes = X3,
      d_percent = X4,
      d_ev = X5,
      r_votes = X6,
      r_percent = X7,
      r_ev = X8,
      other_votes = X9,
      other_percent = X10,
      other_ev = X11,
      year = year
    ) %>% # rearrange the order of cols
    select(
      year, state, total_votes,
      d_votes, d_percent, d_ev,
      r_votes, r_percent, r_ev,
      other_votes, other_percent, other_ev
    )

  # remove maine and nebraska congressional rows. add EV to at large vote based on Party.
  # nebraska
  table[30,6] <- 1
  table[30,9] <- 4
  # maine
  table[20,6] <- 3
  table[20,9] <- 1

  # remove cd rows for maine and nebraska
  table <- table %>%
    slice(-(21:22)) %>%
    slice(-(29:31))
  # rename 'District of Columbia' to 'Dist. of Col."
  table[9,2] <- 'Dist. of Col.'

  return(table)
}

# actual function to create the table:
create_clean_data <- function(){
  # create empty data frame which we add our clean tables to
  clean_data <- data.frame()
  # write a loop to do this
  for (year in seq(1980, 2020, by = 4)) {
    #1980, 1992,1996
    if (year %in% c(1980,1992,1996)){
      clean_data <- rbind(clean_data, clean_80_92_96(year))
    }
    # 1984, 1998
    if (year %in% c(1984,1988)) {
      clean_data <- rbind(clean_data, clean_84_88(year))
    }
    #2000
    if (year == 2000) {
      clean_data <- rbind(clean_data, clean_2000())
    }
    #2004
    if (year == 2004) {
      clean_data <- rbind(clean_data, clean_2004())
    }
    # 2008
    if (year == 2008) {
      clean_data <- rbind(clean_data, clean_2008())
    }
    if (year == 2012) {
      clean_data <- rbind(clean_data, clean_2012())
    }
    # 2016
    if (year == 2016) {
      clean_data <- rbind(clean_data, clean_2016())
    }
    #2020
    if (year == 2020) {
      clean_data <- rbind(clean_data, clean_2020())
    }
  }

  # now that we have the data in a clean format, we can do some feature engineering
  # to extract data which would be useful to use in the analysis.
  clean_data <- clean_data %>%
    mutate(
      win_margin_votes = pmax(r_votes, d_votes) - pmin(r_votes, d_votes),
      win_margin_percent = pmax(r_percent, d_percent) - pmin(r_percent, d_percent),
      # winning party
      winning_party = case_when(r_votes > d_votes & r_votes > other_votes ~ "Republican",
                                d_votes > r_votes & d_votes > other_votes ~ "Democrat",
                                other_votes > r_votes & other_votes > d_votes ~ "Other"),
      total_ev = d_ev + r_ev + other_ev,
      voters_per_ev = total_votes/total_ev,
      # vote share ratio (r/d)
      r_d_vote_ratio = r_votes/d_votes,
      # third party significance
      other_sig = other_votes/(r_votes + d_votes),
      # measures if third party votes influenced the result of the state
      third_party_swing_potential = other_votes > abs(r_votes - d_votes),

      # change data type of winning party
      winning_party = as.factor(winning_party),

    )

  # one final change: in 2000, an elector abstained so we can add the missing EV to DC's total EV count
  clean_data[264,16] <- 3

  return(clean_data)
}

# now that we have the data in a tidy, clean and easy to work with format, we can start the analysis.
data <- create_clean_data()


# we push the data to a google sheets document so that it is easier to retreive when working in the quarto document.
library(googlesheets4)
gs4_auth()
data %>%
  write_sheet(
  ss =
    gs4_get(
    "https://docs.google.com/spreadsheets/d/1GT8UOmD4l2j88wCLfCAlh_R7r0jWCtsmoL84kVvDOlc/edit?usp=sharing"),
  sheet = 'main'
  )
