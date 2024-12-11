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

# getting the data

# have to split years
for (i in 0:8) {
  result <- 1980 + 4 * i
  str <-  paste0("https://www.presidency.ucsb.edu/statistics/elections/",result)
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table()

  assign(paste0("table_", result), table)
}


########### jack
# trying to clean the data for some tables
# Loop through the years from 1980 to 2020, incrementing by 4
for (year in seq(1980, 2020, by = 4)) {
  # Construct the table name dynamically
  table_name <- paste0("table_", year)

  # Get the table (assuming the tables are stored as data frames)
  table <- get(table_name)

  # Handle specific row deletions based on the year
  if (year == 1980 || (year >= 1992 && year <= 2008)) {
    # For 1980 and 1992-2008, remove rows 1 to 9
    table <- table[-(1:9), ]
  } else if (year %in% c(1984, 1988)) {
    # For 1984 and 1988, remove rows 1 to 10
    table <- table[-(1:10), ]
  } else if (year == 2012) {
    # For 2012, remove rows 1 to 11
    table <- table[-(1:11), ]
  } else if (year == 2016) {
    # For 2016, remove rows 1 to 14
    table <- table[-(1:14), ]
    # Additional rows specific to 2016 (assuming they are known; e.g., row 15)
    table <- table[-(29:31), ]  # Adjust as necessary for 2016 extra rows
  } else if (year == 2020) {
    # For 2020, remove rows 1 to 13
    table <- table[-(1:13), ]
    table <- table[-(21:22), ]
    table <- table[-(29:31), ]
  }

  # Reset row numbers to 1 through the number of remaining rows
  rownames(table) <- 1:nrow(table)

  # Remove rows after 51 (if there are more than 51 rows)
  if (nrow(table) > 51) {
    table <- table[1:51, ]
  }

  # Save the modified table back to the environment
  assign(table_name, table)
}


### Prajwal data cleaning

# clean the data from 1980,92,96. This should be pretty straight forward.
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
      year = 2000 # add year col
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

  return(table)
}
# clean the data from 2004
clean_2004 <- function(){
  # to do
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
  return(table)
}

# actual function to create the table:
create_clean_data <- function(){

  clean_data = data.frame()

  # we break the years into cases

  # first few years
  for (i in 0:8) {
    year <- 1980 + 4 * i
    str <-  paste0("https://www.presidency.ucsb.edu/statistics/elections/",year)
    table <- read_html(str) %>%
      html_element(css = 'table') %>%
      html_table()
    rbind(clean_data, clean_this_table(table))

  #1980, 1992,1996
  if (year %in% c(1980,1992,1996)){
    rbind(clean_data, clean_80_92_96(year))
  }
  # 1884
  if (year %in% c(1984,1988)) {
    rbind(clean_data, clean_84_88(year))
  }

  # 2016
  if (year == 2016) {
      rbind(clean_data, clean_2016())
    }
  }
}

