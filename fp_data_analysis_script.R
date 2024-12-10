# Prajwal Bhandari, Jack Mariani, Sam Johnson

# load required packages
library(tidyverse)
library(rvest)
library(ggplot2)

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


