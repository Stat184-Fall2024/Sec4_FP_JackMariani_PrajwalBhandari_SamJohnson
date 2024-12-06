# Prajwal Bhandari, Jack Mariani, Sam Johnson

##------- load required packages
library(tidyverse)
library(rvest)
library(ggplot2)

##------- collecting and cleaning the data
for (i in 0:10) {
  result <- 1980 + 4 * i
  str <-  paste0("https://www.presidency.ucsb.edu/statistics/elections/",result)
  table <- read_html(str) %>%
    html_element(css = 'table') %>%
    html_table()

  assign(paste0("table_", result), table)
}
