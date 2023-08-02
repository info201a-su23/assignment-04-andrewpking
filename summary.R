source("analysis.R")
library(tidyverse)
library(knitr)

# Load data and unpack it:
inc_tables <- load_data()
prison_pop <- inc_tables[[1]]
prison_jail_rate_1990 <- inc_tables[[3]]

# Run summary functions:
change_inc_by_race <- change_inc_white_vs_bipoc(prison_jail_rate_1990)
inc_percent <- signif(increase_percent(prison_pop), digits = 6)

change_percent_message <- paste0(
  "Percent population change in US prisons from 1970 to 2016 is ", 
  inc_percent, "%")

knitr::kable(change_inc_by_race,
             col.names = c("Race","Average Prison Change", "Average Jail Change"))
cat(change_percent_message)
