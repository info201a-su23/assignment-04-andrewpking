source("analysis.R")
library(tidyverse)
library(knitr)

# Run summary functions:
change_inc_by_race <- change_inc_white_vs_bipoc()
inc_percent <- signif(increase_percent(), digits = 6)

change_percent_message <- paste0(
  "Percent population growth in US prisons from 1970 to 2016 is ", 
  inc_percent, "%")

knitr::kable(change_inc_by_race,
             col.names = c("Race","Average Prison Change", "Average Jail Change"))
cat(change_percent_message)
