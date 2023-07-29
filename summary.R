library(tidyverse)

# Import the datasets
prison_pop <- read_csv(
  "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv"
  )
jail_pop <- read_csv(
  "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv"
)
prison_jail_rate <- read_csv(
  "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates.csv"
)
prison_jail_rate_1990 <- read_csv(
  "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv"
)
prison_jail_rate_1990_WA <- read_csv(
  "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv"
)

# Get total columns for each dataset
prison_pop_col <- ncol(prison_pop)
jail_pop_col <- ncol(jail_pop)
prison_jail_rate_col <- ncol(prison_jail_rate)
prison_jail_rate_1990_col <- ncol(prison_jail_rate_1990)
prison_jail_rate_1990_WA_col <- ncol(prison_jail_rate_1990_WA)

# Get total rows for each dataset
prison_pop_row <- nrow(prison_pop)
jail_pop_row <- nrow(jail_pop)
prison_jail_rate_row <- nrow(prison_jail_rate)
prison_jail_rate_1990_row <- nrow(prison_jail_rate_1990)
prison_jail_rate_1990_WA_row <- nrow(prison_jail_rate_1990_WA)

# Get column names for each dataset
prison_pop_col_names <- colnames(prison_pop)
jail_pop_col_names <- colnames(jail_pop)
prison_jail_rate_col_names <- colnames(prison_jail_rate)
prison_jail_rate_1990_col_names <- colnames(prison_jail_rate_1990)
prison_jail_rate_1990_WA_col_names <- colnames(prison_jail_rate_1990_WA)

# Prelim Observations prison_pop:
# - Missing demographic data
# - Years span 1970-2018

# Prelim Observations jail_pop:
# - Missing demographic data
# - Years span 1970-2018

# Prelim Observations prison_jail_rate:
# - Missing demographic data
# - Missing values in total_prison_pop_rate
# - total_pop and total_pop_rate could be helpful variables.
# - Years span 1970-2018

# Prelim Observations prison_jail_rate_1990:
# - Detailed demographic data
# - Missing values in aapi_prison_pop_rate
# - Years span 1990-2018

# Prelim Observations prison_jail_rate_1990_WA:
# - Detailed demographic data
# - Some missing demographic data, but could be cleaned
# - Years span 1990-2018

# Questions:
# 1: Which county in the USA has the highest prison population, which year?

# 2: Which county in the USA has the highest jail population, which year?

# 3: Which county in the USA has the highest prison rate, which year?

# 4: Which county in the USA has the highest jail rate, which year?

# 5: Which county has seen its prison rate grow the most between 1990 and 2018?

# 6: Which county has seen its jail rate grow the most between 1990 and 2018?

# 7: Which county has the largest proportion Black and Indigenous people in 
# prison?

# 7.1: What year was this?

# 8: Which county has the largest proportion Black and Indigenous people in 
# jail?

# 8.1: What year was this?

# 9: Which county in each state has the largest proportion of prisoners for 
# each year?

# 10: Which county in each state has the largest proportion of people in jail 
# for each year?

# 11: Which counties have the most missing demographic data for their prisons?

# 12: Which counties have the most missing demographic data for their jails?