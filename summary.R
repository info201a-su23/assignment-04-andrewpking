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

# Get total columns for each data-set
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

# Aggregate data for BIPOC communities in prison_jail_rate_1990
prison_jail_rate_1990 <- prison_jail_rate_1990 %>%
  mutate(bipoc_jail_pop_rate = black_jail_pop_rate + native_jail_pop_rate) %>%
  mutate(bipoc_prison_pop_rate = black_prison_pop_rate + native_prison_pop_rate)

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
# 1: Which county in the USA has the highest prison population, 
# per year since 1970, what is the population?
prison_pop_highest_county <- prison_pop %>%
  filter(total_pop == max(total_pop), .by = year) %>%
  reframe(year, state, county_name, total_pop)

# 2: Which county in the USA has the highest jail population, 
# per year since 1970, what is the population?
jail_pop_highest_county <- jail_pop %>%
  filter(total_pop == max(total_pop), .by = year) %>%
  reframe(year, state, county_name, total_pop)

# 3: Which county in the USA has the highest prison rate, per year since 1990,
# what is the rate?
prison_rate_highest_county <- prison_jail_rate_1990 %>%
  mutate(total_prison_pop_rate = replace_na(total_prison_pop_rate, 0)) %>%
  group_by(year) %>%
  filter(total_prison_pop_rate == max(total_prison_pop_rate)) %>%
  filter(total_prison_pop_rate != 0) %>% 
  select(-c(total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate)) %>%
  select(-c(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate)) %>%
  select(-c(native_jail_pop_rate, white_jail_pop_rate))

# 4: Which county in the USA has the highest jail rate, per year since 1990,
# what is the rate?
jail_rate_highest_county <- prison_jail_rate_1990 %>%
  mutate(total_jail_pop_rate = replace_na(total_jail_pop_rate, 0)) %>%
  group_by(year) %>%
  filter(total_jail_pop_rate == max(total_jail_pop_rate)) %>%
  filter(total_jail_pop_rate != 0) %>%
  select(-c(total_prison_pop_rate, female_prison_pop_rate)) %>%
  select(-c(male_prison_pop_rate, aapi_prison_pop_rate)) %>%
  select(-c(black_prison_pop_rate, latinx_prison_pop_rate)) %>%
  select(-c(native_prison_pop_rate, white_prison_pop_rate))

# 5: Which county has seen its prison rate grow the most since 1990, what is 
# the rate? (cutoff at 2016 for more reliable data)
prison_rate_growth_county <- prison_jail_rate_1990 %>%
  mutate(total_prison_pop_rate = replace_na(total_prison_pop_rate, 0)) %>%
  mutate(total_pop = replace_na(total_pop, 0)) %>%
  filter(year == 1990 | year == 2016) %>%
  arrange(county_name, state, year) %>%
  mutate(prison_rate_growth = total_prison_pop_rate - lag(total_prison_pop_rate, n = 1)) %>%
  mutate(total_pop_growth = total_pop - lag(total_pop, n = 1)) %>% 
  filter(year == 2016) %>% 
  arrange(desc(prison_rate_growth)) %>%
  select(-c(total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate)) %>%
  select(-c(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate)) %>%
  select(-c(native_jail_pop_rate, white_jail_pop_rate))

# 5.1: What is the net growth of prison incarceration in each state by county and
# incarceration rate.
prison_rate_growth_state <- prison_rate_growth_county %>%
  group_by(state) %>%
  summarize(counties_growing = sum(prison_rate_growth > 0) - sum(prison_rate_growth < 0),
            net_growth = sum(prison_rate_growth)
            )

# 6: Which county has seen its jail rate grow the most since 1990, what is 
# the rate? (cutoff at 2016 for more reliable data)
jail_rate_growth_county <- prison_jail_rate_1990 %>%
  mutate(total_jail_pop_rate = replace_na(total_jail_pop_rate, 0)) %>%
  mutate(total_pop = replace_na(total_pop, 0)) %>%
  filter(year == 1990 | year == 2016) %>%
  arrange(county_name, state, year) %>%
  mutate(jail_rate_growth = total_jail_pop_rate - lag(total_jail_pop_rate, n = 1)) %>%
  mutate(total_pop_growth = total_pop - lag(total_pop, n = 1)) %>% 
  filter(year == 2016) %>% 
  arrange(desc(jail_rate_growth)) %>%
  select(-c(total_prison_pop_rate, female_prison_pop_rate)) %>%
  select(-c(male_prison_pop_rate, aapi_prison_pop_rate)) %>%
  select(-c(black_prison_pop_rate, latinx_prison_pop_rate)) %>%
  select(-c(native_prison_pop_rate, white_prison_pop_rate))

# 6.1: What is the net growth of jail incarceration in each state by county and
# incarceration rate.
jail_rate_growth_state <- jail_rate_growth_county %>%
  group_by(state) %>%
  summarize(counties_growing = sum(jail_rate_growth > 0) - sum(jail_rate_growth < 0),
            net_growth = sum(jail_rate_growth)
  )

# 7: Which county has the largest proportion Black and Indigenous people in 
# prison for each year since 1990, what is the proportion?
prison_rate_bipoc_county <- prison_jail_rate_1990 %>%
  mutate(bipoc_prison_pop_rate = replace_na(bipoc_prison_pop_rate, 0)) %>%
  group_by(year) %>%
  filter(bipoc_prison_pop_rate == max(bipoc_prison_pop_rate)) %>%
  filter(bipoc_prison_pop_rate > 0) %>%
  select(-c(total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate)) %>%
  select(-c(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate)) %>%
  select(-c(native_jail_pop_rate, white_jail_pop_rate))

# 8: Which county has the largest proportion Black and Indigenous people in 
# jail for each year since 1990, what is the proportion?
jail_rate_bipoc_county <- prison_jail_rate_1990 %>%
  mutate(bipoc_jail_pop_rate = replace_na(bipoc_jail_pop_rate, 0)) %>%
  group_by(year) %>%
  filter(bipoc_jail_pop_rate == max(bipoc_jail_pop_rate)) %>%
  filter(bipoc_jail_pop_rate > 0) %>%
  select(-c(total_prison_pop_rate, female_prison_pop_rate)) %>%
  select(-c(male_prison_pop_rate, aapi_prison_pop_rate)) %>%
  select(-c(black_prison_pop_rate, latinx_prison_pop_rate)) %>%
  select(-c(native_prison_pop_rate, white_prison_pop_rate))

# 9: Which county has the largest proportion white people in prison for each 
# year since 1990, what is the proportion?
prison_rate_white_county <- prison_jail_rate_1990 %>%
  mutate(white_prison_pop_rate = replace_na(white_prison_pop_rate, 0)) %>%
  group_by(year) %>%
  filter(white_prison_pop_rate == max(white_prison_pop_rate)) %>%
  filter(white_prison_pop_rate > 0) %>%
  select(-c(total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate)) %>%
  select(-c(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate)) %>%
  select(-c(native_jail_pop_rate, white_jail_pop_rate))

# 10: Which county has the largest proportion white people in jail for each 
# year since 1990, what is the proportion?
jail_rate_white_county <- prison_jail_rate_1990 %>%
  mutate(white_jail_pop_rate = replace_na(white_jail_pop_rate, 0)) %>%
  group_by(year) %>%
  filter(white_jail_pop_rate == max(white_jail_pop_rate)) %>%
  filter(white_jail_pop_rate > 0) %>%
  select(-c(total_prison_pop_rate, female_prison_pop_rate)) %>%
  select(-c(male_prison_pop_rate, aapi_prison_pop_rate)) %>%
  select(-c(black_prison_pop_rate, latinx_prison_pop_rate)) %>%
  select(-c(native_prison_pop_rate, white_prison_pop_rate))

# 11: Which county in each state has the largest rate of prisoners for 
# each year since 1990, what is the rate?

# 12: Which county in each state has the largest rate of people in jail 
# for each year since 1990, what is the rate?

# 13: Which counties have the most missing demographic data for their prisons 
# since 1990, how many values are missing?

# 14: Which counties have the most missing demographic data for their jails 
# since 1990, how many values are missing?