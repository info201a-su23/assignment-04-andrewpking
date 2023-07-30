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

# Aggregate data for BIPOC communities in prison_pop
prison_pop <- prison_pop %>%
  mutate(bipoc_prison_pop = black_prison_pop + native_prison_pop)

# Aggregate data for BIPOC communities in jail_pop
jail_pop <- jail_pop %>%
  mutate(bipoc_jail_pop = black_jail_pop + native_jail_pop)

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

# Questions for visualization:
# 1: Which counties in the USA has the highest prison population, 
# since 1970, what is the population?
prison_pop_highest_county <- prison_pop %>%
  mutate(
    location = paste(county_name, state), 
    total_prison_pop = replace_na(total_prison_pop, 0)
  ) %>%
  filter(total_prison_pop != 0) %>%
  filter(total_prison_pop == max(total_prison_pop), .by = location) %>%
  arrange(desc(total_prison_pop)) %>%
  top_n(10, total_prison_pop)

# 1.1: What is the proportion of White vs BIPOC people in the most populated
# prison counties?
white_vs_bipoc_highest_prison <- prison_pop_highest_county %>%
  reframe(year, 
          location, 
          total_prison_pop, 
          white_prison_pop, 
          bipoc_prison_pop) %>%
  mutate(bipoc_prison_pop = replace_na(total_prison_pop - white_prison_pop))

# 2: Which counties in the USA has the highest jail population, 
# since 1970, what is the population?
jail_pop_highest_county <- jail_pop %>%
  mutate(
    location = paste(county_name, state), 
    total_jail_pop = replace_na(total_jail_pop, 0)
  ) %>%
  filter(total_jail_pop != 0) %>%
  filter(total_jail_pop == max(total_jail_pop), .by = location) %>%
  arrange(desc(total_jail_pop)) %>%
  top_n(10, total_jail_pop)

# 2.1: What is the proportion of White vs BIPOC people in the most populated
# jail counties?
white_vs_bipoc_highest_jail <- jail_pop_highest_county %>%
  reframe(year, 
          location, 
          total_jail_pop, 
          white_jail_pop, 
          bipoc_jail_pop) %>%
  mutate(bipoc_jail_pop = replace_na(total_jail_pop - white_jail_pop))

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
prison_rate_state_county <- prison_jail_rate_1990 %>%
  mutate(total_prison_pop_rate = replace_na(total_prison_pop_rate, 0)) %>%
  group_by(year, state) %>%
  filter(total_prison_pop_rate == max(total_prison_pop_rate)) %>%
  filter(total_prison_pop_rate != 0) %>% 
  select(-c(total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate)) %>%
  select(-c(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate)) %>%
  select(-c(native_jail_pop_rate, white_jail_pop_rate)) %>%
  arrange(state, year)

# 12: Which county in each state has the largest rate of people in jail 
# for each year since 1990, what is the rate?
jail_rate_state_county <- prison_jail_rate_1990 %>%
  mutate(total_jail_pop_rate = replace_na(total_jail_pop_rate, 0)) %>%
  group_by(year, state) %>%
  filter(total_jail_pop_rate == max(total_jail_pop_rate)) %>%
  filter(total_jail_pop_rate != 0) %>%
  select(-c(total_prison_pop_rate, female_prison_pop_rate)) %>%
  select(-c(male_prison_pop_rate, aapi_prison_pop_rate)) %>%
  select(-c(black_prison_pop_rate, latinx_prison_pop_rate)) %>%
  select(-c(native_prison_pop_rate, white_prison_pop_rate)) %>%
  arrange(state, year)

# 13: How many mens only, womens only, and coed prisons are there for each year?
# Additionally how many men and women are in prison?
prison_gender <- prison_jail_rate_1990 %>%
  mutate(male_prison_pop_rate = replace_na(male_prison_pop_rate, 0),
         female_prison_pop_rate = replace_na(female_prison_pop_rate, 0)
         ) %>%
  group_by(year) %>%
  summarise(
    men_only_prison = sum(
      male_prison_pop_rate > 0 & female_prison_pop_rate == 0
      ), 
    men_in_prison_rate = sum(male_prison_pop_rate),
    women_only_prison = sum(
      female_prison_pop_rate > 0 & male_prison_pop_rate == 0
    ),
    women_in_prison_rate = sum(female_prison_pop_rate),
    coed_prisons = sum(
      male_prison_pop_rate > 0 & female_prison_pop_rate > 0
      )
    ) %>%
  mutate(people_in_prison_rate = women_in_prison_rate + men_in_prison_rate)

# 14: How many mens only, womens only, and coed jails are there for each year?
# Additionally how many men and women are in jail?
jail_gender <- prison_jail_rate_1990 %>%
  mutate(male_jail_pop_rate = replace_na(male_jail_pop_rate, 0),
         female_jail_pop_rate = replace_na(female_jail_pop_rate, 0)
  ) %>%
  group_by(year) %>%
  summarise(
    men_only_jail = sum(
      male_jail_pop_rate > 0 & female_jail_pop_rate == 0
    ), 
    men_in_jail_rate = sum(male_jail_pop_rate),
    women_only_jail = sum(
      female_jail_pop_rate > 0 & male_jail_pop_rate == 0
    ),
    women_in_jail_rate = sum(female_jail_pop_rate),
    coed_jails = sum(
      male_jail_pop_rate > 0 & female_jail_pop_rate > 0
    )
  ) %>%
  mutate(people_in_jail_rate = women_in_jail_rate + men_in_jail_rate)

# Observations from data:
# The largest prisons do not collect data on BIPOC prisoners but do on whites.
# Incarceration levels are growing in all states except NJ and DC.
# States with more white people have disproportionately high levels of BIPOC
# people incarcerated.
# LA county jails and imprisons more people than any other place in the USA,
# surpassing New York County in the 1970s.
# There are more mens only prisons than womens only prisons by county.
# There are more men jailed or imprisoned than women per year.
# Rural counties are disproportionately housing prisoners over urban counties.

# Summary statistics:
# 1: What is the percentage increase of total incarceration from 1970 to 2018?

incarceration_1970 <- prison_pop %>%
  filter(year == 1970)

incarceration_2018 <- prison_pop %>%
  filter(year == 2018)

# 1.1: What is the total incarceration for 1970 and 2018:
incarceration_total_1970 <- sum(incarceration_1970$total_pop)
incarceration_total_2018 <- sum(incarceration_2018$total_pop)

total_increase_ratio <- (incarceration_total_2018 / incarceration_total_1970)
total_increase_percent <- (total_increase_ratio - 1) * 100

# 2: How has the proportions changed for bipoc vs white people changed since 
# 1970
# 1.1: What is the population of white people vs bipoc people incarcerated
# in 1990?
incarceration_1990 <- prison_jail_rate_1990 %>%
  filter(year == 1990)

bipoc_avg_prison_1990 <- mean(incarceration_1990$bipoc_prison_pop_rate, 
                              na.rm = TRUE)
white_avg_prison_1990 <- mean(incarceration_1990$white_prison_pop_rate, 
                              na.rm = TRUE)
bipoc_avg_jail_1990 <- mean(incarceration_1990$bipoc_jail_pop_rate, 
                            na.rm = TRUE)
white_avg_jail_1990 <- mean(incarceration_1990$white_jail_pop_rate, 
                            na.rm = TRUE)

# 2.2: What is the average proportion of white people vs bipoc people 
# incarcerated in 2016?
incarceration_2016 <- prison_jail_rate_1990 %>%
  filter(year == 2016)

bipoc_avg_prison_2016 <- mean(incarceration_2016$bipoc_prison_pop_rate, 
                              na.rm = TRUE)

white_avg_prison_2016 <- mean(incarceration_2016$white_prison_pop_rate, 
                              na.rm = TRUE)
bipoc_avg_jail_2016 <- mean(incarceration_2016$bipoc_jail_pop_rate, 
                            na.rm = TRUE)
white_avg_jail_2016 <- mean(incarceration_2016$white_jail_pop_rate, 
                            na.rm = TRUE)

# 2.3: What is the change in proportion of the population over time as a number?
bipoc_avg_prison_change <- bipoc_avg_prison_2016 - bipoc_avg_prison_1990
white_avg_prison_change <- white_avg_prison_2016 - white_avg_prison_1990
bipoc_avg_jail_change <- bipoc_avg_jail_2016 - bipoc_avg_jail_1990
white_avg_jail_change <- white_avg_jail_2016 - white_avg_jail_1990
