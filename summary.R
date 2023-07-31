library(tidyverse)

# Import the datasets
load_data <- function(){
  prison_pop <- read_csv(
    "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv"
    )
  jail_pop <- read_csv(
    "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-jail-pop.csv"
  )
  prison_jail_rate_1990 <- read_csv(
    "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990.csv"
  )

  # Aggregate data for BIPOC communities in prison_jail_rate_1990
  prison_jail_rate_1990 <- prison_jail_rate_1990 %>%
    mutate(bipoc_jail_pop_rate = black_jail_pop_rate + native_jail_pop_rate) %>%
    mutate(
      bipoc_prison_pop_rate = black_prison_pop_rate + native_prison_pop_rate
      )

  # Aggregate data for BIPOC communities in prison_pop
  prison_pop <- prison_pop %>%
    mutate(bipoc_prison_pop = black_prison_pop + native_prison_pop)

  # Aggregate data for BIPOC communities in jail_pop
  jail_pop <- jail_pop %>%
    mutate(bipoc_jail_pop = black_jail_pop + native_jail_pop)
  
  tables <- list(
    prison_pop = prison_pop,
    jail_pop = jail_pop,
    prison_jail_rate_1990 = prison_jail_rate_1990
    )
  return(tables)
}

get_dimensions <- function(
    prison = prison_pop, 
    jail = jail_pop,
    prison_jail = prison_jail_rate_1990
  )
{
  # Get total columns for each data-set
  prison_pop_col <- ncol(prison)
  jail_pop_col <- ncol(jail)
  prison_jail_rate_1990_col <- ncol(prison_jail)
  
  # Get total rows for each data-set
  prison_pop_row <- nrow(prison)
  jail_pop_row <- nrow(jail)
  prison_jail_rate_1990_row <- nrow(prison_jail)
  
  # Create a table of dimensions to return
  dimensions <- data.frame(
    `Dimensions` = c("Columns", "Rows"),
    `prison_pop` = c(prison_pop_col, prison_pop_row),
    `jail_pop` = c(jail_pop_col, jail_pop_row),
    `prison_jail_rate_1990` = c(
      prison_jail_rate_1990_col, 
      prison_jail_rate_1990_row
      )
  )
  return(dimensions)
}

# Get column names for each data-set
get_col_names <- function(
    prison = prison_pop,
    jail = jail_pop,
    prison_jail = prison_jail_rate_1990
    )
{
  prison_pop_col_names <- colnames(prison)
  jail_pop_col_names <- colnames(jail)
  prison_jail_rate_1990_col_names <- colnames(prison_jail)
}

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
# 1: Which prisons and jails have the highest populations of people 
# incarcerated?
# 1.1: Which top 10 counties in the USA has the highest prison population, 
# since 1970, what is the population?
prison_highest <- function(prison = prison_pop){
  prison_pop_highest_county <- prison %>%
    mutate(
      location = paste(county_name, state), 
      total_prison_pop = replace_na(total_prison_pop, 0)
    ) %>%
    filter(total_prison_pop != 0) %>%
    filter(total_prison_pop == max(total_prison_pop), .by = location) %>%
    arrange(desc(total_prison_pop)) %>%
    top_n(10, total_prison_pop)
  
  # 1.1: What is the proportion of White vs BIPOC people in the most populated
  # prison counties? (Include table in document)
  white_vs_bipoc_highest_prison <- prison_pop_highest_county %>%
    reframe(year, 
            prison_location = location, 
            total_prison_pop, 
            white_prison_pop, 
            bipoc_prison_pop) %>%
    mutate(bipoc_prison_pop = replace_na(total_prison_pop - white_prison_pop))
  return(white_vs_bipoc_highest_prison)
}

# 1.2: Which 10 counties in the USA has the highest jail populations, 
# since 1970, what is the population?
jail_highest <- function(jail = jail_pop){
  jail_pop_highest_county <- jail %>%
    mutate(
      location = paste(county_name, state), 
      total_jail_pop = replace_na(total_jail_pop, 0)
    ) %>%
    filter(total_jail_pop != 0) %>%
    filter(total_jail_pop == max(total_jail_pop), .by = location) %>%
    arrange(desc(total_jail_pop)) %>%
    top_n(10, total_jail_pop)
  
  # 1.3: What is the proportion of White vs BIPOC people in the most populated
  # jail counties? (Include table in document)
  white_vs_bipoc_highest_jail <- jail_pop_highest_county %>%
    reframe(year, 
            jail_location = location, 
            total_jail_pop, 
            white_jail_pop, 
            bipoc_jail_pop) %>%
    mutate(bipoc_jail_pop = replace_na(total_jail_pop - white_jail_pop))
  return(white_vs_bipoc_highest_jail)
}

# 1.3 Return a list of both tables
prison_jail_highest <- function(prison = prison_pop, jail = jail_pop){
  prison_most <- prison_highest(prison)
  jail_most <- jail_highest(jail)
  highest <- list(prison_most, jail_most)
  return(highest)
}

# 2: What are the counties with highest prison and jail rates in the USA?
highest_rate_by_year <- function(prison_jail = prison_jail_rate_1990){
  # 2.1: Which county in the USA has the highest prison rate, per year since
  # 1990, what is the rate?
  prison <- prison_jail %>%
    mutate(total_prison_pop_rate = replace_na(total_prison_pop_rate, 0)) %>%
    group_by(year) %>%
    filter(total_prison_pop_rate == max(total_prison_pop_rate)) %>%
    filter(total_prison_pop_rate != 0) %>% 
    select(
      -c(
        total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate,
        aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate,
        native_jail_pop_rate, white_jail_pop_rate, bipoc_jail_pop_rate
        )
      )
  
  # 2.2: Which county in the USA has the highest jail rate, per year since 1990,
  # what is the rate?
  jail <- prison_jail %>%
    mutate(total_jail_pop_rate = replace_na(total_jail_pop_rate, 0)) %>%
    group_by(year) %>%
    filter(total_jail_pop_rate == max(total_jail_pop_rate)) %>%
    filter(total_jail_pop_rate != 0) %>%
    select(
      -c(
        total_prison_pop_rate, female_prison_pop_rate, male_prison_pop_rate,
        aapi_prison_pop_rate, black_prison_pop_rate, latinx_prison_pop_rate,
        native_prison_pop_rate, white_prison_pop_rate, bipoc_prison_pop_rate
        )
      )
  
  highest_rates <- left_join(x = prison, y = jail,
                             by = "year", suffix = c(".p", ".j")
                             )
  return(highest_rates)
}

# 3: Which county has seen its incarceration rate grow the most since 1990, 
# what is the rate? (cutoff at 2016 for more reliable data)
# Is this growth rate related to population growth?
# 3.1 How much has the prison rate grown for each county between 1990 and 2016?
prison_rate_growth_county <- function(
    prison_jail = prison_jail_rate_1990,
    start_year = 1990,
    end_year = 2016
    )
{
  prison_growth_rate <- prison_jail %>%
    mutate(total_prison_pop_rate = replace_na(total_prison_pop_rate, 0)) %>%
    mutate(total_pop = replace_na(total_pop, 0)) %>%
    filter(year == start_year | year == end_year) %>%
    arrange(county_name, state, year) %>%
    mutate(prison_rate_growth = total_prison_pop_rate - lag(total_prison_pop_rate, n = 1)) %>%
    mutate(total_pop_growth = total_pop - lag(total_pop, n = 1)) %>% 
    filter(year == end_year) %>% 
    select(-c(total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate)) %>%
    select(-c(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate)) %>%
    select(-c(native_jail_pop_rate, white_jail_pop_rate))
  return(prison_growth_rate)
}

# 3.2: What is the net growth of prison incarceration in each state by county and
# incarceration rate. (use for creating a map)
prison_rate_growth_state <- function(
    prison_jail = prison_jail_rate_1990,
    start_year = 1990,
    end_year = 2016
    )
{
  prison_growth_rate <- prison_rate_growth_county(
    prison_jail = prison_jail, start_year = start_year, end_year = end_year) %>%
    group_by(state) %>%
    summarize(county_prisons_growing = sum(
      prison_rate_growth > 0) - sum(prison_rate_growth < 0),
              net_prison_growth = sum(prison_rate_growth)
    )
  return(prison_growth_rate)
}

# 3.3: What is the growth by county of jail rates from 1990 to 2016?
jail_rate_growth_county <- function(
    prison_jail = prison_jail_rate_1990,
    start_year = 1990,
    end_year = 2016)
{
  jail_growth_rate <- prison_jail %>%
    mutate(total_jail_pop_rate = replace_na(total_jail_pop_rate, 0)) %>%
    mutate(total_pop = replace_na(total_pop, 0)) %>%
    filter(year == start_year | year == end_year) %>%
    arrange(county_name, state, year) %>%
    mutate(jail_rate_growth = total_jail_pop_rate - lag(total_jail_pop_rate, n = 1)) %>%
    mutate(total_pop_growth = total_pop - lag(total_pop, n = 1)) %>% 
    filter(year == end_year) %>% 
    select(-c(total_prison_pop_rate, female_prison_pop_rate)) %>%
    select(-c(male_prison_pop_rate, aapi_prison_pop_rate)) %>%
    select(-c(black_prison_pop_rate, latinx_prison_pop_rate)) %>%
    select(-c(native_prison_pop_rate, white_prison_pop_rate))
  return(jail_growth_rate)
}

# 3.4: What is the net growth of jail incarceration in each state by county and
# incarceration rate (use for creating a map)
jail_rate_growth_state <- function(
    prison_jail = prison_jail_rate_1990,
    start_year = 1990,
    end_year = 2016)
{
  jail_growth_rate <- jail_rate_growth_county(
    prison_jail = prison_jail, start_year = start_year, end_year = end_year) %>%
    group_by(state) %>%
    summarize(county_jails_growing = sum(jail_rate_growth > 0) - sum(jail_rate_growth < 0),
              net_jail_growth = sum(jail_rate_growth)
    )
  return(jail_growth_rate)
}

# 3.5: Join the tables together to create a scatter-plot and map:
inc_growth_county <- function(
    prison_jail = prison_jail_rate_1990,
    start_year = 1990,
    end_year = 2016)
{
  inc_rate_growth <- left_join(
    prison_rate_growth_county(
      prison_jail = prison_jail, start_year = start_year, end_year = end_year
      ), 
    jail_rate_growth_county(
      prison_jail = prison_jail, start_year = start_year, end_year = end_year
      )
    ) %>% 
    select(
      year, 
      county_name,
      state,
      total_pop_growth,
      prison_rate_growth,
      jail_rate_growth
    ) %>%
    arrange(desc(total_pop_growth))
  return(inc_rate_growth)
}


inc_growth_state <- function(
    prison_jail = prison_jail_rate_1990, 
    start_year = 1990, 
    end_year = 2016)
{
  inc_growth <- left_join(
    prison_rate_growth_state(
      prison_jail = prison_jail, start_year = start_year, end_year = end_year
    ), 
    jail_rate_growth_state(
      prison_jail = prison_jail, start_year = start_year, end_year = end_year
    )
  )
}

# 4: What is the proportion of black vs white people in prison and jail?
inc_rate_by_race <- function(prison_jail = prison_jail_rate_1990){
  # 4.1: What is the proportion Black and Indigenous people in prison for each year 
  # since 1990?
  prison_rate_bipoc_year <- prison_jail %>%
    mutate(bipoc_prison_pop_rate = replace_na(bipoc_prison_pop_rate, 0)) %>%
    group_by(year) %>%
    summarize(bipoc_prison_pop_rate = sum(bipoc_prison_pop_rate))
  
  # 4.2: What is the proportion Black and Indigenous people in jail for each year
  # since 1990, what is the proportion?
  jail_rate_bipoc_year <- prison_jail %>%
    mutate(bipoc_jail_pop_rate = replace_na(bipoc_jail_pop_rate, 0)) %>%
    group_by(year) %>%
    summarize(bipoc_jail_pop_rate = sum(bipoc_jail_pop_rate))
  
  # 4.3: Which county has the largest proportion white people in prison for each 
  # year since 1990, what is the proportion?
  prison_rate_white_year <- prison_jail %>%
    mutate(white_prison_pop_rate = replace_na(white_prison_pop_rate, 0)) %>%
    group_by(year) %>%
    summarize(white_prison_pop_rate = sum(white_prison_pop_rate))
  
  # 4.4: Which county has the largest proportion white people in jail for each 
  # year since 1990, what is the proportion?
  jail_rate_white_year <- prison_jail %>%
    mutate(white_jail_pop_rate = replace_na(white_jail_pop_rate, 0)) %>%
    group_by(year) %>%
    summarize(white_jail_pop_rate = sum(white_jail_pop_rate))
  
  # 4.5: Join the tables together for time series data:
  inc_rate_white <- left_join(prison_rate_white_year, jail_rate_white_year)
  inc_rate_bipoc <- left_join(prison_rate_bipoc_year, jail_rate_bipoc_year)
  inc_rate <- left_join(inc_rate_white, inc_rate_bipoc)
  return(inc_rate)
}

# 5: What gender disparities exist in prison?
inc_rate_by_gender <- function(prison_jail = prison_jail_rate_1990){
  # 5.1: How many mens only, womens only, and coed prisons are there for each year?
  # Additionally how many men and women are in prison?
  prison_gender <- prison_jail %>%
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
    
    # 5.1: How many mens only, womens only, and coed jails are there for each year?
    # Additionally how many men and women are in jail?
    jail_gender <- prison_jail %>%
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
    
    # 5.2: Join the gender info tables together for time series data
    gender_info <- left_join(prison_gender, jail_gender)
    return(gender_info)
}

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
# 1: What is the percentage increase of prison incarceration from 1970 to 2018?
increase_percent <- function(prison_table = prison_pop){
  incarceration_1970 <- prison_table %>%
    filter(year == 1970)

  incarceration_2018 <- prison_table %>%
    filter(year == 2018)

  # 1.1: What is the total incarceration for 1970 and 2018:
  incarceration_total_1970 <- sum(incarceration_1970$total_pop)
  incarceration_total_2018 <- sum(incarceration_2018$total_pop)

  total_increase_ratio <- (incarceration_total_2018 / incarceration_total_1970)
  total_increase_percent <- (total_increase_ratio - 1) * 100
  return(total_increase_percent)
}

# 2: How has the proportions changed for bipoc vs white people changed since 
# 1970
change_inc_white_vs_bipoc <- function(
  inc_rate = prison_jail_rate_1990,
  start_year = 1990,
  end_year = 2016
  ){
    incarceration_1990 <- inc_rate %>%
      filter(year == start_year)
    # 2.1: What is the population of white people vs bipoc people incarcerated
    # in 1990?
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
    incarceration_2016 <- inc_rate %>%
      filter(year == end_year)

    bipoc_avg_prison_2016 <- mean(
      incarceration_2016$bipoc_prison_pop_rate, 
      na.rm = TRUE
      )

    white_avg_prison_2016 <- mean(
      incarceration_2016$white_prison_pop_rate, 
      na.rm = TRUE
      )
    bipoc_avg_jail_2016 <- mean(
      incarceration_2016$bipoc_jail_pop_rate, 
      na.rm = TRUE
      )
    white_avg_jail_2016 <- mean(
      incarceration_2016$white_jail_pop_rate, 
      na.rm = TRUE
      )

    # 2.3: What is the change in proportion of the population over time as a 
    # number?
    bipoc_avg_prison_change <- bipoc_avg_prison_2016 - bipoc_avg_prison_1990
    white_avg_prison_change <- white_avg_prison_2016 - white_avg_prison_1990
    bipoc_avg_jail_change <- bipoc_avg_jail_2016 - bipoc_avg_jail_1990
    white_avg_jail_change <- white_avg_jail_2016 - white_avg_jail_1990
  
    inc_change <- data.frame(
      Race = c("BIPOC", "White"),
      Avg_Prison_Change = c(bipoc_avg_prison_change, white_avg_prison_change),
      Avg_Jail_Change = c(bipoc_avg_jail_change, white_avg_jail_change)
  )
  return(inc_change)
}

# Unused data sets
# prison_jail_rate_1990_WA <- read_csv(
#   "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv"
# )
# prison_jail_rate <- read_csv(
#   "https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates.csv"
# 
# Unused dims:
# # prison_jail_rate_col <- ncol(prison_jail_rate)
# # prison_jail_rate_1990_WA_col <- ncol(prison_jail_rate_1990_WA)
# # prison_jail_rate_row <- nrow(prison_jail_rate)
# # prison_jail_rate_1990_WA_row <- nrow(prison_jail_rate_1990_WA)
#
# Unused colnames:
# # prison_jail_rate_col_names <- colnames(prison_jail_rate)
# # prison_jail_rate_1990_WA_col_names <- colnames(prison_jail_rate_1990_WA)
# 
# # Unused code: Which county in each state has the largest rate of prisoners for 
# # each year since 1990, what is the rate?
# prison_rate_state_county <- prison_jail_rate_1990 %>%
#   mutate(total_prison_pop_rate = replace_na(total_prison_pop_rate, 0)) %>%
#   group_by(year, state) %>%
#   filter(total_prison_pop_rate == max(total_prison_pop_rate)) %>%
#   filter(total_prison_pop_rate != 0) %>% 
#   select(-c(total_jail_pop_rate, female_jail_pop_rate, male_jail_pop_rate)) %>%
#   select(-c(aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate)) %>%
#   select(-c(native_jail_pop_rate, white_jail_pop_rate)) %>%
#   arrange(state, year)
# 
# # Unused code: Which county in each state has the largest rate of people in jail 
# # for each year since 1990, what is the rate?
# jail_rate_state_county <- prison_jail_rate_1990 %>%
#   mutate(total_jail_pop_rate = replace_na(total_jail_pop_rate, 0)) %>%
#   group_by(year, state) %>%
#   filter(total_jail_pop_rate == max(total_jail_pop_rate)) %>%
#   filter(total_jail_pop_rate != 0) %>%
#   select(-c(total_prison_pop_rate, female_prison_pop_rate)) %>%
#   select(-c(male_prison_pop_rate, aapi_prison_pop_rate)) %>%
#   select(-c(black_prison_pop_rate, latinx_prison_pop_rate)) %>%
#   select(-c(native_prison_pop_rate, white_prison_pop_rate)) %>%
#   arrange(state, year)

