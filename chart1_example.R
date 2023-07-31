source("summary.R")
library(ggplot2)

# Load data and unpack it:
inc_tables <- load_data()
prison_pop <- inc_tables[[1]]
jail_pop <- inc_tables[[2]]
prison_jail_rate_1990 <- inc_tables[[3]]

# Create time series data for incarceration by race
inc_by_race <- inc_rate_by_race(prison_jail = prison_jail_rate_1990)
inc_by_race <- inc_by_race %>%
  gather(key = "race", value = "population", -year) %>%
  mutate(population = na_if(population, 0))

ggplot(data = inc_by_race) +
  geom_line(aes(x = year, y = population, colour = race))
