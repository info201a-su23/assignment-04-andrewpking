source("summary.R")
library(ggplot2)

# Load data and unpack it:
inc_tables <- load_data()
prison_pop <- inc_tables[[1]]
jail_pop <- inc_tables[[2]]
prison_jail_rate_1990 <- inc_tables[[3]]

# Create time series data for incarceration by race
inc_by_race <- inc_rate_by_race(prison_jail = prison_jail_rate_1990)

ggplot(data = inc_by_race) +
  geom_line(aes(x = year, y = population, 
                colour = race, linetype = incarceration_type)) +
  labs(title = "Inceration in USA by Race", 
       subtitle = "Aggregated from county incarceration data", 
       x = "Year", y = "Population") +
  guides(colour = guide_legend("Race"),
         linetype = guide_legend("Incarceration Type"))
