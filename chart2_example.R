source("summary.R")
library(ggplot2)

# Load data and unpack it:
inc_tables <- load_data()
prison_pop <- inc_tables[[1]]
jail_pop <- inc_tables[[2]]
prison_jail_rate_1990 <- inc_tables[[3]]

inc_growth_by_state <- inc_growth_state(prison_jail = prison_jail_rate_1990)

ggplot(data = inc_growth_by_state) +
  geom_point(aes(x = net_prison_growth, y = county_prisons_growing, 
                 size = total_pop_growth, colour = state))
