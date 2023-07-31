source("summary.R")
library(ggplot2)

# Load data and unpack it:
inc_tables <- load_data()
prison_pop <- inc_tables[[1]]
jail_pop <- inc_tables[[2]]
prison_jail_rate_1990 <- inc_tables[[3]]

inc_growth_by_state <- inc_growth_state(prison_jail = prison_jail_rate_1990)
inc_growth_prison_top_10 <- inc_growth_by_state %>%
  arrange(desc(net_prison_growth)) %>%
  top_n(10, net_prison_growth)

ggplot(data = inc_growth_prison_top_10) +
  geom_point(aes(x = total_pop_growth, y = county_prisons_growing, 
                 size = net_prison_growth, colour = state)) +
  scale_y_continuous(breaks = seq(0, 200, 50), limits = c(0, 200)) +
  scale_x_continuous(
    breaks = seq(0, 11000000, 1000000), limits = c(0, 11000000)
  ) +
  labs(title = "Is population growth related to prison rate growth?", 
       subtitle = "Top 10 states with growing incarceration since 1990", 
       x = "Net Population Growth", y = "Number of Prisons Growing") +
  guides(colour = guide_legend("State"),
         size = guide_legend("Incarceration Rate Growth"),
         x = guide_axis(angle = 90))
