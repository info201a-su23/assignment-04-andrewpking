source("analysis.R")
library(ggplot2)

# Process the data for graphing
inc_growth_by_state <- inc_growth_state()
inc_growth_prison_top_10 <- inc_growth_by_state %>%
  arrange(desc(avg_prison_growth)) %>%
  top_n(10, avg_prison_growth)

# Plot the data
pop_vs_prison <- ggplot(
    data = inc_growth_prison_top_10, 
    aes(x = total_pop_growth, y = avg_prison_growth)) +
  geom_point(aes(size = county_prisons_growing, colour = state)) +
  scale_y_continuous(
    breaks = seq(0, 40000000, 5000000), limits = c(0, 40000000)) +
  scale_x_continuous(
     breaks = seq(0, 11000000, 1000000), limits = c(0, 11000000)) +
  labs(title = "Is population growth related to prison growth?", 
       subtitle = "Top 10 states with growing incarceration since 1990", 
       x = "Net Population Growth since 1990", 
       y = "Average Incarceration growth by since 1990") +
  guides(colour = guide_legend("State"),
         size = guide_legend("Counties with Prison Growth"),
         x = guide_axis(angle = 90)) +
  geom_smooth(orientation = "x", method = "lm", level = 0)
pop_vs_prison
