source("analysis.R")
library(ggplot2)
  
# Create time series data for incarceration by race
inc_by_race <- inc_rate_by_race()
  
# Plot the data in a human readable form:
ggplot(data = inc_by_race) +
    geom_line(aes(x = year, y = population, 
                  colour = race, linetype = incarceration_type)) +
    scale_x_continuous(breaks = seq(1990,2020,2), limits = c(1990, 2018)) +
    scale_y_continuous(
      breaks = seq(0, 8000000, 1000000), limits = c(0, 8000000)
    ) +
    labs(title = "Incarceration in USA by Race", 
         subtitle = "Aggregated from annual county incarceration data", 
         x = "Year", y = "Population") +
    guides(colour = guide_legend("Race"),
           linetype = guide_legend("Incarceration Type"),
           x = guide_axis(angle = 90))
