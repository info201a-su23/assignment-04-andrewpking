source("summary.R")

# Load data and unpack it:
inc_tables <- load_data()
prison_pop <- inc_tables[[1]]
jail_pop <- inc_tables[[2]]
prison_jail_rate_1990 <- inc_tables[[3]]

x_values <- seq(1, 3)
y_values <- seq(1,3)

library(ggplot2)
ggplot() +
  geom_line(aes(x=x_values, y = y_values))
