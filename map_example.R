source("analysis.R")
library(ggmap)

# Load data and unpack it:
inc_tables <- load_data()
prison_pop <- inc_tables[[1]]
jail_pop <- inc_tables[[2]]
prison_jail_rate_1990 <- inc_tables[[3]]

# Use ggmap here:
