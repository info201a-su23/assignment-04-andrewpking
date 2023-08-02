source("analysis.R")
library(ggplot2)
library(ggmap)

# Get the data to plot:
state_growth <- inc_growth_state()

# Use ggmap here:
# 1.  First, create the maps data frame:
states_map <- map_data("state")
# Create a data frame of map data

#2. Create a data frame with the values for color intensity:
# state.name is a built-in dataset! 
region <- tolower(state.name)

state_growth <- state_growth %>%
  filter(state != "DC") %>%
  mutate(
    avg_prison_growth = ifelse(
      avg_prison_growth > 0, log(avg_prison_growth), avg_prison_growth
      ),
    avg_prison_growth = ifelse(
      avg_prison_growth < 0, -log(-avg_prison_growth), avg_prison_growth)
    )

  
#creating a 50x2 data frame here:
states_growing <- data.frame(
  state = region, value = as.integer(state_growth$avg_prison_growth))
  

#Plot the choropleth! 
mp <- ggplot(states_growing, aes(map_id = state)) +
  geom_map(aes(fill = value), map = states_map)
mp <- mp + expand_limits(x = states_map$long, y = states_map$lat)
mp <- mp + scale_fill_gradient2(
  low = '#3322E6', high = '#E0C736', mid = "#E3E1D5", limits = c(-20, 20),
  midpoint = 0) +
  coord_map() +
  theme_void() + 
  labs(title = "Where in the USA has mass incarceration grown since 1990?", 
       subtitle = "Average prison population growth per county by state", 
       ) +
  guides(fill = guide_legend("Prison Growth (log)"))
mp
