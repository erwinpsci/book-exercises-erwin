library(ggplot2)
midwest <- midwest

# plot the 'midwest' dataset, with college education rate on the x-axis and percentage of adult poverty on the y-axis
ggplot(data = midwest) + 
  geom_point(mapping = aes(x = percollege, y = percadultpoverty)) 

# a bar chart of the total population of each state
# the `state` is mapped to the x axis and the `poptotal` is mapped to the y-axis
ggplot(data = midwest) + 
  geom_col(mapping = aes(x = state, y = poptotal))

# a hexagonal aggregation that counts the co-occurence of college education rate and percentage of adult poverty
ggplot(data = midwest) + 
  geom_hex(mapping = aes(x = percollege, y = percadultpoverty)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Hexagonal Aggregation of College Education Rate vs Adult Poverty",
       x = "College Education Rate (%)",
       y = "Percentage of Adult Poverty (%)")

# a plot with both points and a smoothed line
ggplot(data = midwest) + 
  geom_point(mapping = aes(x = percollege, y = percadultpoverty)) + 
  geom_smooth(mapping = aes(x = percollege, y = percadultpoverty))

# a plot with both points and a smoothed line, sharing aestheic mappings
ggplot(data = midwest, mapping = aes(x = percollege, y = percadultpoverty)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  geom_point(mapping = aes(y = percchildbelowpovert), color = "red") # adding another point layer with a different y aesthetic

# change the color of each point based on the sate it is in
ggplot(data = midwest) + 
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = state) 
  )
# set a consistent color (red) for all points -- not driven by data
ggplot(data = midwest) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty), 
    color = "red",
    alpha = 0.5 # set transparency to 50%
  )

# 16.3 ----
library(dplyr)
library(tidyr)

# wrangle the data using dplyr and tidyr 
# select the columns of racial population totals, then gather () those column values into `race` and 'population` columns
state_race_long <- midwest %>% 
  select(state, popwhite, popblack, popamerindian, popasian, popother) %>% 
  gather(key = race, value = population, -state) # all columns except `state` are gathered into `race` and `population`
# create a stacked bar chart of the number of the people in each state
# fill the bar using different colors to show racial composition
ggplot(state_race_long) +
  geom_col(mapping = aes(x = state, y = population, fill = race)) # fill the bars by `race`

# create a percentage (filled) column of the population (by race) in each state
ggplot(state_race_long) + 
  geom_col( # geom_col() is used to create a bar chart
    mapping = aes(x = state, y = population, fill = race), position = "fill" # position = "fill" creates a percentage stacked bar chart
  )
# create a grouped (dodged) column of the number of people (by race) in each state
ggplot(state_race_long) +
  geom_col(
    mapping = aes(x= state, y = population, fill = race), position = "dodge" # position = "dodge" creates a grouped bar chart
  )

# plot the midwest dataset, with college education rate and percentage of adult poverty. Explicityly set the scales
ggplot(data = midwest) +
  geom_point(mapping = aes(x = percollege, y = percadultpoverty, colour = state)) +
  scale_x_continuous() + # This line means the x-axis will have a continuous scale
  scale_y_continuous() + # This line means the y-axis will have a continuous scale
  scale_color_discrete()# This line means the color will be discrete (different colors for different states).

# create a better label for the `inmetro` column
labeled <- midwest %>% 
  mutate(location = if_else(inmetro == 0, # if_else() is used to create a new column based on a condition, if_else() selects values based on a condition (inmetro == 0 or inmetro == 1), == 1 values will be labeled as "In Metro Area" and == 0 values will be labeled as "Not in Metro Area"
                            "Rural", "Urban")) # "Rural" for inmetro == 0 and "Urban" for inmetro == 1
# subset data by state
wisconsin_data <- labeled %>% filter(state == "WI") # filter() is used to select rows based on a condition, here we are filtering the data for the state of Wisconsin
michigan_data <- labeled %>% filter(state == "MI") # filter() is used to select rows based on a condition, here we are filtering the data for the state of Michigan
# define continuous scales based on the entire data set:
# ranage() produces a min max vector to use as the limits
x_scale <- scale_x_continuous(limits = range(labeled$percollege))
y_scale <- scale_y_continuous(limits = range(labeled$percadultpoverty))
# define a discrete color scale using the unique set of locations (urban/rural)
color_scale <- scale_color_discrete(limits= unique(labeled$location)) # unique() returns the unique values in the `location` column, which are "Urban" and "Rural"
# plot the wisconsin data with the defined scales
ggplot(data = wisconsin_data) +
  geom_point(mapping = aes(x = percollege, y = percadultpoverty, color = location)) +
  x_scale + # add the x scale
  y_scale + # add the y scale
  color_scale # add the color scale
# plot the michigan data with the same scales
ggplot(data = michigan_data) +
  geom_point(mapping = aes(x = percollege, y = percadultpoverty, color = location)) +
  x_scale + # add the x scale
  y_scale + # add the y scale
  color_scale # add the color scale

## color scales ----
# change the color of each point based on the state it is in
ggplot(data = midwest) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = state) 
  ) +
  scale_color_brewer(palette = "Set3") # use a color palette from the RColorBrewer package
# create a horizontal bar chart of the most populous countries
# thoughtful use of tidyr and dplyr is requried for wrangling
# filter down to top 10 most populous counties
top_10 <- midwest %>% 
  top_n(10, wt = poptotal) %>% # top_n() selects the top n rows based on a column, here we are selecting the top 10 rows based on the `poptotal` column. wt = poptotal means we are using the `poptotal` column to determine the top 10 rows
  unite(county_state, county, state, sep = ", ") %>% # unite() combines two columns into one, here we are combining the `county` and `state` columns into a new column called `county_state`, with a comma and space as the separator
  arrange(poptotal) %>% # sort the data by `poptotal` in ascending order
  mutate(location = factor(county_state, county_state)) # mutate() is used to create a new column, here we are creating a new column called `location` which is a factor of the `county_state` column
# render a horizontal bar chart of the top 10 most populous counties
ggplot(data = top_10) +
  geom_col(mapping = aes(x= location, y = poptotal)) +
  coord_flip() # coord_flip() flips the x and y axes, making the bar chart horizontal

# create a better label for the `inmetro` column
labeled <- midwest %>%
  mutate(location = if_else(inmetro == 0, "Rural", "Urban"))

ggplot(data = labeled) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = location),
    alpha = 0.5 # set transparency to 50%
  ) +
  facet_wrap(~state) # pass the `state` column to facet_wrap() to create a separate plot for each state

# adding better labels to the plot
ggplot(data = labeled) +
  geom_point(
    mapping = aes(x = percollege, y = percadultpoverty, color = location), 
    alpha = 0.5 # set transparency to 50%
  ) +
  # add title and axis labels
  labs(
    title = "Percentage of College Education vs Adult Poverty by State",
    x = "Percentage of College Education",
    y = "Percentage of Adult Poverty",
    color = "Urbanity" # legend lable for color
  )

# load the ggrepel package: functions that prevent text labels from overlapping
library(ggrepel)
# find the highest level of poverty in each state
most_poverty <- midwest %>% 
  group_by(state) %>% 
  filter(percadultpoverty == max(percadultpoverty)) %>%
  unite(county_state, county, state, sep = ", ") 

# store the subtitles in a variable for cleaner graphing code
subtitle <- "(the county with the highest level of poverty in each state is labeled)"  
# plot the data with labels
ggplot(data = labeled, mapping = aes(x = percollege, y = percadultpoverty)) +
  # add the point geometry
  geom_point(mapping = aes(color = location), alpha = 0.5) +
  # add the label geometry
  geom_text_repel( # this function is from the ggrepel package, which helps to prevent text labels from overlapping
    data = most_poverty, 
    mapping = aes(label = county_state), 
    nudge_x = 0.5, # nudge the label to the right
    nudge_y = 0.5, # nudge the label up
  ) +
  # set the scale for the axis
  scale_x_continuous(limits = range(labeled$percollege)) +
  # add title and axis labels
  labs(
    title = "Percentage of College Education vs Adult Poverty by State",
    subtitle = subtitle,
    x = "Percentage of College Education",
    y = "Percentage of Adult Poverty",
    color = "Urbanity" # legend label for color
  )

# 16.4 Building Map ----
# load the maps package
library(maps)
install.packages("maps") # install the maps package if not already installed
# load the map of canada
canada_map <- map_data("world", region = "Canada") # map_data() is used to get the map data for a specific region, here we are getting the map data for Canada
# plot the map of canada
ggplot(data = canada_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") + # geom_polygon() is used to create a polygon map, here we are using the `long` and `lat` columns for the x and y coordinates, and the `group` column to group the polygons
  coord_fixed() + # coord_fixed() is used to fix the aspect ratio of the map
  labs(title = "Map of Canada") # add a title to the map
# plot the map of British Columbia of Canada
# load a shapefile of US states using map_data function
state_shape <- map_data("state") # map_data() is used to get the map data for a specific region, here we are getting the map data for US states
# create a blank map of US states
ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group), 
    fill = "lightblue", 
    color = "black",
    size = 0.1 # set the size of the polygon border
  ) +
  coord_map() # coord_map() is used to create a map projection, here we are using the default map projection
# then draw the same map for Canada provinces
canada_shape <- map_data("world", region = "Canada") # get the map data for Canada
# plot the map of Canada provinces
ggplot(canada_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group), 
    fill = "lightblue", 
    color = "black",
    size = 0.1 # set the size of the polygon border
  ) +
  coord_map() + # coord_map() is used to create a map projection, here we are using the default map projection
  labs(title = "Map of Canada Provinces") # add a title to the map
# create a data frame of city coordinates to display
cities <- data.frame(
  city = c("Vancouver", "Victoria", "Calgary", "Edmonton", "Winnipeg"),
  lat = c(49.2827, 48.4284, 51.0447, 53.5461, 49.8951),
  long = c(-123.1207, -123.3656, -114.0719, -113.4938, -97.1384)
)
# draw the state outlines, then plot the city point on the map
ggplot(canada_shape) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "lightblue", color = "black", size = 0.1) +
  geom_point(
    data = cities, 
    mapping = aes(x = long, y = lat), 
    color = "red", 
    size = 3, 
    shape = 21, # shape 21 is a filled circle
    fill = "yellow" # fill the circle with yellow color
  ) +
coord_map()

# 16.5 mapping evictions in San Francisco ----
notices <- read.csv("Eviction_Notices.csv", stringsAsFactors = FALSE) # read the eviction notices data, stringasFactors = FALSE means that the strings will not be converted to factors

# data wrangling: format dates, filter to 2017 notices, extract lat/long data
notices <- notices %>%
  mutate(date = as.Date(File.Date, format = "%m/%d/%Y")) %>%
  filter(format(date, "%Y") == "2017") %>%
  separate(Location, c("lat", "long"), ", ") %>%  # this splits the 'Location' column
  mutate(
    lat = as.numeric(gsub("\\(", "", lat)),       # remove '('
    long = as.numeric(gsub("\\)", "", long))      # remove ')'
  ) 
# create a mape of SF, with a point at teach eviction notice address
# use `install_github()` to install the newer version of `ggmap` on Github
# devtools: install_github("dkahle/ggmap") 
library(ggmap)
library(ggplot2)  
source("api_keys.R")
# create the background of the map titles
base_plot <- qmplot(
  data = notices,
  x = long,
  y = lat, # longitude and latitude coordinates
  geom = "blank", # geom = "blank" means we are creating a blank map
  maptype = "stamen_terrain", # maptype = "toner-lite" means we are using the toner-lite map type
  darken = 0.5, # darken the map by 50%
  legend = "topleft" # no legend is needed
)

# the use of Stadia Maps is embedded in the latest version of the ggmap package.
# Starting with ggmap v3.0.0, the package dropped support for Google’s map services (due to API policy changes) and began using Stadia Maps as its primary tile provider for qmplot() and get_map().

# add the location of evictions to the map
base_plot +
  geom_point(
    data = notices, 
    mapping = aes(x = long, y = lat), 
    color = "red", 
    size = 0.5, 
    alpha = 0.5 # set transparency to 50%
  ) +
  labs(title = "Eviction Notices in San Francisco (2017)", x = "Longitude", y = "Latitude") + # add title and axis labels
theme_minimal() # use a minimal theme for the plot

# draw a heatmapo of eviction rates, computing the contours
base_plot +
  stat_density2d( # stat_density2d() is used to compute the density of points in a 2D space
    geom = "polygon", # geom = "polygon" means we are creating a polygon map
    mapping = aes(x = long, y = lat, fill = after_stat(level)), # after_stat(level) is used to compute the density of points in a 2D space
    alpha = 0.3
  ) +
  scale_fill_gradient2(
    "# of Evictions", # label for the fill scale
    low = "white",
    mid = "yellow",
    high = "red"
  ) +
  labs(
    title = "Number of Evictions in San Francisco, 2017",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(plot.margin = margin(0.3, 0, 0, 0, "cm"))