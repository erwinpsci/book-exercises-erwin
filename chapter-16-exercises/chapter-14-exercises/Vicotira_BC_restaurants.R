# Chinese Restaurants in Victoria ----
  
# Load API keys
source("api_key.R")

# Load necessary packages
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggmap)
library(ggplot2)
library(ggrepel)

# Register Google API key
register_google(key = google_key, write = TRUE)

# Construct Yelp Fusion API request
base_uri <- "https://api.yelp.com/v3"
endpoint <- "/businesses/search"
search_uri <- paste0(base_uri, endpoint)

# Query parameters for Chinese restaurants in Victoria, BC
query_params <- list(
  term = "restaurants",
  categories = "chinese",
  location = "Victoria, BC, Canada",
  sort_by = "rating",
  radius = 20000
)

# Make GET request
response <- GET(
  search_uri,
  query = query_params,
  add_headers(Authorization = paste("bearer", yelp_key))
)

# Parse and flatten response
response_text <- content(response, type = "text")
response_data <- fromJSON(response_text)
restaurants <- flatten(response_data$businesses)

# Add rank and label
restaurants <- restaurants %>%
  mutate(rank = row_number()) %>%
  mutate(name_and_rank = paste0(rank, ". ", name))

# Base map of Victoria
base_map <- ggmap(get_map(location = "Victoria, BC", zoom = 12))
print(base_map)

# Add restaurant labels to the map
base_map +
  geom_label_repel(
    data = restaurants,
    aes(x = coordinates.longitude, y = coordinates.latitude, label = name_and_rank),
    size = 3.5,
    nudge_x = 0.01,
    nudge_y = 0.01,
    segment.color = "grey50",
    segment.size = 0.2
  )