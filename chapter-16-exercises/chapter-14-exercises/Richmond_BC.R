# richmond_chinese_restaurants.R

# Load API keys
source("api_key.R")

# Load required packages
library(httr)
library(jsonlite)
library(dplyr)
library(ggmap)
library(ggplot2)
library(ggrepel)

# Define Yelp API endpoint and parameters
base_uri <- "https://api.yelp.com/v3"
endpoint <- "/businesses/search"
search_uri <- paste0(base_uri, endpoint)

query_params <- list(
  term = "restaurants",
  categories = "chinese",
  location = "Richmond, BC",
  sort_by = "best_match",
  radius = 15000,
  limit = 50
)

# Make Yelp API request
response <- GET(
  search_uri,
  query = query_params,
  add_headers(Authorization = paste("bearer", yelp_key))
)

# Parse and flatten response
response_text <- content(response, type = "text")
response_data <- fromJSON(response_text)
restaurants <- flatten(response_data$businesses)

# Add ranking and label
restaurants <- restaurants %>%
  mutate(rank = row_number()) %>%
  mutate(name_and_rank = paste0(rank, ". ", name))

# Register Google API and fetch base map
register_google(key = google_key, write = TRUE)
base_map <- ggmap(get_map(location = "Richmond, BC", zoom = 12))
print(base_map)

# Overlay restaurant labels
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


