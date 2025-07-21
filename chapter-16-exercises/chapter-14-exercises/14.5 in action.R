# 14.5 ----
source("api_key.R")

# make a request to GEt, inclueind your API key as a header
response <- GET( 
  uri, 
  query = query_params, add_headers(Authorization = paste("bearer", yelp_key)) 
)

# construct a search query for the yelp fusion API's business search ednpoint
base_uri <- "https://api.yelp.com/v3"
endpoint <- "/businesses/search"
search_uri <- paste0(base_uri, endpoint)

# store a list of query parameters for Cuban restaurants around Seattle
query_params <- list(
  term = "restaurants", # search term
  categories = "cuban", # category of businesses to search for
  location = "Seattle, WA", # location to search in
  sort_by = "rating", # sort results by rating
  radius = 8000 # search radius in meters
)

# make a get request, inclueding the API key (as a header) and the list of quyery parameters
response <- GET(
  search_uri,
  query = query_params, # query parameters for the request
  add_headers(authorization = paste("bearer", yelp_key))
)

# parse results and isolate data of interest
response_text <- content(response, type = "text") # extract the content of the response as text
response_data <- fromJSON(response_text) # parse the JSON string into a list
# inspect the response data
names(response_data) 
# flatten the data frame stored in the buisnesses key of the response data
restaurants <- flatten(response_data$businesses) # flatten the 'businesses' element of the response data

# modify the data frame for analysis and presentation
# generate a rank of each resturant based on row number
library(dplyr)
restaurants <- restaurants %>% 
  mutate(rank = row_number()) %>% 
  mutate(name_and_rank = paste0(rank, ". ", name))

# create a base layer for the map (google map image of Seattle)
install.packages("ggmap")
library(ggmap)
library(ggplot2)
register_google(key = google_key, write = TRUE) # register your Google API key for use with ggmap
base_map <- ggmap(get_map(location = "Seattle, WA", zoom = 12))
print(base_map) # print the base map

# add labels to the map based on the coordinates in the data
install.packages("ggrepel")
library(ggrepel)
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