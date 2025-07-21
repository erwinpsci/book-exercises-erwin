# Exercise 2: working with data APIs

# load relevant libraries
library("httr")
library("jsonlite")

# Be sure and check the README.md for complete instructions!


# Use `source()` to load your API key variable from the `apikey.R` file you made.
# Make sure you've set your working directory!
source("apikey.R")  # Load the API key from the apikey.R file
print(nyt_apikey)  # Print the API key to confirm it's loaded correctly

# Construct an HTTP request to search for reviews for the given movie.
# The base URI is `https://api.nytimes.com/svc/movies/v2/`
# The resource is `reviews/search.json`
# See the interactive console for parameter details:
#   https://developer.nytimes.com/movie_reviews_v2.json
#
# You should use YOUR api key (as the `api-key` parameter)
# and your `movie_name` variable as the search query!
base_uri <- "https://api.nytimes.com/svc/search/v2"
resource <- "/articlesearch.json"
query_params <- list(
  "api-key" = nyt_apikey,
  q = "political science",
  sort = "newest"
)

# Send the HTTP Request to download the data
# Extract the content and convert it from JSON
response <- GET(paste0(base_uri, resource), query = query_params)
body <- fromJSON(content(response, "text"))


# What kind of data structure did this produce? A data frame? A list?
# Check the structure of the response data
articles <- body$response$docs
str(articles)

# Optional: extract useful fields from the first result
if (!is.null(articles) && nrow(articles) > 0) {
  headline <- articles$headline$main[1]
  summary <- articles$abstract[1]
  url <- articles$web_url[1]
  article_info <- list(headline = headline, summary = summary, url = url)
  print(article_info)
}

class(body)  # Check the class of the response data
is.data.frame(body)  # Check if it's a data frame
is.list(body)  # Check if it's a list
# Manually inspect the returned data and identify the content of interest 
# (which are the movie reviews).
# Use functions such as `names()`, `str()`, etc.
names(body)  # Get the names of the elements in the response data
names(body)  # Get the names of the elements in the results

# Flatten the movie reviews content into a data structure called `reviews`
reviews <- flatten(body$response$docs)  # Flatten the reviews data

# From the most recent review, store the headline, short summary, and link to
# the full article, each in their own variables
if (nrow(reviews) > 0) {
  headline <- reviews$headline.main[1]  # Get the headline of the first review
  summary <- reviews$abstract[1]  # Get the summary of the first review
  url <- reviews$web_url[1]  # Get the URL of the first review
} else {
  headline <- NA
  summary <- NA
  url <- NA
}

# Create a list of the three pieces of information from above. 
# Print out the list.
article_info <- list(headline = headline, summary = summary, url = url)
print(article_info)  # Print the article information list
# Check if the article_info list is not empty
if (length(article_info) > 0) {
  print(article_info)  # Print the article information list
} else {
  print("No articles found.")
}

# updating chapter 14