# Exercise 1: reading and querying a web API

# Load the httr and jsonlite libraries for accessing data
# You can also load `dplyr` if you wish to use it
library(httr)
library(jsonlite)
library(dplyr)

# Create a variable base_uri that stores the base URI (as a string) for the 
# Github API (https://api.github.com)
base_uri <- "https://api.github.com"

# Under the "Repositories" category of the API documentation, find the endpoint 
# that will list _repos in an organization_. Then create a variable named
# `org_resource` that stores the endpoint for the `programming-for-data-science`
# organization repos (this is the _path_ to the resource of interest).
org_resource <- "/orgs/programming-for-data-science/repos" # this steps you through the process of finding the endpoint in the documentation

# Send a GET request to this endpoint (the `base_uri` followed by the 
# `org_resource` path). Print the response to show that your request worked. 
# (The listed URI will also allow you to inspect the JSON in the browser easily).
response <- GET(paste0(base_uri, org_resource))
print(response)

# Extract the content of the response using the `content()` function, saving it
# in a variable.
response_test <- content(response, as = "text")

# Convert the content variable from a JSON string into a data frame.
response_data <- fromJSON(response_test)

# How many (public) repositories does the organization have?
print(nrow(response_data)) # This will give you the number of rows in the data frame, which corresponds to the number of repositories.

# Now a second query:
# Create a variable `search_endpoint` that stores the endpoint used to search 
# for repositories. (Hint: look for a "Search" endpoint in the documentation).
search_endpoint <- "/search/repositories" # This is the endpoint for searching repositories in the GitHub API

# Search queries require a query parameter (for what to search for). Create a 
# `query_params` list variable that specifies an appropriate key and value for 
# the search term (you can search for anything you want!)
query_params <- list(q = "graphics") # This specifies that we want to search for repositories related to "graphics"

# Send a GET request to the `search_endpoint`--including your params list as the
# `query`. Print the response to show that your request worked.
response <- GET(paste0(base_uri, search_endpoint), query = query_params)
print(response)
# Extract the content of the response and convert it from a JSON string into a
# data frame. 
response_text <- content(response, as = "text") # this step extracts the content of the response as a text string # test string means that the `response_text` will be a JSON string
response_data <- fromJSON(response_text) # this step converts the JSON string into a data frame
print(response_data)

# How many search repos did your search find? (Hint: check the list names to 
# find an appropriate value).
print(response_data$total_count) # This will give you the total number of repositories found in the search results.

# What are the full names of the top 5 repos in the search results?
top_repos <- response_data$items[1:5, "full_name"] # This extracts the full names of the top 5 repositories
print(top_repos) # This will print the full names of the top 5 repositories
