# 14.3 ----
# The URI for the `search/repositories` endpoint of the GitHub API: query # for `dplyr`, sorting by `forks
# https://api.github.com/search/repositories?q=dplyr&sort=forks

install.packages("httr") # once per machine 
library("httr") # in each relevant script

url <- "https://api.github.com/search/repositories?q=dplyr&sort=forks"
response <- GET(url)

# restructure the previous request to make it easier to read and update

# make a get request to the GitHub API's "search/repositories" endpoint
# request repositories that match the search "dplyr", sorted by forks, which means that the most popular repositories will be returned first

# construct your `resource_uri` from a reusable `base_uri` and an `endpoint`
base_uri <- "https://api.github.com" # base URI for the GitHub API
endpoint <- "/search/repositories" # endpoint for searching repositories
resource_uri <- paste0(base_uri, endpoint) # combine base URI and endpoint # paste0() is used to concatenate strings in R # don's use paste() here because it adds spaces between the element # string means that the `resource_uri` will be "https://api.github.com/search/repositories"

# store any query parameters in a list
query_params <- list(q = "dplyr", sort = "forks") # query parameters for the request

# make your request, specifying the query parameters via the `query` argument

response <- GET(resource_uri, query = query_params)
GET(url) # make a GET request to the constructed resource URI with the specified query parameters

# extract content from `response` as a text string
response_text <- content(response, as = "text") # extract the content of the response as text

# 14.4 ----
install.packages("jsonlite")
library("jsonlite") # in each relevant script

# make a request to a given `uri` with a set of `query_params`
# then extract and parse the results 

# make the request
base_uri <- "https://api.github.com"
endpoint <- "/search/repositories"
uri <- paste0(base_uri, endpoint)

query_params <- list(q = "dplyr", sort = "forks")
response <- GET(uri, query = query_params) # make a GET request to the specified URI with the query parameters

response_text <- content(response, "text") # extract the content of the response as text

# convert the JSON string to a list
response_data <- fromJSON(response_text) # parse the JSON string into a list
head(response_data)

is.data.frame(response_data) # check if the response data is a data frame
print(resonse_data) # print the response data)
str(response_data) # display the structure of the response data
names(response_data) # get the names of the elements in the response data

# extract the useful data
items <- response_data$items # extract the 'items' element from the response data
is.data.frame(items) # check if 'items' is a data frame)
head(items) # display the first few rows of 'items'

# a demonstraction of the structure of `nexted` data frames
# create a `people` data frame with a `names` column and a `details` column

people <- data.frame(
  names = c("ED", "Jessica", "Keagan")
)

# create a data frame of favorites with two columns
favorites <- data.frame(
  food = c("Pizza", "Pasta", "Salad"),
  music = c("Bluegrass", "Indie", "Electronic")
)
# store the second data frame as column of the first -- a bad idea
people$favorites <- favorites
# print the people data frame
print(people) # print the people data frame with nested favorites

people$favorites.food # why does this not work? # this does not work because 'favorites' is a data frame, not a list

# access the `food` column of the data frame stored in `people$favorites`
people$favorites$food 

# use `flatten()` to format nested data frames
people <- flatten(people) # flatten the nested data frame, the aim of this line is to convert the nested structure into a flat data frame
# print the flattened people data frame
print(people) # print the flattened people data frame
people$favorites.food # now this works because the nested structure has been flattened


