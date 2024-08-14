# Exercise 4: practicing with dplyr

# Install the `"nycflights13"` package. Load (`library()`) the package.
# You'll also need to load `dplyr`
install.packages("nycflights13")
library(nycflights13)
library(dplyr)

# 1 basic information ----

# The data frame `flights` should now be accessible to you.
# Use functions to inspect it: how many rows and columns does it have?
dim(flights)
# What are the names of the columns?
colnames(flights)
# Use `??flights` to search for documentation on the data set (for what the 
# columns represent)
??flights


# Use `dplyr` to give the data frame a new column that is the amount of time
# gained or lost while flying (that is: how much of the delay arriving occured
# during flight, as opposed to before departing).
flights <- mutate(
  flights,
  time_gained_or_lost = arr_delay - dep_delay
)

# Use `dplyr` to sort your data frame in descending order by the column you just
# created. Remember to save this as a variable (or in the same one!)
flights_descending <- arrange(flights, -time_gained_or_lost)

# For practice, repeat the last 2 steps in a single statement using the pipe
# operator. You can clear your environmental variables to "reset" the data frame
flights_descending <- flights %>% 
  mutate(time_gained_or_lost = (arr_delay - dep_delay)) %>% 
  arrange(-time_gained_or_lost)

# Make a histogram of the amount of time gained using the `hist()` function
hist(flights$time_gained_or_lost)

# On average, did flights gain or lose time?
# Note: use the `na.rm = TRUE` argument to remove NA values from your aggregation
average_time_difference <- flights %>% 
  summarise(mean(time_gained_or_lost, na.rm = TRUE))

# Create a data.frame of flights headed to SeaTac ('SEA'), only including the
# origin, destination, and the "gain_in_air" column you just created
Sea_head_flights <- flights %>% 
  select(origin, dest, time_gained_or_lost) %>%
  filter(dest == "SEA")

# On average, did flights to SeaTac gain or loose time?
summarise(Sea_head_flights,
          hear_to_sea_average = mean(time_gained_or_lost, na.rm = TRUE))

# Consider flights from JFK to SEA. What was the average, min, and max air time
# of those flights? Bonus: use pipes to answer this question in one statement
# (without showing any other data)!
jfk_to_sea <- flights %>% 
  filter(origin == "JFK", dest == "SEA") %>% 
  select(carrier, origin, dest, air_time) %>% 
  summarise(
    mean(air_time, na.rm = TRUE),
    min(air_time, na.rm = TRUE),
    max(air_time, na.rm = TRUE)
  )
