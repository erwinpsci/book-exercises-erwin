# Exercise 5: dplyr grouped operations

# Install the `"nycflights13"` package. Load (`library()`) the package.
# You'll also need to load `dplyr`
#install.packages("nycflights13")  # should be done already
library("nycflights13")
library("dplyr")
View(flights)

# What was the average departure delay in each month?
grouped <- group_by(flights, month)
summarise(
  grouped,
  dep_mean_each_month = mean(dep_delay, na.rm = TRUE)
)
# Save this as a data frame `dep_delay_by_month`
dep_delay_by_month <- flights %>% 
  group_by(month) %>% 
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE))
dep_delay_by_month
# Hint: you'll have to perform a grouping operation then summarizing your data


# Which month had the greatest average departure delay?
summarise(
  dep_delay_by_month,
  greatests_average_departure = max(dep_delay)
)

# If your above data frame contains just two columns (e.g., "month", and "delay"
# in that order), you can create a scatterplot by passing that data frame to the
# `plot()` function
plot(dep_delay_by_month)

# To which destinations were the average arrival delays the highest?
grouped_dest <- group_by(flights, dest)
grouped_dest_mean <- summarise(
  grouped_dest,
  dest_arr_mean = mean(arr_delay, na.rm = TRUE)
)
grouped_dest_mean
max_delay_dest <- grouped_dest_mean %>% 
  filter(dest_arr_mean == max(dest_arr_mean, na.rm = TRUE))
print(max_delay_dest)
# Hint: you'll have to perform a grouping operation then summarize your data
# You can use the `head()` function to view just the first few rows

# You can look up these airports in the `airports` data frame!
filter(airports, faa == max_delay_dest$dest)

# Which city was flown to with the highest average speed?
city_fastest_speed <- flights %>% 
  mutate(speed = distance/air_time) %>% 
  group_by(dest) %>% 
  summarise(avg_speed = mean(speed, na.rm = TRUE)) %>% 
  filter(avg_speed == max(avg_speed, na.rm = TRUE))
city_fastest_speed

filter(airports, faa ==city_fastest_speed$dest)
