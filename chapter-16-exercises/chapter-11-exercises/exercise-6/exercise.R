# Exercise 6: dplyr join operations

# Install the `"nycflights13"` package. Load (`library()`) the package.
# You'll also need to load `dplyr`
#install.packages("nycflights13")  # should be done already
library("nycflights13")
library("dplyr")

# Create a dataframe of the average arrival delays for each _destination_, then 
# use `left_join()` to join on the "airports" dataframe, which has the airport
# information
# Which airport had the largest average arrival delay?
mean_arr_delay_des <- flights %>% 
  group_by(dest) %>% 
  summarise(mean_arr_delay_des = mean(arr_delay, na.rm = TRUE)) %>% 
  mutate(faa = dest)

join_table <- left_join(mean_arr_delay_des, airports, by = "faa")

largest_delay_airport <- join_table %>% 
  filter(mean_arr_delay_des == max(mean_arr_delay_des, na.rm = TRUE)) 

largest_delay_airport

# Create a dataframe of the average arrival delay for each _airline_, then use
# `left_join()` to join on the "airlines" dataframe
# Which airline had the smallest average arrival delay?
mean_arr_delay_airline <- flights %>% 
  group_by(carrier) %>% 
  summarise(mean_arr_delay_airline = mean(arr_delay, na.rm = TRUE))

join_table_2 <- left_join(mean_arr_delay_airline, airlines, by ="carrier")

smallest_delay_airline <- join_table_2 %>% 
  filter(mean_arr_delay_airline == max(mean_arr_delay_airline, na.rm = TRUE))

smallest_delay_airline

