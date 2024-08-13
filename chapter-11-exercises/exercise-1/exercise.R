# Exercise 1: working with data frames (review)

# Install devtools package: allows installations from GitHub
install.packages("devtools")

# Install "fueleconomy" dataset from GitHub
devtools::install_github("hadley/fueleconomy")

# Use the `libary()` function to load the "fueleconomy" package
library(fueleconomy)

# You should now have access to the `vehicles` data frame
# You can use `View()` to inspect it


# Select the different manufacturers (makes) of the cars in this data set. 
# Save this vector in a variable
makes_manu <- select(vehicles, make)

# Use the `unique()` function to determine how many different car manufacturers
# are represented by the data set
length(unique(vehicles$make))

# Filter the data set for vehicles manufactured in 1997
makes_manu_1997 <- filter(vehicles, year == 1997)

# Arrange the 1997 cars by highway (`hwy`) gas milage
# Hint: use the `order()` function to get a vector of indices in order by value
# See also:
# https://www.r-bloggers.com/r-sorting-a-data-frame-by-the-contents-of-a-column/
cars_hwy_1997 <- arrange(makes_manu_1997, hwy)

# Mutate the 1997 cars data frame to add a column `average` that has the average
# gas milage (between city and highway mpg) for each car
cars_hwy_1997 <-mutate(
  makes_manu_1997,
  average = (hwy + cty)/2
)

# Filter the whole vehicles data set for 2-Wheel Drive vehicles that get more
# than 20 miles/gallon in the city. 
# Save this new data frame in a variable.
makes_2_wheels_20 <- vehicles %>% 
  filter(drive == "2-Wheel Drive") %>% 
  filter(cty > 20)

# Of the above vehicles, what is the vehicle ID of the vehicle with the worst 
# hwy mpg?
# Hint: filter for the worst vehicle, then select its ID.
worst_hwy_mpg <- makes_2_wheels_20 %>% 
  filter(hwy == min(hwy)) %>% 
  select(id, hwy)

# Write a function that takes a `year_choice` and a `make_choice` as parameters, 
# and returns the vehicle model that gets the most hwy miles/gallon of vehicles 
# of that make in that year.
# You'll need to filter more (and do some selecting)!
find_best_model <- function(make_choice, year_choice){
  filter_vehicles <- vehicles %>% 
    filter(make == make_choice, year == year_choice) %>% 
    arrange(desc(hwy)) %>% 
    slice(1) %>% 
    pull(model)
}

# What was the most efficient Honda model of 1995?
most_efficient_honda_1995 <- find_best_model("Honda", 1995)
print(most_efficient_honda_1995)
