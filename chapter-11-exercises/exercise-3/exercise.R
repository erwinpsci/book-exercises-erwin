# Exercise 3: using the pipe operator

# Install (if needed) and load the "dplyr" library
#install.packages("dplyr")
library("dplyr")

# Install (if needed) and load the "fueleconomy" package
#install.packages('devtools')
#devtools::install_github("hadley/fueleconomy")
library("fueleconomy")

# Which 2015 Acura model has the best hwy MGH? (Use dplyr, but without method
# chaining or pipes--use temporary variables!)
Acura_2015 <- filter(vehicles, make == "Acura", year == 2015)
best_acura_2015 <- filter(Acura_2015, hwy == max(hwy))
best_acura_2015

# Which 2015 Acura model has the best hwy MPG? (Use dplyr, nesting functions)
best_acura_2015 <- vehicles %>% 
  filter(make == "Acura", year == 2015) %>% 
  filter(hwy == max(hwy))
best_acura_2015

# Which 2015 Acura model has the best hwy MPG? (Use dplyr and the pipe operator)



### Bonus

# Write 3 functions, one for each approach.  Then,
# Test how long it takes to perform each one 1000 times


# best Honda model to save fuel average
best_average <-  function(hwy, cty){
  result <- (hwy + cty)/2
  return(result)
}

best_honda <- vehicles %>% 
  filter(make == "Honda") %>% 
  mutate(best_average = best_average(hwy, cty)) %>% 
  filter(best_average == max(best_average)) %>% 
  select(model, year, best_average)
