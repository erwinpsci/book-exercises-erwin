# Exercise 8: exploring data sets

# Load the `dplyr` library


# Read in the data (from `data/pupulitzer-circulation-data.csv`). Remember to 
# not treat strings as factors!
pulitzer.data <- read.csv("pulitzer-circulation-data.csv", stringsAsFactors = FALSE)
colnames(pulitzer.data)

# View in the data set. Start to understand what the data set contains
dim(pulitzer.data)

# Print out the names of the columns for reference
head(pulitzer.data)
colnames(pulitzer.data)

# Use the 'str()' function to also see what types of values are contained in 
# each column (you're looking at the second column after the `:`)
# Did any value type surprise you? Why do you think they are that type?
str(pulitzer.data)


# Add a column to the data frame called 'Pulitzer.Prize.Change` that contains 
# the difference in the number of times each paper was a winner or finalist 
# (hereafter "winner") during 2004-2014 and during 1990-2003
pulitzer.data <- pulitzer.data %>% 
  mutate(Pulitzer.Prize.Change = 
           Pulitzer.Prize.Winners.and.Finalists..2004.2014 - Pulitzer.Prize.Winners.and.Finalists..1990.2003)

# What was the name of the publication that has the most winners between 
# 2004-2014?
most_winners <- pulitzer.data %>% 
  filter(Pulitzer.Prize.Winners.and.Finalists..2004.2014 == 
              max(Pulitzer.Prize.Winners.and.Finalists..2004.2014)) %>% 
  select(Newspaper)

# Which publication with at least 5 winners between 2004-2014 had the biggest
# decrease(negative) in daily circulation numbers?
  

biggest_decrease <- pulitzer.data %>% 
  filter(Pulitzer.Prize.Winners.and.Finalists..2004.2014 >= 5) %>% 
  filter(Change.in.Daily.Circulation..2004.2013 
         == min(Change.in.Daily.Circulation..2004.2013)) %>% 
  select(Newspaper)

biggest_decrease <- filter(pulitzer.data, Pulitzer.Prize.Winners.and.Finalists..2004.2014 >= 5) %>% 
  filter(Change.in.Daily.Circulation..2004.2013
         == min(Change.in.Daily.Circulation..2004.2013)) %>% 
  select(Newspaper)

filter(pulitzer.data, Pulitzer.Prize.Winners.and.Finalists..2004.2014 >= 5) %>%
  filter(min(Change.in.Daily.Circulation..2004.2013) 
         == Change.in.Daily.Circulation..2004.2013) %>%
  select(Newspaper)


# An important part about being a data scientist is asking questions. 
# Write a question you may be interested in about this data set, and then use  
# dplyr to figure out the answer!

# "Is there a correlation between the number of Pulitzer Prizes won 
# and the change in circulation numbers?

pulitzer.data$Change.in.Daily.Circulation..2004.2013 <- as.numeric(gsub("%", "", pulitzer.data$Change.in.Daily.Circulation..2004.2013))

correlation <- cor(
  pulitzer.data$Pulitzer.Prize.Winners.and.Finalists..1990.2014,
  pulitzer.data$Change.in.Daily.Circulation..2004.2013,
  use = "complete.obs"
)
