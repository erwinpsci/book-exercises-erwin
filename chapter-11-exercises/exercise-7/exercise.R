# Exercise 7: using dplyr on external data

# Load the `dplyr` library


# Use the `read.csv()` function to read in the included data set. Remember to
# save it as a variable.
nbadata <- read.csv("nba_teams_2016.csv")

# View the data frame you loaded, and get some basic information about the 
# number of rows/columns. 
dim(nbadata)
# Note the "X" preceding some of the column titles as well as the "*" following
# the names of teams that made it to the playoffs that year.


# Add a column that gives the turnovers to steals ratio (TOV / STL) for each team
nbadata <- mutate(
  nbadata,
  steals_ratio = TOV / STL
)

# Sort the teams from lowest turnover/steal ratio to highest
# Which team has the lowest turnover/steal ratio?
highest_ratio <- nbadata %>% 
  arrange(-steals_ratio) %>% 
  filter(steals_ratio == min(steals_ratio)) 

# Using the pipe operator, create a new column of assists per game (AST / G) 
# AND sort the data.frame by this new column in descending order.
nbadata <- nbadata %>% 
  mutate(assists_per_gam = AST / G) %>% 
  arrange(desc(assists_per_gam))

# Create a data frame called `good_offense` of teams that scored more than 
# 8700 points (PTS) in the season
good_offense <- nbadata %>% 
  filter(PTS > 8700)

# Create a data frame called `good_defense` of teams that had more than 
# 470 blocks (BLK)
good_defense <- nbadata %>% 
  filter(BLK > 470)


# Create a data frame called `offense_stats` that only shows offensive 
# rebounds (ORB), field-goal % (FG.), and assists (AST) along with the team name.
offense_stats <- nbadata %>% 
  select(Team, ORB, FG., AST)

# Create a data frame called `defense_stats` that only shows defensive 
# rebounds (DRB), steals (STL), and blocks (BLK) along with the team name.
defense_stats <- nbadata %>% 
  select(Team, DRB, STL, BLK)

# Create a function called `better_shooters` that takes in two teams and returns
# a data frame of the team with the better field-goal percentage. Include the 
# team name, field-goal percentage, and total points in your resulting data frame
better_shooters <- function(Team1, Team2){
  better_team <- filter(nbadata, Team %in% c(Team1, Team2)) %>% 
    filter(FG. == max(FG.)) %>% 
    select(Team, FG., PTS)
  better_team
}

# Call the function on two teams to compare them (remember the `*` if needed)
better_shooters("Golden State Warriors*", "Atlanta Hawks*")
