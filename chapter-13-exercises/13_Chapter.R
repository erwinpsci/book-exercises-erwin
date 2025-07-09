install.packages("dbplyr")
library("DBI")
library("dbplyr")
install.packages("RSQLite")
library("RSQLite")

# To access an SQLite database
install.packages("RPostgreSQL")
library("RPostgreSQL")

# Create a "connection" to the RDMS 
db_connection <- dbConnect(SQLite(), dbname = "path/to/database.sqlite")