wd_data <- read.csv(
  "world_bank_data.csv",
  stringsAsFactors = FALSE, # Ensure strings are not converted to factors
  skip = 4, # Skip the first 4 rows
)

library(dplyr)
# visually compare expenditures for 1990 and 2014
# begin by filering the rows for the indicator of interest
indicator <- "Government expenditure on education, total (% of GDP)"
expenditure_plot_data <- wd_data %>% 
  filter(Indicator.Name == indicator)

# plot the expenditure in 1990 against 2014 using the "ggplot2" package
library(ggplot2)
library(ggrepel)
library(scales)
expenditure_chart <- ggplot(data = expenditure_plot_data) +
  geom_text_repel(
    mapping = aes(x = X1990/100, y = X2014/100, label = Country.Code),
  ) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(title = indicator, x = "Expenditure in 1990", y = "Expenditure in 2014") 
print(expenditure_chart)

library(tidyr)
# reshape the data to create a new colum for the "year"
long_year_data <- wd_data %>%
  gather(
    key = year, # year will be the new column name
    value = value, # value will be the new column name
    X1960:X # all columns from X1960 to X
  )

# filter the rows for the indicator and country of interest
indicator <- "Government expenditure on education, total (% of GDP)"
spain_plot_data <- long_year_data %>% 
  filter(
    Indicator.Name == indicator,
    Country.Code == "ESP" # Spain's country code is "ESP"
  ) %>% 
  mutate(year = as.numeric(substr(year, 2, 5))) # Extract the year from the column name, characters from position 2 to 5 in each year value.
# show the educational expenditure over time
chart_title <- paste(indicator, "in Spain") # paste combines strings
spain_chart <- ggplot(data = spain_plot_data) +
  geom_line(mapping = aes(x = year, y = value/100)) + # divide by 100 to convert percentage to decimal
  scale_y_continuous(labels = percent) +
  labs(title = chart_title, x = "Year", y = "Expenditure (% of GDP)") 
print(spain_chart)

# reshape the data to create columns for each indicator
wide_data <- long_year_data %>% 
  select(-Indicator.Code) %>% # remove the Indicator.Code column
  spread(
    key = Indicator.Name, # the new column names will be the indicator names
    value = value # populate new columns with values from the value column
  )

# prepare data and filter for year of interest
x_var <- "Literacy rate, adult female (% of females ages 15 and above)"
y_var <- "Unemployment, female (% of female labor force) (modeled ILO estimate)"
lit_plot_data <- wide_data %>% 
  mutate(
    lit_percent_2014 = wide_data[, x_var] / 100, # convert percentage to decimal
    employ_percent_2014 = wide_data[, y_var] / 100 # convert percentage to decimal
  ) %>% 
  filter(year == "X2014")
# show the literacy vs. employment rates
lit_chart <- ggplot(data = lit_plot_data) +
  geom_point(mapping = aes(x = lit_percent_2014, y = employ_percent_2014)) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(
    x = x_var,
    y = "Unemployment, female (% of female labor force)",
    title = "Female Literacy Rate versus Female Unemployment Rate (2014)"
  )
print(lit_chart)
