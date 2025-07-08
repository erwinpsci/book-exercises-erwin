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
expenditure_chart <- ggplot(data = expenditure_plot_data) +
  geom_text_repel(
    mapping = aes(x = X1990/100, y = X2014/100, label = Country.Code),
  ) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  labs(title = indicator, x = "Expenditure in 1990", y = "Expenditure in 2014") 
