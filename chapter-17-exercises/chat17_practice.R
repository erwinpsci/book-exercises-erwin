# Exercise 17.2.1
library(ggplot2)
library(datasets)
ggplot(data = iris) +
  geom_point(mapping = aes(x = Sepal.Width, y = Petal.Width, color = Species))

# view the dataset
View(iris)

install.packages("plotly")
library(plotly)

# create and score a scatter plot of the `iris` dataset using ggplot2
flower_plot <- ggplot(data = iris) +
    geom_point(mapping = aes(x = Sepal.Length, y = Petal.Length, color = Species))
# make the plot interactive by passing it to ggplotly() function
ggplotly(flower_plot)

# option 2
plot_ly(
    data = iris, # pass in the data to be visualized
    x = ~Sepal.Width, # specify x-axis variable
    y = ~Petal.Width, # specify y-axis variable
    color = ~Species, # specify variable to color points by, `~` indicates a formula, without ~ it would be interpreted as a string
    type = 'scatter', # specify the type of plot
    mode = 'markers' # specify that we want to plot points (markers)
)