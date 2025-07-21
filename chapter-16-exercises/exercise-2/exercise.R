# Exercise 2: advanced ggplot2 practice

# Install and load the `ggplot2` package
#install.packages('ggplot2')
library("ggplot2")

# For this exercise you will again be working with the `diamonds` data set.
# Use `?diamonds` to review details about this data set
?diamonds

## Position Adjustments

# Draw a column (bar) chart of diamonds cuts by price, with each bar filled by 
# clarity. You should see a _stacked_ bar chart.
ggplot(diamonds, aes(x = cut, y = price, fill = clarity)) +
  geom_bar(stat = "identity")

# Draw the same chart again, but with each element positioned to "fill" the y axis
ggplot(diamonds, aes(x = cut, y = price, fill = clarity)) +
  geom_bar(stat = "identity", position = "fill")
# position = "fill" will make the y-axis sum to 1, so each bar is a percentage of the total

# Draw the same chart again, but with each element positioned to "dodge" each other
ggplot(diamonds, aes(x = cut, y = price, fill = clarity)) +
  geom_bar(stat = "identity", position = "dodge")
# position = "dodge" will make the bars next to each other, instead of stacked

# Draw a plot with point geometry with the x-position mapped to `cut` and the 
# y-position mapped to `clarity`
# This creates a "grid" grouping the points
ggplot(diamonds, aes(x = cut, y = clarity)) +
  geom_point()

# Use the "jitter" position adjustment to keep the points from all overlapping!
# (This works a little better with a sample of diamond data, such as from the 
# previous exercise).
ggplot(diamonds_sample, aes(x = cut, y = clarity)) +
  geom_point(position = "jitter")

## Scales

# Draw a "boxplot" (with `geom_boxplot`) for the diamond's price (y) by color (x)
ggplot(diamonds, aes(x = color, y = price)) +
  geom_boxplot()

# This has a lot of outliers, making it harder to read. To fix this, draw the 
# same plot but with a _logarithmic_ scale for the y axis.
ggplot(diamonds, aes(x = color, y = price)) +
  geom_boxplot() +
  scale_y_log10()
# scale_y_log10() will change the y-axis to a logarithmic scale, which helps with outliers
# logarithmic scales are useful when the data has a wide range of values, as it compresses the scale and makes it easier to see patterns in the data
# logarithmic means that each step on the y-axis is a factor of 10, so the values are not evenly spaced

# For another version, draw the same plot but with `violin` geometry instead of 
# `boxplot` geometry!
# How does the logarithmic scale change the data presentation?
ggplot(diamonds, aes(x = color, y = price)) +
  geom_violin() +
  scale_y_log10()
# geom_violin() will create a violin plot, which is similar to a boxplot but shows the distribution of the data

# Another interesting plot: draw a plot of the diamonds price (y) by carat (x), 
# using a heatmap of 2d bins (geom_bin2d)
# What happens when you make the x and y channels scale logarithmically?
ggplot(diamonds, aes(x = carat, y = price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()
# geom_bin2d() will create a heatmap of 2d bins, which is useful for visualizing the density of points in a scatter plot

# Draw a scatter plot for the diamonds price (y) by carat (x). Color each point
# by the clarity (Remember, this will take a while. Use a sample of the diamonds 
# for faster results)
ggplot(diamonds_sample, aes(x = carat, y = price, color = clarity)) +
  geom_point()

# Change the color of the previous plot using a ColorBrewer scale of your choice. 
# What looks nice?
ggplot(diamonds_sample, aes(x = carat, y = price, color = clarity)) +
  geom_point() +
  scale_color_brewer(palette = "Set1")


## Coordinate Systems

# Draw a bar chart with x-position and fill color BOTH mapped to cut
# For best results, SET the `width` of the geometry to be 1 (fill plot, no space
# between)
# TIP: You can save the plot to a variable for easier modifications
p <- ggplot(diamonds, aes(x = cut, fill = cut)) +
  geom_bar(width = 1)
print(p)
# fill = cut will fill the bars with the color of the cut, and width = 1 will make the bars fill the entire x-axis

# Draw the same chart, but with the coordinate system flipped
p + coord_flip()

# Draw the same chart, but in a polar coordinate system. It's a Coxcomb chart!
p + coord_polar() +
  labs(x = "Cut", y = "Count", title = "Coxcomb Chart of Diamond Cuts") +
  theme_minimal()
# coord_polar() will change the coordinate system to polar, which is useful for creating circular charts like pie charts or coxcomb charts
# coxcomb chart is a variation of a pie chart that shows the size of each slice in relation to the total, but also adds a radial component to show the distribution of the data

## Facets

# Take the scatter plot of price by carat data (colored by clarity) and add 
# _facets_ based on the diamond's `color`
ggplot(diamonds_sample, aes(x = carat, y = price, color = clarity)) +
  geom_point() +
  facet_wrap(~ color)
ggsave("my-plot-2.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
# facet_wrap() will create a separate plot for each level of the color variable, allowing you to see the distribution of the data for each color
# add what each facet represents in the plot title

# You can also use `facet_grid()` to create a grid of plots based on two variables
ggplot(diamonds_sample, aes(x = carat, y = price, color = clarity)) +
  geom_point() +
  facet_grid(color ~ clarity) +
  labs(title = "Scatter Plot of Price by Carat, Faceted by Color and Clarity")
## Saving Plots

# Use the `ggsave()` function to save the current (recent) plot to disk.
# Name the output file "my-plot.png".
# Make sure you've set the working directory!!
ggsave("my-plot.png", width = 8, height = 6, dpi = 300)
# The `ggsave()` function will save the current plot to a file, with the specified width, height, and resolution (dpi)
# specify where will the png be saved
# The file will be saved in the current working directory, which you can check with `getwd()`
ggsave("my-plot.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
