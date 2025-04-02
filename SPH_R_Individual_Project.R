# Load required packages
library(tidyverse)
install.packages("reshape2")
library(reshape2)

# Data Transformation 
mtcars2 <- mtcars %>%
  mutate(
    power_to_weight = hp / wt,
    mpg_class = if_else(mpg > 20, "High", "Low")
  )

# Grouping and Summarizing 
mtcars_summary <- mtcars2 %>%
  group_by(cyl, am) %>%
  summarise(
    avg_mpg = mean(mpg),
    avg_hp = mean(hp),
    avg_weight = mean(wt),
    .groups = 'drop'
  )

# Histogram of MPG
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(fill = "lightblue", bins = 10) +
  labs(title = "Histogram of MPG", x = "Miles per Gallon")

# Boxplot of HP by Cylinders
ggplot(mtcars, aes(x = factor(cyl), y = hp)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Horsepower by Cylinders", x = "Cylinders", y = "Horsepower")

# Scatter Plot: Weight vs MPG 
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "red") +
  labs(title = "Weight vs MPG", x = "Weight (1000 lbs)", y = "MPG")

# Bar Chart of Cylinder Counts 
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(fill = "orange") +
  labs(title = "Number of Cars by Cylinders", x = "Cylinders", y = "Count")

# Density Plot of HP 
ggplot(mtcars, aes(x = hp)) +
  geom_density(fill = "darkgreen", alpha = 0.5) +
  labs(title = "Density Plot of Horsepower", x = "Horsepower")

# Line Plot: MPG by Index 
ggplot(mtcars, aes(x = 1:nrow(mtcars), y = mpg)) +
  geom_line(color = "blue") +
  labs(title = "Line Plot of MPG by Index", x = "Index", y = "MPG")

# Correlation Heatmap 
cor_data <- round(cor(mtcars), 2)
melted_cor <- melt(cor_data)

ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0) +
  geom_text(aes(label = value), color = "black", size = 3) +
  theme_minimal() +
  labs(title = "Correlation Heatmap", x = "", y = "")


# Bar Chart: Average MPG by Gear 
ggplot(mtcars2, aes(x = factor(gear), y = mpg)) +
  stat_summary(fun = "mean", geom = "bar", fill = "steelblue") +
  labs(title = "Average MPG by Gear", x = "Gear", y = "Average MPG")

# Scatter Plot with Regression Line: HP vs MPG
ggplot(mtcars2, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "HP vs MPG with Regression Line")

# Custom Plot: Power to Weight vs MPG by Transmission 
ggplot(mtcars2, aes(x = power_to_weight, y = mpg, color = factor(am))) +
  geom_point(size = 3) +
  labs(title = "Power-to-Weight vs MPG by Transmission", color = "Transmission (0 = Auto, 1 = Manual)")

