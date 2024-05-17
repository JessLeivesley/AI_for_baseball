## Example script for running data visualization
## 17 May 2024

# Libraries needed (install if you don't have them)
library(dplyr)
library(ggplot2)

# Read in data
# I'm going to use Bo Bichette's 2023 data
bichette_2023 <- read.csv("Batter Data/2023_Bichette.csv")

# You can look at the structure of the data like this:
str(bichette_2023)

# Or the first six entries like this:
head(bichette_2023)

# Or a combination of the above:
glimpse(bichette_2023)


# We are interested in location of pitches when the cross the plate
# First, I'll plot all pitch locations Bichette saw in 2023
# Note the rectangle is a very rough indication of where the strikezone might be
ggplot(data = bichette_2023) +
  geom_point(aes(x = plate_x, y = plate_z), alpha = 0.25, size = 2) +
  annotate("rect",
    xmin = -0.71, xmax = 0.71, ymin = 1.5, ymax = 3.5,
    alpha = 0,
    color = "red",
    lwd = 1
  ) +
  theme_bw()

# Next, it would be useful to know which pitches were hit vs not
# I will create a new column called hit, which is a binary variable
# 0 means the pitch was not hit or was an out
# 1 means Bichette got on base (had a hit)
# The code says if events is any of the levels in quotes, then assign hit as 1
# if none of these are true, then hit is assigned as a 0
bichette_2023$hit <- ifelse(bichette_2023$events == "home_run", 1,
  ifelse(bichette_2023$events == "single", 1,
    ifelse(bichette_2023$events == "double", 1,
      ifelse(bichette_2023$events == "triple", 1, 0)
    )
  )
)
bichette_2023$hit <- as.factor(bichette_2023$hit)

# Now I can colour hits differently on the plot
ggplot(data = bichette_2023) +
  geom_point(aes(x = plate_x, y = plate_z, colour = hit),
             alpha = 0.5, size = 2) +
  annotate("rect",
    xmin = -0.71, xmax = 0.71, ymin = 1.5, ymax = 3.5,
    alpha = 0,
    color = "red",
    lwd = 1
  ) +
  theme_bw()

# You could split the plot based on many factors in the dataset
# For example
# This is how you would plot hits against left/right handed pitchers
ggplot(data = bichette_2023) +
  geom_point(aes(x = plate_x, y = plate_z, colour = hit),
             alpha = 0.5, size = 2) +
  annotate("rect",
    xmin = -0.71, xmax = 0.71, ymin = 1.5, ymax = 3.5,
    alpha = 0,
    color = "red",
    lwd = 1
  ) +
  theme_bw() +
  facet_wrap(~p_throws)

# Bichette has faced many more pitches from right handed pitchers

# In this way you can start to explore how different variables influence hit probability.
