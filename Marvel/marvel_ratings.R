# / Info / ================================================================================================================================
# Script by Jarod Wilson.
# Jon Schwabish wrote a really good article about 5 visualisations that aren't used often, but should be.One such chart is a 
# dumbbell chart/dot plot/gap chart, which can be used to show difference/ranges in a different way to using a stacked bar chart, which can 
# be difficult to read. His article can be found here: https://policyviz.com/2021/02/08/five-charts-youve-never-used-but-should/.
# I also wanted to try writing a script that makes a cplot in a more 'step by step' kind of way. 

# With that in mind, lets create a visualisation to show critic vs. audience ratings for the Marvel 
# Cinematic Universe movies released up until February 2021, taken from https://www.rottentomatoes.com/.

# / 1 - Load libraries / ==================================================================================================================

library(tidyverse)
library(ggalt)
library(dplyr)

# / 2 - Data Preparation and Manipulation / ===============================================================================================
# Now for the annoying bit, at least for me. I can't find dataset available anywhere with the information
# I would like. Therefore, I'll have to input it myself, but this will give me some practice with dataframes.

# Inputting the data in release order according to https://en.wikipedia.org/wiki/List_of_Marvel_Cinematic_Universe_films
df <- data_frame(film=c("Iron Man (2008)", "The Incredible Hulk (2008)", "Iron Man 2 (2010)", "Thor (2011)", 
                        "Captain America: The First Avenger (2011)", "The Avengers (2012)", "Iron Man 3 (2013)", 
                        "Thor: The Dark World (2013)", "Captain America: The Winter Soldier (2014)", 
                        "Guardians of the Galaxy (2014)", "Avengers: Age of Ultron (2015)", "Ant-Man (2015)", 
                        "Captain America: Civil War (2016)", "Doctor Strange (2016)", "Guardians of the Galaxy: Vol 2 (2017)", 
                        "Spider-Man: Homecoming (2017)", "Thor: Ragnarok (2017)", "Black Panther (2018)", 
                        "Avengers: Infinity War (2018)", "Ant-Man and the Wasp (2018)", "Captain Marvel (2019)", 
                        "Avengers: Endgame (2019)", "Spiderman: Far From Home (2019)"),
                 critic_rating=c(0.94, 0.67, 0.72, 0.77, # Side note: I know the formatting of this is awful, but I wanted to keep
                                 0.80, 0.91, 0.79,       # it consistent with how I inputted the titles of the movies.
                                 0.66, 0.90,
                                 0.92, 0.83, 0.82,
                                 0.90, 0.89, 0.85,
                                 0.92, 0.93, 0.96,
                                 0.85, 0.87, 0.79,
                                 0.94, 0.91),
                 audience_rating=c(0.91, 0.70, 0.71, 0.76,
                                   0.74, 0.91, 0.78,
                                   0.75, 0.92,
                                   0.92, 0.83, 0.86,
                                   0.89, 0.85, 0.87,
                                   0.87, 0.87, 0.79,
                                   0.91, 0.75, 0.45,
                                   0.90, 0.95)) 

# I want to keep the order in the plot so they stay in release order, therefore we'll create a factor for film.

df <- arrange(df, desc(diff))
df$film<- factor(df$film, levels=rev(df$film))

# The ratings are given as percentages, but including them for every data point may cause chart clutter. So we'll just have
# a percetage symbol on the first line. I found a really efficient way to do this whilst reading this article: 
# https://rud.is/b/2016/04/17/ggplot2-exercising-with-ggalt-dumbbells/ so full credit goes to Bob Rudis.

percent_first <- function(x) {
  x <- sprintf("%d%%", round(x))
  x[2:length(x)] <- sub("%$", "", x[2:length(x)])
  x
}

# Finally, we'll create a new function within "df" to calculate the difference between the two ratings for later use.
df <- df %>%
  mutate_if(is.numeric, function(x) {x*100}) %>%
  mutate(diff = critic_rating - audience_rating)

# / 3 - Data Visualisation / ==============================================================================================================

# Before we start we'll define some colours for the data points, we'll use green for critic rating, and orange for audience rating.
green <- "#9fb059"
orange <- "#edae52"

# Creating the plot
ggplot() + 
  # We need a y axis major grid line first.
  geom_segment(data = df, aes(y = film, yend = film, x = 25, xend = 100), colour = "#b2b2b2", size = 0.15) + 
  geom_dumbbell(data = df, aes(y = film, x = critic_rating, xend = audience_rating),
                size = 1.5, color = "#b2b2b2", size_x = 3, size_xend = 3,
                colour_x = green, colour_xend = orange) +
  
  # Next we'll add labels above the first row of data points.
  geom_text(data = filter(df, film == "Iron Man (2008)"),
            aes(x = critic_rating, y = film, label = "Critic Rating"),
            color=green, size = 3, hjust = -0.05, vjust = -1.5, fontface = "bold", family = "Roboto Condensed") + 
  geom_text(data = filter(df, film == "Iron Man (2008)"),
            aes(x = audience_rating, y = film, label = "Audience Rating"),
            color = orange, size = 3, hjust = 0.95, vjust = -1.5, fontface = "bold", family = "Roboto Condensed") +
  
  # And for below them.
  geom_text(data = df, aes(x = critic_rating, y = film, label = percent_first(critic_rating)),
            color = green, size = 2.75, vjust = 2.5, family = "Roboto Condensed") +
  geom_text(data = df, aes(x = audience_rating, y = film, label = percent_first(audience_rating)),
            color = orange, size = 2.75, vjust = 2.5, family = "Roboto Condensed") +
  
  # We'll also create a column showing the difference between the critic rating and audience rating for each film.
  geom_rect(data = df, aes(xmin = 25, xmax = 35, ymin = -Inf, ymax = Inf), fill = "grey") +
  geom_text(data = df, aes(label = paste0(diff, "%"), y = film, x = 30), fontface = "bold", size = 5, family="Roboto Condensed") +
  geom_text(data = filter(df, film == "Iron Man (2008)"), aes(x = 30, y = film, label="Difference"),
          color = "black", size = 6, vjust = -2, fontface = "bold", family = "Roboto Condensed") +
  scale_y_discrete(expand=c(0.08,0)) + 
  labs(x = NULL, y = NULL,
       title = "Critic vs. Audience Ratings for the MCU",
       caption = "Source: Rotten Tomatoes\n\nDesign: Jarod Wilson") +
  
  # Now for the finishing touches.
  theme_bw(base_family = "Roboto Condensed") + 
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title=element_text(size = 16, face="bold", hjust = 0.5),
    plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
  )
      



            





                     
                         
                        