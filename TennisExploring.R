# Amilcar Gomez Samayoa
# Final Project
# CDS 301 - 002

# Tennis Exploring

# Data Obtained : https://www.kaggle.com/datasets/gmadevs/atp-matches-dataset
# Data : atp_matches_2016.csv

library(ggplot2)

library(dplyr)
library(base)

# read in data

library(readr)
tennis <- read_csv("/Users/Amilcar/Desktop/FA 22/CDS 303/atp_matches_2016.csv", show_col_types = FALSE)

# remove commas in this cell
tennis$l_bpFaced <- as.numeric(gsub(",","",tennis$l_bpFaced))

# update data by creating column of total double faults in match
t <- tennis %>%
  filter(surface != "Carpet", minutes < 1000) %>%  # only include legit surfaces and do not include outlier match
  mutate(Dfaults = w_df + l_df, aces = w_ace + l_ace, Tpoints = w_svpt + l_svpt)  # get total points, aces, and double faults


#  3 different data sets to run tests on data easier
hard <- filter(t, surface == "Hard")
clay <- filter(t, surface == "Clay")
grass <- filter(t, surface == "Grass")
carpet <- filter(tennis, surface == "Carpet")    # Carpet is a thing ??


# Regression on Aces (increasing trend which correlates with surface speed)
lm(aces ~ Tpoints, clay) # 0.05914
lm(aces ~ Tpoints, hard) # 0.08042
lm(aces ~ Tpoints, grass) # 0.1073


# Regression on Double Faults (no correlation)
lm(Dfaults ~ Tpoints, clay) # 0.02873
lm(Dfaults ~ Tpoints, hard) # 0.04318
lm(Dfaults ~ Tpoints, grass) # 0.03198



######   DATA VISUALIZATIONS  ######

## best visualization, regression line per surface, scatterplot
ggplot(t, aes(x = Tpoints, y = aces, color = surface))+
  geom_point() +
  geom_smooth(method='lm') +
  labs(
    title = "2016 ATP Match Aces by Length of Match",
    x = "Total Service Points",
    y = "Total Aces in Match"
  )



##  Plot of num of matches per court surface  - not very useful
ggplot() +
  geom_bar(mapping = aes(x = t$surface, fill = t$surface)) +
  labs(
    title = "ATP matches by court surface 2016",
    x = "Court Surface",
    y = "Number of Matches"
  )

# Histogram of number of aces per match, overlapping
ggplot() +
  geom_bar(mapping = aes(x = t$aces, fill = t$surface)) +
  labs(
    title = "ATP matches by court surface",
    x = "Aces",
    y = "Frequency"
  )


## Histogram of average aces per match by court
ggplot(t,aes(x = factor(surface), y = aces, fill = surface)) + 
  geom_bar(stat = "summary", fun = "mean") +
  labs(
    title = "Average ATP Ace per match",
    x = "Average Aces",
    y = "Court Surfaces"
  )
