library(tidyverse)
library(readxl)
library(GGally)
library(ggcorrplot)

# Load dataset
mlb <- read_excel("C:/Users/dunca/OneDrive/Desktop/CS 625/Final_Project/mlbpitching.xlsx")

# Preview
glimpse(mlb)

ggplot(mlb, aes(x = ERA)) +
  geom_histogram(binwidth = 0.1, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of ERA", x = "ERA", y = "Frequency")

ggplot(mlb, aes(x = year, y = ERA)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(title = "ERA Trends Over Time", x = "Year", y = "ERA")

ggplot(mlb, aes(x = year, y = strikeouts)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "forestgreen") +
  theme_minimal() +
  labs(title = "Strikeouts Over Time", x = "Year", y = "Total Strikeouts")

ggplot(mlb, aes(x = year, y = complete_game)) +
  geom_line(color = "navy", size = 1) +
  geom_point(color = "blue") +
  theme_minimal() +
  labs(title = "Complete Games Over Time", x = "Year", y = "Number of Complete Games")

ggplot(mlb, aes(x = strikeouts, y = ERA)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  theme_minimal() +
  labs(title = "ERA vs. Strikeouts", x = "Strikeouts", y = "ERA")

key_metrics <- mlb %>%
  select(ERA, strikeouts, complete_game, WHIP, hits_9, walks_9)

corr_matrix <- cor(key_metrics, use = "complete.obs")

ggcorrplot(corr_matrix, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix of Key Pitching Metrics",
           ggtheme = theme_minimal())

pdf("mlb_pitching_plots.pdf", width = 7, height = 5)
print(ggplot(mlb, aes(x = ERA)) + geom_histogram(binwidth = 0.1))
print(ggplot(mlb, aes(x = year, y = ERA)) + geom_line())
print(ggplot(mlb, aes(x = year, y = strikeouts)) + geom_line())
print(ggplot(mlb, aes(x = year, y = complete_game)) + geom_line())
print(ggplot(mlb, aes(x = strikeouts, y = ERA)) + geom_point())
dev.off()