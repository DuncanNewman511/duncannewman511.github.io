# ------------------------------------------------------------
# 🚗 Chicago Traffic Speed Analysis & Modeling
# ------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(caret)
library(factoextra)
library(cluster)
library(ggpubr)
library(viridis)

# 1️⃣ Load Dataset
# Replace with your actual file path:
data <- read_excel("C:/Users/dunca/OneDrive/Desktop/ENMA 754/Project/Chicago_Traffic_Tracker_-_Congestion_Estimates_by_Segments_20251110.xlsx")

# Inspect data
glimpse(data)

# Clean column names if necessary
data <- data %>%
  rename(
    segment_id = SEGMENTID,
    street = STREET,
    direction = DIRECTION,
    from_street = FROM_STREET,
    to_street = TO_STREET,
    length_miles = LENGTH,
    heading = STREET_HEADING,
    start_lon = START_LONGITUDE,
    start_lat = START_LATITUDE,
    end_lon = END_LONGITUDE,
    end_lat = END_LATITUDE,
    current_speed = CURRENT_SPEED,
    timestamp = LAST_UPDATED
  )

# Convert timestamp to POSIXct
data$current_speed <- as.numeric(data$current_speed)
# Replace invalid speeds with NA
data$current_speed[data$current_speed < 0] <- NA

# ------------------------------------------------------------
# 2️⃣ Speed Analysis: Histogram + KDE Plot
# ------------------------------------------------------------



ggplot(data, aes(x = current_speed)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "skyblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", linewidth = 1.2) +
  labs(
    title = "Speed Distribution of Traffic Segments",
    x = "Current Speed (mph)", y = "Density"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 3️⃣ Exploratory Spatial + Temporal Analysis
# ------------------------------------------------------------

# ⚙️ Handle missing values and basic summary
summary(data$current_speed)
sum(is.na(data$current_speed))

# 🕓 Temporal trend: average speed by hour
data %>%
  mutate(hour = hour(timestamp)) %>%
  group_by(hour) %>%
  summarise(mean_speed = mean(current_speed, na.rm = TRUE)) %>%
  ggplot(aes(x = hour, y = mean_speed)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(size = 2, color = "darkblue") +
  labs(
    title = "Average Speed by Hour of Day",
    x = "Hour", y = "Mean Speed (mph)"
  ) +
  theme_minimal()

# 🌎 Spatial view: map start points colored by speed
spatial_data <- st_as_sf(
  data,
  coords = c("start_lon", "start_lat"),
  crs = 4326
)

ggplot(spatial_data) +
  geom_sf(aes(color = current_speed), size = 2, alpha = 0.8) +
  scale_color_viridis(option = "plasma", na.value = "grey50") +
  labs(title = "Spatial Distribution of Current Speed",
       color = "Speed (mph)") +
  theme_minimal()

# ------------------------------------------------------------
# 4️⃣ Predictive Modeling: Predict Current Speed
# ------------------------------------------------------------

# Create features for modeling
data_model <- data %>%
  mutate(
    hour = hour(timestamp),
    day_of_week = wday(timestamp, label = TRUE)
  ) %>%
  select(current_speed, hour, day_of_week, direction, length_miles, heading)

# Remove rows with missing values
data_model <- na.omit(data_model)

# Split into training/testing
set.seed(123)
trainIndex <- createDataPartition(data_model$current_speed, p = 0.8, list = FALSE)
train <- data_model[trainIndex, ]
test <- data_model[-trainIndex, ]

# Random Forest model
model <- train(
  current_speed ~ .,
  data = train,
  method = "rf",
  trControl = trainControl(method = "cv", number = 5)
)

# Evaluate
pred <- predict(model, newdata = test)
results <- postResample(pred, test$current_speed)
print(results)


# ------------------------------------------------------------
# 5️⃣ Clustering: Identify Time Periods with Similar Speed Patterns
# ------------------------------------------------------------

# Aggregate average speed by hour
speed_hour <- data %>%
  mutate(hour = hour(timestamp)) %>%
  group_by(hour) %>%
  summarise(mean_speed = mean(current_speed, na.rm = TRUE))

# Determine optimal number of clusters
fviz_nbclust(speed_hour[, "mean_speed"], kmeans, method = "wss") +
  labs(title = "Optimal Number of Clusters for Hourly Speed")

# Apply K-means clustering
set.seed(42)
kmeans_result <- kmeans(speed_hour[, "mean_speed"], centers = 3)
speed_hour$cluster <- as.factor(kmeans_result$cluster)

# Visualize clustered time periods
ggplot(speed_hour, aes(x = hour, y = mean_speed, color = cluster)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Clustering of Hourly Speed Patterns",
    x = "Hour of Day", y = "Average Speed (mph)"
  ) +
  theme_minimal()