# Project 4 - data_visualisation.R
# Bus Trip Observation Log

library(tidyverse)
library(lubridate)
library(stringr)

# Read data directly from the published CSV URL
csv_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ_2ZHK55mzQyTotFrfOHVDtVrNOUGny5iW8jE4-9b-pf9pXVBUljsz9vySqCvbD02RGtB-EpMm-Imk/pub?output=csv"

logged_data <- read_csv(csv_url, show_col_types = FALSE)

# Clean and prepare the data
bus_data <- logged_data %>%
  rename(
    timestamp = `时间戳记`,
    time_of_day = `What time of day was this bus trip?`,
    passengers = `Approximately how many passengers were on the bus when you observed it?`,
    empty_seat = `Was there an empty seat available when you got on the bus?`,
    trip_minutes = `About how many minutes did this bus trip take?`,
    crowdedness = `How crowded did the bus feel overall?`
  ) %>%
  mutate(
    passengers = as.numeric(passengers),
    trip_minutes = as.numeric(trip_minutes),
    time_of_day = factor(
      time_of_day,
      levels = c("Morning", "Afternoon", "Evening", "Night")
    ),
    crowdedness = factor(crowdedness),
    empty_seat = factor(empty_seat),
    
    # Use stringr and lubridate to work with timestamp
    date_text = str_extract(timestamp, "\\d{4}/\\d{1,2}/\\d{1,2}"),
    date = ymd(date_text),
    observation_order = row_number()
  )

# Colour theme
bus_palette <- c(
  "Morning" = "#1b9e77",
  "Afternoon" = "#d95f02",
  "Evening" = "#7570b3",
  "Night" = "#e7298a"
)

crowded_palette <- c(
  "Not crowded" = "#66c2a5",
  "Slightly crowded" = "#fc8d62",
  "Moderately crowded" = "#8da0cb",
  "Very crowded" = "#e78ac3"
)

# ------------------------------------------------------------
# Plot 1: Passenger numbers by time of day
# ------------------------------------------------------------

plot1 <- ggplot(bus_data, aes(x = time_of_day, y = passengers, fill = time_of_day)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = bus_palette, na.value = "grey70") +
  labs(
    title = "Passenger numbers varied by time of day",
    subtitle = "Each observation represents one observed bus trip",
    x = "Time of day",
    y = "Approximate number of passengers",
    caption = "Data source: Bus Trip Observation Log"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

plot1

ggsave("plot1.png", plot1, width = 8, height = 5, dpi = 300)


# ------------------------------------------------------------
# Plot 2: Average passengers by time of day
# This uses group_by() and summarise().
# ------------------------------------------------------------

passengers_by_time <- bus_data %>%
  filter(!is.na(time_of_day), !is.na(passengers)) %>%
  group_by(time_of_day) %>%
  summarise(
    mean_passengers = mean(passengers, na.rm = TRUE),
    number_of_trips = n(),
    .groups = "drop"
  ) %>%
  arrange(time_of_day)

plot2 <- ggplot(passengers_by_time, aes(x = time_of_day, y = mean_passengers, fill = time_of_day)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = bus_palette, na.value = "grey70") +
  labs(
    title = "Average passenger numbers were different across the day",
    subtitle = "Calculated from grouped bus trip observations",
    x = "Time of day",
    y = "Mean number of passengers",
    caption = "Data source: Bus Trip Observation Log"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )

plot2

ggsave("plot2.png", plot2, width = 8, height = 5, dpi = 300)


# ------------------------------------------------------------
# Plot 3: Trip duration and passenger numbers
# This uses mutate() and arrange().
# ------------------------------------------------------------

busy_trips <- bus_data %>%
  filter(!is.na(trip_minutes), !is.na(passengers)) %>%
  mutate(
    passenger_group = case_when(
      passengers < 20 ~ "Fewer than 20 passengers",
      passengers < 40 ~ "20 to 39 passengers",
      TRUE ~ "40 or more passengers"
    )
  ) %>%
  arrange(trip_minutes)

plot3 <- ggplot(busy_trips, aes(x = trip_minutes, y = passengers, colour = crowdedness)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_colour_manual(values = crowded_palette, na.value = "grey70") +
  labs(
    title = "Trip duration and passenger numbers",
    subtitle = "Passenger numbers are compared with trip duration",
    x = "Approximate trip duration in minutes",
    y = "Approximate number of passengers",
    colour = "Crowdedness",
    caption = "Data source: Bus Trip Observation Log"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

plot3

ggsave("plot3.png", plot3, width = 8, height = 5, dpi = 300)


# ------------------------------------------------------------
# Plot 4: Number of observations by date
# This uses timestamp data and lubridate.
# ------------------------------------------------------------

observations_by_date <- bus_data %>%
  filter(!is.na(date)) %>%
  group_by(date) %>%
  summarise(
    observations = n(),
    .groups = "drop"
  )

plot4 <- ggplot(observations_by_date, aes(x = date, y = observations)) +
  geom_col(width = 0.6, fill = "#1b9e77") +
  labs(
    title = "Bus trip observations were collected across different dates",
    subtitle = "Timestamp data were converted into dates using lubridate",
    x = "Date",
    y = "Number of observations logged",
    caption = "Data source: Bus Trip Observation Log"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

plot4

ggsave("plot4.png", plot4, width = 8, height = 5, dpi = 300)