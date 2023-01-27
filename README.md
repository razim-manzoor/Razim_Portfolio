# Import necessary packages and libraries
install.packages(c("dplyr", "readr", "tidyr","tidyverse","lubridate"))
library(dplyr)
library(readr)
library(tidyr)
library(tidyverse)
library(lubridate)

# Set directory where the files are located
setwd("D:/Projects/Capstone/Track 1/Data")

# Import data (could also use the "lapply" function)
Jan <- read_csv("D:/Projects/Capstone/Track 1/Data/202101-divvy-tripdata.csv")
Feb <- read_csv("D:/Projects/Capstone/Track 1/Data/202102-divvy-tripdata.csv")
Mar <- read_csv("D:/Projects/Capstone/Track 1/Data/202103-divvy-tripdata.csv")
Apr <- read_csv("D:/Projects/Capstone/Track 1/Data/202104-divvy-tripdata.csv")
May <- read_csv("D:/Projects/Capstone/Track 1/Data/202105-divvy-tripdata.csv")
Jun <- read_csv("D:/Projects/Capstone/Track 1/Data/202106-divvy-tripdata.csv")
Jul <- read_csv("D:/Projects/Capstone/Track 1/Data/202107-divvy-tripdata.csv")
Aug <- read_csv("D:/Projects/Capstone/Track 1/Data/202108-divvy-tripdata.csv")
Sep <- read_csv("D:/Projects/Capstone/Track 1/Data/202109-divvy-tripdata.csv")
Oct <- read_csv("D:/Projects/Capstone/Track 1/Data/202110-divvy-tripdata.csv")
Nov <- read_csv("D:/Projects/Capstone/Track 1/Data/202111-divvy-tripdata.csv")
Dec <- read_csv("D:/Projects/Capstone/Track 1/Data/202112-divvy-tripdata.csv")

# Bind all of the data frames together into one single data frame
data_list <- list(Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
cyclist_data <- bind_rows(data_list)

# Clean Data - Format date and time/Create a column for duration
cyclist_data$started_at = ymd_hms(cyclist_data$started_at)
cyclist_data$ended_at = ymd_hms(cyclist_data$ended_at)
cyclist_data$duration = as.numeric(difftime(cyclist_data$ended_at, cyclist_data$started_at, units = "mins"))

# Create a column for member type
cyclist_data <- cyclist_data %>%
  mutate(member_type = ifelse(member_casual == "casual", "casual","annual"))

# Removing missing values
cyclist_data <- cyclist_data %>%
  filter(!is.na(duration))

# Removing duplicates
cyclist_data <- cyclist_data %>%
  distinct()

# Check for outliers
boxplot(cyclist_data$duration, main = "Duration of Rides", xlab = "Duration (minutes)")

# Removing outliers
cyclist_data <- cyclist_data %>%
  filter(cyclist_data$duration > quantile(cyclist_data$duration, 0.95))

# Create a summary of the data
summary_data <- cyclist_data %>%
  group_by(member_type) %>%
  summarise(avg_duration = mean(duration),
            median_duration = median(duration),
            total_rides = n(),
            avg_rides_per_week = total_rides/52)

# Plot the average duration of rides by member type
ggplot(cyclist_data, aes(x = member_type, y = duration))+
  geom_boxplot(fill = "#0077c9")+
  ggtitle("Average Duration of Rides by Member Types (Minutes)")+
  xlab("Member Type") + ylab("Duration (Minutes)")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12)) +
  geom_hline(yintercept = median(cyclist_data$duration), color = "#ffc107", linetype = "dashed")+
  geom_vline(xintercept = c("Customer", "Subscriber"), color = "#ffc107", linetype = "dashed")

# Plot the total number of rides by member type
ggplot(cyclist_data, aes(x = member_type, fill = member_type))+
  geom_bar(stat = "count", color = "black")+
  ggtitle("Total Number of Rides by Member Type")+
  xlab("Member Type")+
  ylab("Number of Rides")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.5))

# Plot the average number of rides per week by member type
ggplot(summary_data, aes(x = member_type, y = avg_rides_per_week)) +
  geom_col(aes(fill = member_type), width = 0.8, color = "black") +
  ggtitle("Average Number of Rides per Week by Member Type") +
  xlab("Member Type") + ylab("Average Number of Rides per Week") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_blank(),
        panel.grid.major = element_line(color = "lightgray", size = 0.5))

