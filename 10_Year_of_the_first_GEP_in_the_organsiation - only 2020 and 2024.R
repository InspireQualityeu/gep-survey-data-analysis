rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")


# Load packages
library(haven)
library(dplyr)
library(ggplot2)

# Read the Stata file
data <- read_dta("survey.dta")

# View the data
head(data)

data <- data[, "First_GEP", drop = FALSE]  # Keeps data as a data frame

data <- data %>%
  filter(!First_GEP %in% c("No answer", "Not edited", "Not known", "Not seen the question"))

data$First_GEP <- as.numeric(data$First_GEP)


# Create a new dataframe with year counts
year_counts <- data %>%
  group_by(First_GEP) %>%
  summarise(Observations = n()) %>%
  ungroup() %>%
  mutate(Percentage = (Observations / nrow(data)) * 100)  # Calculate percentage


library(ggplot2)
library(scales)

# Create the bar chart
plot <- ggplot(year_counts, aes(x = factor(First_GEP), y = Percentage)) +
  geom_bar(stat = "identity", fill = "darkblue", color = "black", width = 0.7) +  
  scale_x_discrete(limits = as.character(2020:2024)) +  # Show only 2020-2024 on x-axis
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10), labels = percent_format(scale = 1)) +  
  theme_classic() +  
  labs(title = "Percentage of First GEP Occurrences (2020-2024)", 
       x = "Year", 
       y = "Percentage (Relative to Full Data)") +  
  theme(
    panel.grid.major = element_line(color = "gray"),  
    panel.grid.minor = element_line(color = "lightgray"),  
    axis.text.x = element_text(angle = 0, hjust = 0.5)  
  )

print(plot)


# Save the plot as a PNG file
ggsave("10_Year_of_the_first_GEP_in_the_organsiation - only 2020 and 2024.png", plot = plot, width = 10, height = 8, dpi = 300)

# Confirm saved file
print("10_Year_of_the_first_GEP_in_the_organsiation - only 2020 and 2024.png")
