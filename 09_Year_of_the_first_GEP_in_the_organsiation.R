rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")

# Load packages
library(haven)
library(dplyr)
library(ggplot2)
library(scales)

# Read the Stata file
data <- read_dta("survey.dta")

# View the data
head(data)

data <- data[, "First_GEP", drop = FALSE]  # Keeps data as a data frame


data <- data %>%
  filter(!First_GEP %in% c("No answer", "Not edited", "Not known", "Not seen the question"))

data$First_GEP <- as.numeric(data$First_GEP)




data <- data %>%
  mutate(GEP_interval = case_when(
    First_GEP >= 1980 & First_GEP <= 1989 ~ "1980-1989",
    First_GEP >= 1990 & First_GEP <= 1999 ~ "1990-1999",
    First_GEP >= 2000 & First_GEP <= 2009 ~ "2000-2009",
    First_GEP >= 2010 & First_GEP <= 2019 ~ "2010-2019",
    First_GEP >= 2020 & First_GEP <= 2024 ~ "2020-2024",
    TRUE ~ NA_character_  # Assign NA if the value does not fit in any range
  ))



# Ensure GEP_interval is a factor before plotting
data <- data %>%
  mutate(GEP_interval = factor(GEP_interval, 
                               levels = c("1980-1989", "1990-1999", "2000-2009", "2010-2019", "2020-2024")))

# Create bar chart showing percentages
plot <- ggplot(data, aes(x = GEP_interval)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count))), 
           fill = "darkblue", color = "black", width = 0.7) +  # Adjust width for thicker bars  
  scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.7, 0.1), labels = percent_format()) +  # Convert y-axis to percentages
  theme_classic() +  # Keep grid
  labs(title = "Percentage of GEP Intervals", x = "GEP Interval", y = "Percentage") +  
  theme(
    panel.grid.major = element_line(color = "gray"),  # Ensure major grid lines
    panel.grid.minor = element_line(color = "lightgray"),  # Ensure minor grid lines
    axis.text.x = element_text(angle = 0, hjust = 0.5)  # Center align x-axis labels
  )

print(plot)

# Save the plot as a PNG file
ggsave("09_Year_of_the_first_GEP_in_the_organisations.png", plot = plot, width = 10, height = 8, dpi = 300)

# Confirm saved file
print("09_Year_of_the_first_GEP_in_the_organisations.png")


