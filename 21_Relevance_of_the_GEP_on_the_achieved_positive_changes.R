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

data <- data %>%
  filter(No_GEP_GEDP_DEIP != 1)

data <- data %>%
  mutate(mean = mean(Positive_Change, na.rm = TRUE))

data <- data[, c("Positive_Change", "mean"), drop = FALSE]  # Keeps data as a data frame

# Compute the numeric mean value (after removing NA values)
mean_value <- mean(data$Positive_Change, na.rm = TRUE)

# Create bar chart with thicker bars, mean line, and star symbol at the top
plot <- ggplot(data, aes(x = Positive_Change)) +
  geom_bar(aes(y = after_stat(count) / sum(after_stat(count))), 
           fill = "blue", color = "black", width = 0.4) +  # Thicker bars (width = 0.9)
  scale_x_continuous(
    breaks = 0:5,  # Define breaks
    labels = c("0" = "No influence", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "High influence")  # Custom labels
  ) +
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.7, 0.1), labels = scales::percent_format()) +  # Convert y-axis to percentages
  geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", linewidth = 1) +  # Mean line
  annotate("text", x = mean_value +0.3, y = 0.28, 
           label = paste0("μ = ", round(mean_value, 2)),  # Display mu symbol with mean value
           size = 6, color = "black", fontface = "bold") +
  # Star at the top of mean line
  theme_classic() +  # Keep grid
  labs(title = "", x = "", y = "Percentage") +  
  theme(
    panel.grid.major = element_line(color = "gray"),  # Ensure major grid lines
    panel.grid.minor = element_line(color = "lightgray"),  # Ensure minor grid lines
    axis.text.x = element_text(angle = 0, hjust = 0.5)  # Center align x-axis labels
  )

# Print the plot
print(plot)

# Save the plot as a PNG file
ggsave("21_Relevance_of_the_GEP_on_the_achieved_positive_changes.png", plot = plot, width = 10, height = 8, dpi = 300)

# Confirm saved file
print("21_Relevance_of_the_GEP_on_the_achieved_positive_changes.png")