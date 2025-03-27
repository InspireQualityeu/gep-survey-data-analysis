rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")

# Load packages
library(haven)
library(dplyr)
library(ggplot2)
library(ggplot2)
library(dplyr)

# Read the Stata file
data <- read_dta("survey.dta")

# View the data
head(data)


# Define the columns representing multiple responses
columns <- c("Involved_GE_Officer", "Involved_GE_Unit", "Involved_Staff",
             "Involved_Management", "Involved_Leadership", "Involved_Other")

# Define new labels for the categories
labels <- c("Involved_GE_Officer" = "Gender equality officer",
            "Involved_GE_Unit" = "Gender equality unit",
            "Involved_Staff" = "Administration staff",
            "Involved_Management" = "Member of the top management",
            "Involved_Leadership" = "Staff linked to the leadership",
            "Involved_Other" = "Other")

# Ensure data is numeric
data[, columns] <- lapply(data[, columns], as.numeric)

# Count occurrences for each category (sum over columns)
counts <- colSums(data[, columns], na.rm = TRUE)

# Convert to data frame for plotting
counts_df <- data.frame(Dimension = names(counts), Count = counts)

# Calculate percentages relative to total respondents (rows in data)
total_respondents <- nrow(data)
counts_df <- counts_df %>%
  mutate(Percentage = (Count / total_respondents) * 100,
         Dimension = factor(Dimension, levels = names(labels), labels = labels))  # Apply custom labels

# Create the bar plot with thicker bars and custom labels
plot <- ggplot(counts_df, aes(y = reorder(Dimension, Percentage), x = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.4) +  # Make bars thicker
  labs(title = "Survey Responses for Involvement",
       x = "Percentage", 
       y = "Survey Response Categories") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80"),  # Add x-axis grid
        panel.grid.minor.x = element_line(color = "grey90")) +  # Lighter grid lines
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2) +  # Add % labels
  xlim(0, 70)  # Set x-axis maximum to 70%

print(plot)  # Display the plot



# Save the plot as a PNG file
ggsave("05_People_and_functions_fullfilling_the_survey.png", plot = plot, width = 10, height = 8, dpi = 300)

# Confirm saved file
print("05_People_and_functions_fullfilling_the_survey.png")