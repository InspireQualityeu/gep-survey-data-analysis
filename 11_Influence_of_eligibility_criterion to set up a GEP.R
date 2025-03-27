rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")


# Load packages
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)

# Read the Stata file
data <- read_dta("survey.dta")

# Filter only rows where No_GEP_GEDP_DEIP == 1
data <- data %>%
  filter(No_GEP_GEDP_DEIP == 0)

data <- data[, "Horizon_Europe", drop = FALSE]  # Keeps data as a data frame

# View variable labels for all variables
sapply(data, function(x) attr(x, "label"))

# Convert all labelled variables to factors
data_factor <- as_factor(data)

# View the first few rows
head(data_factor)

data_factor <- data_factor[data_factor$Horizon_Europe != "No answer", ]

library(ggplot2)
library(dplyr)
library(stringr)

# Compute frequency counts
data_for_plot <- as.data.frame(table(data_factor$Horizon_Europe))

# Rename columns for clarity
colnames(data_for_plot) <- c("Dimension", "Count")

# Remove "No answer" and "Not seen the question"
data_for_plot <- data_for_plot %>%
  filter(!Dimension %in% c("No answer", "Not seen the question"))

# Calculate percentages
data_for_plot <- data_for_plot %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

# Function to wrap text into multiple lines (max 20 characters per line)
wrap_text <- function(text, width = 50) {
  str_wrap(text, width = width)
}

# Apply wrapping to y-axis labels
data_for_plot$Dimension <- sapply(data_for_plot$Dimension, wrap_text)

# Create the bar plot
plot <- ggplot(data_for_plot, aes(y = reorder(Dimension, Percentage), x = Percentage)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.4) +
  labs(title = "",
       x = "Percentage", 
       y = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.minor.x = element_line(color = "grey90")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2) +
  xlim(0, 50)

print(plot)  # Display the plot

# Save the plot as a PNG file
ggsave("11_Influence_of_eligibility_criterion_to_set_up_a_GEP.png", plot = plot, width = 10, height = 8, dpi = 300)

# Confirm saved file
print("11_Influence_of_eligibility_criterion_to_set_up_a_GEP.png")