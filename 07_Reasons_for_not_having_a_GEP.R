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

# Filter only rows where No_GEP_GEDP_DEIP == 1
data <- data %>%
  filter(No_GEP_GEDP_DEIP == 1)



# Define the columns representing multiple responses
columns <- c("No_GEP_Time", "No_GEP_Resources", "No_GEP_Acceptance",
             "No_GEP_Necessity", "No_GEP_Other_Specify")

# Define new labels for the categories
labels <- c("No_GEP_Time" = "Time-consuming activities",
            "No_GEP_Resources" = "No human resources",
            "No_GEP_Acceptance" = "No acceptance",
            "No_GEP_Necessity" = "No necessity quoted",
            "GEP_Under_Pre" = "GEP under preparation",
            "No_GEP_Other_Specify" = "Other reasons not quoted"
)


# Ensure selected columns are numeric (convert characters to numeric)
data[columns] <- lapply(data[columns], function(x) as.numeric(as.character(x)))

# Count occurrences for each category (sum over columns)
counts <- colSums(data[columns], na.rm = TRUE)

# Convert to data frame for plotting
counts_df <- data.frame(Dimension = names(counts), Count = counts)

# Calculate percentages relative to total respondents (rows in data)
total_respondents <- nrow(data)
counts_df <- counts_df %>%
  mutate(Percentage = (Count / total_respondents) * 100,
         Dimension = recode(Dimension, !!!labels))  # Apply labels correctly



# Create the bar plot with thicker bars and custom labels
plot <- ggplot(counts_df, aes(y = reorder(Dimension, Percentage), x = Percentage)) +
  geom_bar(stat = "identity", fill = "darkblue", width = 0.4) +  # Make bars thicker
  labs(title = "Survey Responses for Involvement",
       x = "Percentage", 
       y = "Survey Response Categories") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_line(color = "grey80"),  # Add x-axis grid
        panel.grid.minor.x = element_line(color = "grey90")) +  # Lighter grid lines
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), hjust = -0.2) +  # Add % labels
  xlim(0, 50)  # Set x-axis maximum to 70%

print(plot)  # Display the plot



# Save the plot as a PNG file
ggsave("07_Reasons_not_having_a_GEP.png", plot = plot, width = 10, height = 8, dpi = 300)

# Confirm saved file
print("07_Reasons_not_having_a_GEP.png")