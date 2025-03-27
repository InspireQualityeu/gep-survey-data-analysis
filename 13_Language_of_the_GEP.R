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

# Filter only rows where No_GEP_GEDP_DEIP == 0
data <- data %>%
  filter(No_GEP_GEDP_DEIP == 0)


selected_columns <- c("Nat_lang", "Eng_lang", "Oth_lang")

data <- data[,  selected_columns, drop = FALSE]  # Keeps data as a data frame


# Define the columns representing multiple responses
columns <- c("Nat_lang", "Eng_lang", "Oth_lang")

# Define new labels for the categories
labels <- c("Nat_lang" = "National language/s",
            "Eng_lang" = "English",
            "Oth_lang" = "Other_language/s"
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
  geom_text(aes(label = Count), hjust = -0.2)+  # Add % labels
  xlim(0, 100)  # Set x-axis maximum to 70%

print(plot)  # Display the plot



# Save the plot as a PNG file
ggsave("13_Language_of_the_GEP.png", plot = plot, width = 10, height = 8, dpi = 300)

# Confirm saved file
print("13_Language_of_the_GEP.png")

