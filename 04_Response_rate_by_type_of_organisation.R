rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")

# Load packages
library(haven)
library(dplyr)
library(gt)
library(webshot2)

# Read the Stata file
data <- read_dta("survey.dta")

# View the data
head(data)

# View variable labels for all variables
sapply(data, function(x) attr(x, "label"))

# Convert all labelled variables to factors
data_factor <- as_factor(data)

# View the first few rows
head(data_factor)

# Collapse data by counting occurrences of each Country
Organisation_counts <- data_factor %>%
  group_by(Organisation_type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))  # Sort by count in descending order

# Print the collapsed data
print(Organisation_counts)



# Add the "Language_sample" column based on Language
Organisation_counts <- Organisation_counts %>%
  mutate(Organisation_sample = case_when(
    Organisation_type == "Higher Education Institution" ~ 1314,  # HES
    Organisation_type == "Research organisation" ~ 2042,  # REC
    Organisation_type == "Research Funding Organisation" ~ 42,  # RFO
    Organisation_type == "Private Company" ~ 1253,  # PRC
    TRUE ~ 0  # Default value if no condition is met
  ))

# Print updated dataframe
print(Organisation_counts)




# Calculate response rate and replace NA with 0
Organisation_counts <- Organisation_counts %>%
  mutate(Response_Rate = round((Count / Organisation_sample) * 100, 2)) %>%
  replace_na(list(Response_Rate = 0))  # Replace NA with 0

# Convert dataframe to a gt table
table_plot <- Organisation_counts %>%
  gt() %>%
  tab_header(
    title = "Response rate by country type of organisation"
  )

# Save as PNG
gtsave(table_plot, "04_Responde_rate_by_type_of_organisation.png")

# Confirm saved file
print("04_Responde_rate_by_type_of_organisation.png")



