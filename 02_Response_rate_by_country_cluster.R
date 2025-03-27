rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")

# Load packages
library(haven)
library(dplyr)
library(tidyr)
library(gt)
library(webshot2)
library(readxl)

# Read the Stata file
data <- read_dta("survey.dta")

# View the data
head(data)


# Step 1: Create a new variable for the region
data <- data %>%
  mutate(Country_Cluster = case_when(
    Country %in% c(7, 12, 15, 20, 23, 31) ~ "North West",
    Country %in% c(1, 2, 8, 13, 19, 4, 28) ~ "Central West",
    Country %in% c(5, 10, 16, 22, 11, 18) ~ "Southern",
    Country %in% c(3, 6, 9, 14, 17, 24, 21, 29, 33, 30, 27, 32, 44) ~ "Central East and Eastern",
    TRUE ~ ""  # Default to an empty string if no match
  ))



# View variable labels for all variables
sapply(data, function(x) attr(x, "label"))

# Convert all labelled variables to factors
data_factor <- as_factor(data)

# View the first few rows
head(data_factor)

# View only Country and Country_Cluster columns
data_selected <- data_factor %>%
  select(Country, Country_Cluster)

# Display the selected columns
print(data_selected)


# Collapse data by counting occurrences of each Country
country_cluster_counts <- data_factor %>%
  group_by(Country_Cluster) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))  # Sort by count in descending order

# Print the collapsed data
print(country_cluster_counts)


# Read the new dataset from an Excel file (modify the file path & sheet name)
sample_data <- read_excel("sample_data.xlsx", sheet = "Sheet1")

# Step 1: Create a new variable for the region
sample_data_cluster <- sample_data %>%
  mutate(Country_Cluster = case_when(
    `Country Cluster` == 1 ~ "North West",
    `Country Cluster` == 2 ~ "Central West",
    `Country Cluster` == 3 ~ "Southern",
    `Country Cluster` == 4 ~ "Central East and Eastern",
    `Country Cluster` == 5 ~ "Not belonging to a cluster",
    TRUE ~ ""  # Default to an empty string if no match
  ))



country_cluster_sample_counts <- sample_data_cluster %>%
  group_by(Country_Cluster) %>%
  summarise(Total_Sample = sum(Sample, na.rm = TRUE)) %>%  # Summing Sample values
  arrange(desc(Total_Sample))  # Sorting in descending order

# Print the collapsed data
print(country_cluster_sample_counts)


# Merge datasets on the "Country" column (keeping all records from both datasets)
merged_data <- full_join(country_cluster_counts, country_cluster_sample_counts, by = "Country_Cluster")

merged_data <- merged_data %>%
  mutate(Count = replace_na(Count, 0))

# Print the merged dataset
print(merged_data)


# Calculate response rate and replace NA with 0
merged_data <- merged_data %>%
  mutate(Response_Rate = round((Count / Total_Sample) * 100, 2)) %>%
  replace_na(list(Response_Rate = 0))  # Replace NA with 0


# Print the updated dataset
print(merged_data)


# Convert dataframe to a gt table
table_plot <- merged_data %>%
  gt() %>%
  tab_header(
    title = "Response rate by country cluster"
  )

# Save as PNG
gtsave(table_plot, "02_Responde_rate_by_country_cluster.png")

# Confirm saved file
print("02_Responde_rate_by_country_cluster.png")







