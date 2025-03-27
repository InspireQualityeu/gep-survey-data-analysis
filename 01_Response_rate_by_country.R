rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")


# Load packages
library(haven)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)

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
country_counts <- data_factor %>%
  group_by(Country) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))  # Sort by count in descending order

# Print the collapsed data
print(country_counts)




# Read the new dataset from an Excel file (modify the file path & sheet name)
sample_data <- read_excel("sample_data.xlsx", sheet = "Sheet1")

# Merge datasets on the "Country" column (keeping all records from both datasets)
merged_data <- full_join(country_counts, sample_data, by = "Country")

# Print the merged dataset
print(merged_data)





# Replace NA values in the Count column with 0
merged_data <- merged_data %>%
  mutate(Count = replace_na(Count, 0))

# Print the updated dataset
print(merged_data)



# Calculate response rate
merged_data <- merged_data %>%
  mutate(Response_Rate = round((Count / Sample) * 100, 2))

# Print the updated dataset with response rates
print(merged_data)


merged_data <- merged_data %>%
  arrange(desc(Response_Rate))




# Create the bar plot
p <- ggplot(merged_data, aes(y = reorder(Country, Response_Rate), x = Response_Rate)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.7) +  # Blue bars
  theme_classic() +
  labs(title = "Response Rate per Country", x = "Response Rate (%)", y = "Country") +
  scale_x_continuous(limits = c(0, 20)) +  # Limit x-axis to 20%
  theme(axis.text.y = element_text(size = 12))  # Improve readability

# Print the plot
print(p)


# Save the plot as a PNG file
ggsave("01_Response_rate_by_country.png", plot = p, width = 10, height = 8, dpi = 300)

# Confirm saved file
print("01_Response_rate_by_country.png")
