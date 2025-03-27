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


# Define language mapping for specific countries
not_national_languages <- c("Germany", "Austria", "Switzerland", "Luxembourg", 
                            "Poland", "France", "Belgium", "Spain", 
                            "United Kingdom", "Ireland", "Malta")

data_factor <- data_factor %>%
  mutate(Language = case_when(
    !(Country %in% not_national_languages) ~ "Not in the national language",  # Default for other countries
    Country %in% c("United Kingdom", "Ireland", "Malta") ~ "English",        # Set English
    Country %in% c("Germany", "Austria", "Switzerland", "Luxembourg") ~ "German",
    Country %in% c("France", "Belgium") ~ "French",                      # Set French
    Country == "Poland" ~ "Polish",                                          # Set Polish
    Country == "Spain" ~ "Spanish",# Set Spanish
    TRUE ~ as.character(Language)  # Keep original values for other cases
  )) %>%
  mutate(Language = factor(Language))  # Convert back to factor


# Collapse data by counting occurrences of each Country
Language_counts <- data_factor %>%
  group_by(Language) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))  # Sort by count in descending order

# Print the collapsed data
print(Language_counts)


# Add the "Language_sample" column based on Language
Language_counts <- Language_counts %>%
  mutate(Language_sample = case_when(
    Language == "German" ~ 1002,  # Germany, Austria, Switzerland, and Luxembourg
    Language == "Polish" ~ 204, #Poland
    Language == "French" ~ 499, #France, Belgium
    Language == "Spanish" ~ 550, # Spain
    Language == "Not in the national language" ~ 1914, #Rest of the countries in the sample
    Language == "English" ~ 482,# UK, Ireland, and Malta"
    TRUE ~ 0  # Default value if no condition is met
  ))

# Print updated dataframe
print(Language_counts)


# Calculate response rate and replace NA with 0
Language_counts <- Language_counts %>%
  mutate(Response_Rate = round((Count / Language_sample) * 100, 2)) %>%
  replace_na(list(Response_Rate = 0))  # Replace NA with 0


# Convert dataframe to a gt table
table_plot <- Language_counts %>%
  gt() %>%
  tab_header(
    title = "Response rate by country language"
  )

# Save as PNG
gtsave(table_plot, "03_Responde_rate_by_language.png")

# Confirm saved file
print("03_Responde_rate_by_language.png")



