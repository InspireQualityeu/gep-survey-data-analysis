rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")

# Load packages
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)    
library(fmsb)



# Read the Stata file
data <- read_dta("survey.dta")

# View the data
head(data)


# --- Calculate GEP Percentages ---

# Create the new variable GEP_unique
data <- data %>%
  mutate(GEP_unique = if_else(GEP == 1 | GEDP == 1 | DEIP == 1, 1, 0))

# Count total number of organizations per type
total_by_type <- data %>%
  group_by(Organisation_type) %>%
  summarise(Total = n())

# Count number of organizations with GEP per type
with_GEP_by_type <- data %>%
  filter(GEP_unique == 1) %>%
  group_by(Organisation_type) %>%
  summarise(With_GEP = n())

# Merge both counts and calculate percentage
GEP_Percentages <- total_by_type %>%
  left_join(with_GEP_by_type, by = "Organisation_type") %>%
  mutate(With_GEP = replace_na(With_GEP, 0),
         Percentage = round((With_GEP / Total) * 100,2)) %>%
  arrange(desc(Percentage))

print(GEP_Percentages)

# Calculate overall totals
total_row <- tibble(
  Organisation_type = "Total",
  Total = sum(GEP_Percentages$Total),
  With_GEP = sum(GEP_Percentages$With_GEP),
  Percentage = round((sum(GEP_Percentages$With_GEP) / sum(GEP_Percentages$Total)) * 100, 2)
)


GEP_Percentages$Organisation_type <- as_factor(GEP_Percentages$Organisation_type)

# Append total row
GEP_Percentages <- bind_rows(GEP_Percentages, total_row)


# Convert Organisation_type to factor
GEP_Percentages <- GEP_Percentages %>%
  mutate(Organisation_type = as_factor(Organisation_type))

# Reshape the data
reshaped_data <- GEP_Percentages %>%
  select(Organisation_type, Percentage) %>%  # Keep only relevant columns
  pivot_wider(names_from = Organisation_type, values_from = Percentage)

# Print result
print(reshaped_data)


reshaped_data <- rbind(rep(100,5) , rep(0,5) , reshaped_data)


# Save the radar chart as a PNG file
png("06_Prevalence_survey_organisation_type.png", width = 1000, height = 800)  # Adjust size as needed

# Custom the radarChart with 20% increments
radarchart(reshaped_data, axistype = 1, seg = 5,
           
           # Custom polygon
           pcol = rgb(0.2, 0.5, 0.5, 0.9),
           pfcol = rgb(0.2, 0.5, 0.5, 0.5),
           plwd = 5, 
           
           # Custom the grid
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey",
           caxislabels = paste0(seq(0, 100, 20), "%"),  # Ensure 20% steps
           cglwd = 0.8,
           
           # Custom labels
           vlcex = 0.8 
)
dev.off()  # Close the device and save the file

