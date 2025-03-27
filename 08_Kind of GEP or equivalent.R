rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")

# Load packages
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gt)
library(webshot2)


# Read the Stata file
data <- read_dta("survey.dta")

# View the data
head(data)

# Filter only rows where No_GEP_GEDP_DEIP == 1
data <- data %>%
  filter(No_GEP_GEDP_DEIP == 0)



data <- data %>% select(GEP, GEDP, DEIP)
data <- data[, c("GEP", "GEDP", "DEIP")]

data_filtered <- data %>%
  mutate(
    # New variable: GEP equals 1 if the original GEP equals 1, else 0
    GEP = if_else(GEP == 1, 1, 0),
    
    # only_GEP equals 1 if GEP is 1 and both GEDP and DEIP are 0
    only_GEP = if_else(GEP == 1 & GEDP == 0 & DEIP == 0, 1, 0),
    
    # GEP_plus_another_plan equals 1 if GEP is 1 and at least one of GEDP or DEIP is 1
    GEP_plus_another_plan = if_else(GEP == 1 & (GEDP == 1 | DEIP == 1), 1, 0),
    
    # other_plans_no_GEP equals 1 if GEP is 0 and at least one of GEDP or DEIP is 1
    other_plans_no_GEP = if_else(GEP == 0 & (GEDP == 1 | DEIP == 1), 1, 0)
  )


# First, compute the summary with total observations and counts for each plan
summary_df <- data_filtered %>%
  summarise(
    total_observations = n(),
    total_GEP = sum(GEP, na.rm = TRUE),
    total_only_GEP = sum(only_GEP, na.rm = TRUE),
    total_GEP_plus_another_plan = sum(GEP_plus_another_plan, na.rm = TRUE),
    total_other_plans_no_GEP = sum(other_plans_no_GEP, na.rm = TRUE)
  )

# Save the total observations as a scalar
tot_obs <- summary_df$total_observations

# Reshape the summary to have one row per plan type
summary_long <- summary_df %>%
  # Remove the overall total so only plan counts remain
  select(-total_observations) %>%
  # Convert wide to long format
  pivot_longer(
    cols = everything(),
    names_to = "Kind_of_plan",
    values_to = "Observations"
  ) %>%
  # Calculate the percentage based on total observations
  mutate(Percentage = round((Observations / tot_obs) * 100, 2)) %>%
  # Optionally, recode the plan names to be more readable
  mutate(Kind_of_plan = recode(Kind_of_plan,
                               total_GEP = "GEP",
                               total_only_GEP = "Only GEP",
                               total_GEP_plus_another_plan = "GEP plus another plan",
                               total_other_plans_no_GEP = "Other plans, no GEP"))

# View the resulting summary data frame
print(summary_long)

# Append a final row with the overall total and 100% percentage
summary_long <- bind_rows(
  summary_long,
  tibble(
    Kind_of_plan = "Total",
    Observations = tot_obs,
    Percentage = 100
  )
)

print(summary_long)


# Convert dataframe to a gt table
table_plot <- summary_long %>%
  gt() %>%
  tab_header(
    title = "Response rate by country language"
  )

# Save as PNG
gtsave(table_plot, "08_Kind_of_GEP_or_equivalent.png")

# Confirm saved file
print("08_Kind_of_GEP_or_equivalent.png")

