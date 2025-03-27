rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")


# Load packages
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)  
library(fmsb)
library(gridExtra)
library(grid)

# Read the Stata file
data <- read_dta("survey.dta")

# View the data
head(data)


# Convert all columns to numeric while replacing non-numeric values with NA
data <- data %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.)))))



# Define the vector of columns you want to keep
keep_variables <- c("First_GEP", "Changes_Awareness", "Changes_Balance", 
                    "Changes_Leadership", "Changes_Recruitment", "Changes_GBV", 
                    "Changes_Gen_Dim", "Reached_Awareness", "Reached_Balance", 
                    "Reached_Leadership", "Reached_Recruitment", "Reached_GBV", 
                    "Reached_Gen_Dim")

# Subset the data frame (assuming your data frame is named 'df')
data <- data[, keep_variables, drop = FALSE]





data <- data %>%
  rowwise() %>%
  mutate(stabilisations_total = mean(c_across(c("Reached_Awareness", "Reached_Balance", 
                                   "Reached_Leadership", "Reached_Recruitment", 
                                   "Reached_GBV", "Reached_Gen_Dim")), na.rm = TRUE)) %>%
  ungroup()


mean_stabilisations_total_before_2021 <- mean(data$stabilisations_total[data$First_GEP < 2021], na.rm = TRUE)
print(mean_stabilisations_total_before_2021)

mean_stabilisations_total_after_2021 <- mean(data$stabilisations_total[data$First_GEP >= 2021], na.rm = TRUE)
print(mean_stabilisations_total_after_2021)


# Perform the t-test
t_test_result <- t.test(
  data$stabilisations_total[data$First_GEP < 2021], 
  data$stabilisations_total[data$First_GEP >= 2021], 
  var.equal = TRUE, na.rm = TRUE
)


# Extract relevant statistics with readable formatting
mean_before_2021 <- round(mean(data$stabilisations_total[data$First_GEP < 2021], na.rm = TRUE), 3)
mean_after_2021 <- round(mean(data$stabilisations_total[data$First_GEP >= 2021], na.rm = TRUE), 3)

total_obs_gep <- sum(data$First_GEP < 2021, na.rm = TRUE)  # Count for GEP=1
total_obs_no_gep <- sum(data$First_GEP >= 2021, na.rm = TRUE)  # Count for GEP=0

t_statistic <- round(t_test_result$statistic, 2)
df <- round(t_test_result$parameter, 0)  # No decimals for degrees of freedom
mean_diff <- round(t_test_result$estimate[1] - t_test_result$estimate[2], 3)
conf_low <- round(t_test_result$conf.int[1], 3)
conf_high <- round(t_test_result$conf.int[2], 3)
p_value <- format(round(t_test_result$p.value, 4), scientific = FALSE)  # Avoids scientific notation

# Create a well-structured data frame WITHOUT row indexing
t_test_df <- data.frame(
  Metric = c("Mean (GEP=1)", "Mean (GEP=0)", "Total Observations (GEP=1)", 
             "Total Observations (GEP=0)", "T-Statistic", "Degrees of Freedom", 
             "Mean Difference", "Confidence Interval (Lower)", "Confidence Interval (Upper)", "P-Value"),
  Value = c(mean_before_2021, mean_after_2021, total_obs_gep, total_obs_no_gep, 
            t_statistic, df, mean_diff, conf_low, conf_high, p_value),
  stringsAsFactors = FALSE
)

# Remove row names completely
rownames(t_test_df) <- NULL

# Create a nicely formatted table plot without row indexing (rows = NULL eliminates the left-side numbering)
table_plot <- tableGrob(t_test_df, rows = NULL, theme = ttheme_default(
  core = list(fg_params = list(fontsize = 12)), 
  colhead = list(fg_params = list(fontsize = 14, fontface = "bold"))
))

png("16_Mean_of_ratings_stabilisation_before_2021_and_after_2021.png", width = 1200, height = 800, res = 150)
grid.draw(table_plot)
dev.off()

# Display the table in R
grid.draw(table_plot)
