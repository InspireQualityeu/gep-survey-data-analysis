rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")

# Load haven
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


# Filter only rows where No_GEP_GEDP_DEIP == 0
data <- data %>%
  filter(Int_financial_res == 1 | Int_financial_res ==2 | Int_financial_res ==3)


# Convert all columns to numeric while replacing non-numeric values with NA
data <- data %>%
  mutate(across(everything(), ~ suppressWarnings(as.numeric(as.character(.)))))



# Define the vector of columns you want to keep
keep_variables <- c("Int_financial_res", "Changes_Awareness", "Changes_Balance", 
                    "Changes_Leadership", "Changes_Recruitment", "Changes_GBV", 
                    "Changes_Gen_Dim", "Reached_Awareness", "Reached_Balance", 
                    "Reached_Leadership", "Reached_Recruitment", "Reached_GBV", 
                    "Reached_Gen_Dim")

# Subset the data frame (assuming your data frame is named 'df')
data <- data[, keep_variables, drop = FALSE]





data <- data %>%
  rowwise() %>%
  mutate(changes_total = mean(c_across(c("Changes_Awareness", "Changes_Balance", 
                                   "Changes_Leadership", "Changes_Recruitment", 
                                   "Changes_GBV", "Changes_Gen_Dim")), na.rm = TRUE)) %>%
  ungroup()


mean_changes_with_resources <- mean(data$changes_total[data$Int_financial_res == 1], na.rm = TRUE)
print(mean_changes_with_resources)

mean_changes_without_resources <- mean(data$changes_total[data$Int_financial_res == 2 | data$Int_financial_res == 3], na.rm = TRUE)
print(mean_changes_without_resources)


# Perform the t-test
t_test_result <- t.test(
  data$changes_total[data$Int_financial_res == 1], 
  data$changes_total[data$Int_financial_res == 2 | data$Int_financial_res == 3], 
  var.equal = TRUE, na.rm = TRUE
)

# Extract relevant statistics with readable formatting
mean_with_resources <- round(mean(data$changes_total[data$Int_financial_res == 1], na.rm = TRUE), 3)
mean_without_resources <- round(mean(data$changes_total[data$Int_financial_res == 2 | data$Int_financial_res == 3], na.rm = TRUE), 3)

total_with_resources <- sum(data$Int_financial_res == 1, na.rm = TRUE)  # Count for GEP=1
total_without_resources <- sum(data$Int_financial_res == 2 | data$Int_financial_res == 3, na.rm = TRUE)  # Count for GEP=0

t_statistic <- round(t_test_result$statistic, 2)
df <- round(t_test_result$parameter, 0)  # No decimals for degrees of freedom
mean_diff <- round(t_test_result$estimate[1] - t_test_result$estimate[2], 2)
conf_low <- round(t_test_result$conf.int[1], 3)
conf_high <- round(t_test_result$conf.int[2], 3)
p_value <- format(round(t_test_result$p.value, 4), scientific = FALSE)  # Avoids scientific notation

# Create a well-structured data frame WITHOUT row indexing
t_test_df <- data.frame(
  Metric = c("Mean (with financial resources)", "Mean (without financial resources)", "Total Observations (with financial resources)", 
             "Total Observations (without financial resources)", "T-Statistic", "Degrees of Freedom", 
             "Mean Difference", "Confidence Interval (Lower)", "Confidence Interval (Upper)", "P-Value"),
  Value = c(mean_with_resources, mean_without_resources, total_with_resources, total_without_resources, 
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

png("17_Mean_of_ratings_changes_with_and_without_financial_resources.png", width = 1200, height = 800, res = 150)
grid.draw(table_plot)
dev.off()

# Display the table in R
grid.draw(table_plot)






