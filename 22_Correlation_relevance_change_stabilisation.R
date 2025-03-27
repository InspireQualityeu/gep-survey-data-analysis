rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")

# Load packages
library(haven)
library(dplyr)
library(ggplot2)
library(scales)
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(grid)

# Read the Stata file
data <- read_dta("survey.dta")

# View the data
head(data)

data <- data %>%
  filter(No_GEP_GEDP_DEIP != 1)


# Define the vector of columns you want to keep
keep_variables <- c( "Positive_Change", "Changes_Awareness", "Changes_Balance", 
                    "Changes_Leadership", "Changes_Recruitment", "Changes_GBV", 
                    "Changes_Gen_Dim", "Reached_Awareness", "Reached_Balance", 
                    "Reached_Leadership", "Reached_Recruitment", "Reached_GBV", 
                    "Reached_Gen_Dim")

# Subset the data frame (assuming your data frame is named 'df')
data <- data[, keep_variables, drop = FALSE]





# Compute row means for "changes"
data$changes <- rowMeans(data[, c("Changes_Awareness", "Changes_Balance", 
                                  "Changes_Leadership", "Changes_Recruitment", 
                                  "Changes_GBV", "Changes_Gen_Dim")], na.rm = TRUE)

# Compute row means for "reached"
data$reached <- rowMeans(data[, c("Reached_Awareness", "Reached_Balance", 
                                  "Reached_Leadership", "Reached_Recruitment", 
                                  "Reached_GBV", "Reached_Gen_Dim")], na.rm = TRUE)



# Convert necessary columns to numeric
data$Positive_Change <- as.numeric(as.character(data$Positive_Change))
data$changes        <- as.numeric(as.character(data$changes))
data$reached        <- as.numeric(as.character(data$reached))

# Compute Pearson correlation
cor_results <- rcorr(as.matrix(data[, c("Positive_Change", "changes", "reached")]), type = "pearson")

# Extract correlation coefficients, p-values, and N
cor_changes <- cor_results$r["Positive_Change", "changes"]
cor_reached <- cor_results$r["Positive_Change", "reached"]

p_changes   <- cor_results$P["Positive_Change", "changes"]
p_reached   <- cor_results$P["Positive_Change", "reached"]

n_changes   <- cor_results$n["Positive_Change", "changes"]
n_reached   <- cor_results$n["Positive_Change", "reached"]

# Format function (3 decimals, replace dot with comma)
format_num <- function(x) {
  formatted <- format(round(x, 3), nsmall = 3)
  gsub("\\.", ",", formatted)
}

# Format correlation coefficients and significance levels
corr_str_changes <- paste0(format_num(cor_changes), "**")
corr_str_reached <- paste0(format_num(cor_reached), "**")

sig_str_changes <- ifelse(p_changes < 0.001, "<0,001", format_num(p_changes))
sig_str_reached <- ifelse(p_reached < 0.001, "<0,001", format_num(p_reached))

# Create a data frame for the table
table_df <- data.frame(
  Statistic = c("Pearson correlation coefficient", "Significance (2-tailed)", "N"),
  `Mean of changes in all areas` = c(corr_str_changes, sig_str_changes, n_changes),
  `Mean of stabilisation in all areas` = c(corr_str_reached, sig_str_reached, n_reached),
  check.names = FALSE
)


# Convert table to a ggplot-friendly format
table_grob <- tableGrob(table_df, rows = NULL)



# Combine title and table
final_table <- grid.arrange( table_grob, ncol = 1)

# Save as PNG
ggsave("22_Relevance_of_the_GEP_on_the_achieved_positive_changes.png", plot = final_table, width = 8, height = 4, dpi = 300)

# Confirm saved files
print("22_Relevance_of_the_GEP_on_the_achieved_positive_changes_and_the_mean_of_changes_in_all_areas_and_the_mean_of_stabilisation_in_all_areas.png")