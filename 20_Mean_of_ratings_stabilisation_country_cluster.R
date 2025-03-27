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

# Step 1: Create a new variable for the region
data <- data %>%
  mutate(Country_Cluster = case_when(
    Country %in% c(7, 12, 15, 20, 23, 31) ~ "North West",
    Country %in% c(1, 2, 8, 13, 19, 4, 28) ~ "Central West",
    Country %in% c(5, 10, 16, 22, 11, 18) ~ "Southern",
    Country %in% c(3, 6, 9, 14, 17, 24, 21, 29, 33, 30, 27, 32, 44) ~ "Central East and Eastern",
    TRUE ~ ""  # Default to an empty string if no match
  ))

# Convert Country to its label (as a string)
if (!is.null(attr(data$Country, "labels"))) {
  labels <- attr(data$Country, "labels")  # Extract numeric values and labels
  label_names <- names(labels)  # Country names
  label_values <- labels  # Country numeric codes
  
  # Convert Country to its corresponding label (country name)
  data$Country <- factor(data$Country, levels = label_values, labels = label_names) %>%
    as.character()
} else {
  data$Country <- as.character(data$Country)  # Fallback if no labels exist
}

# Ensure Country_Cluster is also character
data$Country_Cluster <- as.character(data$Country_Cluster)

# Display unique values of Country to verify conversion
cat("Converted Country variable:\n")
print(unique(data$Country))

# View the first few rows
head(data)


# Define the vector of columns you want to keep
keep_variables <- c("Country_Cluster", "Country","Changes_Awareness", "Changes_Balance", 
                    "Changes_Leadership", "Changes_Recruitment", "Changes_GBV", 
                    "Changes_Gen_Dim", "Reached_Awareness", "Reached_Balance", 
                    "Reached_Leadership", "Reached_Recruitment", "Reached_GBV", 
                    "Reached_Gen_Dim")

# Subset the data frame (assuming your data frame is named 'df')
data <- data[, keep_variables, drop = FALSE]

# Convert all other variables to numeric
numeric_vars <- setdiff(names(data), c("Country", "Country_Cluster"))
data[numeric_vars] <- lapply(data[numeric_vars], as.numeric)

#data$Country_Cluster[data$Country == "Cyprus"] <- "Central East and Eastern"


data <- data %>%
  rowwise() %>%
  mutate(stabilisation_total = mean(c_across(c("Reached_Awareness", "Reached_Balance", 
                                         "Reached_Leadership", "Reached_Recruitment", "Reached_GBV", 
                                         "Reached_Gen_Dim")), na.rm = TRUE)) %>%
  ungroup()


mean_stabilisation_North_West <- mean(data$stabilisation_total[data$Country_Cluster == "North West"], na.rm = TRUE)
print(mean_stabilisation_North_West)

mean_stabilisation_Central_West <- mean(data$stabilisation_total[data$Country_Cluster == "Central West"], na.rm = TRUE)
print(mean_stabilisation_Central_West)

mean_stabilisation_Southern <- mean(data$stabilisation_total[data$Country_Cluster == "Southern"], na.rm = TRUE)
print(mean_stabilisation_Southern)

mean_stabilisation_Central_East_Eastern <- mean(data$stabilisation_total[data$Country_Cluster == "Central East and Eastern"], na.rm = TRUE)
print(mean_stabilisation_Central_East_Eastern)


anova_result <- aov(stabilisation_total ~ Country_Cluster, data = data)
summary(anova_result)


# Perform ANOVA
anova_result <- aov(stabilisation_total ~ Country_Cluster, data = data)

# Extract means for each group
mean_north_west <- round(mean(data$stabilisation_total[data$Country_Cluster == "North West"], na.rm = TRUE), 3)
mean_central_west <- round(mean(data$stabilisation_total[data$Country_Cluster == "Central West"], na.rm = TRUE), 3)
mean_southern <- round(mean(data$stabilisation_total[data$Country_Cluster == "Southern"], na.rm = TRUE), 3)
mean_central_east <- round(mean(data$stabilisation_total[data$Country_Cluster == "Central East and Eastern"], na.rm = TRUE), 3)
mean_total <- round(mean(data$stabilisation_total, na.rm = TRUE), 3)  # Total Mean

# Count number of observations in each group
total_north_west <- sum(data$Country_Cluster == "North West", na.rm = TRUE)
total_central_west <- sum(data$Country_Cluster == "Central West", na.rm = TRUE)
total_southern <- sum(data$Country_Cluster == "Southern", na.rm = TRUE)
total_central_east <- sum(data$Country_Cluster == "Central East and Eastern", na.rm = TRUE)
total_overall <- sum(!is.na(data$stabilisation_total))  # Total Count

# Standard deviation calculations
stddev_north_west <- round(sd(data$stabilisation_total[data$Country_Cluster == "North West"], na.rm = TRUE), 3)
stddev_central_west <- round(sd(data$stabilisation_total[data$Country_Cluster == "Central West"], na.rm = TRUE), 3)
stddev_southern <- round(sd(data$stabilisation_total[data$Country_Cluster == "Southern"], na.rm = TRUE), 3)
stddev_central_east <- round(sd(data$stabilisation_total[data$Country_Cluster == "Central East and Eastern"], na.rm = TRUE), 3)
stddev_total <- round(sd(data$stabilisation_total, na.rm = TRUE), 3)  # Total Std Dev

# Extract ANOVA summary values
anova_summary <- summary(anova_result)
f_statistic <- round(anova_summary[[1]][["F value"]][1], 2)  # Extract F-statistic
df_between <- anova_summary[[1]][["Df"]][1]  # Degrees of freedom for groups
df_within <- anova_summary[[1]][["Df"]][2]   # Degrees of freedom for residuals
p_value <- format(round(anova_summary[[1]][["Pr(>F)"]][1], 4), scientific = FALSE)  # Avoid scientific notation

# Create a well-structured data frame for the ANOVA results
anova_df <- data.frame(
  Metric = c("Mean (North West)", "Mean (Central West)", "Mean (Southern)", "Mean (Central East and Eastern)", "Mean (Total)",
             "Total Observations (North West)", "Total Observations (Central West)", "Total Observations (Southern)", 
             "Total Observations (Central East and Eastern)", "Total Observations (Overall)",
             "Std Dev (North West)", "Std Dev (Central West)", "Std Dev (Southern)", "Std Dev (Central East and Eastern)", "Std Dev (Total)",
             "F-Statistic", "Degrees of Freedom (Between Groups)", "Degrees of Freedom (Within Groups)", "P-Value"),
  Value = c(mean_north_west, mean_central_west, mean_southern, mean_central_east, mean_total,
            total_north_west, total_central_west, total_southern, total_central_east, total_overall,
            stddev_north_west, stddev_central_west, stddev_southern, stddev_central_east, stddev_total,
            f_statistic, df_between, df_within, p_value),
  stringsAsFactors = FALSE
)

# Remove row names completely
rownames(anova_df) <- NULL


# Display as a formatted table in R

table_plot <- tableGrob(anova_df, rows = NULL, theme = ttheme_default(
  core = list(fg_params = list(fontsize = 12)), 
  colhead = list(fg_params = list(fontsize = 14, fontface = "bold"))
))

# Save the table as a PNG image
png("20_Mean_of_ratings_stabilisation_country_cluster.png", width = 1200, height = 1000, res = 150)
grid.draw(table_plot)
dev.off()

# Display the table in R
grid.draw(table_plot)
