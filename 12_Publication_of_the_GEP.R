rm(list = ls()) #clears the environment

#Set your working directory
setwd("specify_your_working_directory")

# Load packages
library(haven)
library(dplyr)
library(ggplot2)
library(tidyr)
library(grid) 



# Read the Stata file
data <- read_dta("survey.dta")

# Filter only rows where No_GEP_GEDP_DEIP == 1
data <- data %>%
  filter(No_GEP_GEDP_DEIP == 0)

data <- data[, "Published_GEP", drop = FALSE]  # Keeps data as a data frame

# View variable labels for all variables
sapply(data, function(x) attr(x, "label"))

# Convert all labelled variables to factors
data_factor <- as_factor(data)

# View the first few rows
head(data_factor)


# Drop rows where Published_GEP is "Not seen the question"
data_factor <- subset(data_factor, Published_GEP != "Not seen the question")
# Calculate percentages
data_summary <- as.data.frame(table(data_factor$Published_GEP))
colnames(data_summary) <- c("Published_GEP", "Count")
data_summary$Percentage <- (data_summary$Count / sum(data_summary$Count)) * 100
data_summary <- subset(data_summary, Percentage > 0)  # Remove zero percentage rows


# Create pie chart with increased spacing between legend items via label margins
plot <- ggplot(data_summary, aes(x = "", y = Percentage, fill = Published_GEP)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Distribution of Published GEP") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  theme(
    legend.text = element_text(lineheight = 0.8, size = 12),  # Tighten space within each label
    legend.spacing.y = unit(1, "cm"),                           # Increase space between legend items
    legend.key = element_blank(),                               # Remove rectangle from legend keys
    plot.background = element_rect(fill = "white", color = NA)    # Set background to white
  ) +
  scale_fill_manual(
    values = c(
      "The document is accessible to people outside the organisation" = "darkblue", 
      "Only internal document and accessible to all members of the organisation" = "darkred", 
      "Only internal document and accessible only to the management" = "lightblue"
    ),
    labels = c(
      "The document is accessible to people outside the organisation" = "Accessible to people \noutside the organisation",
      "Only internal document and accessible to all members of the organisation" = "Internal and accessible to all \nmembers of the organisation",
      "Only internal document and accessible only to the management" = "Internal and accessible \nonly to the management"
    )
  ) +
  guides(fill = guide_legend(
    title = "Published GEP", 
    keywidth = 0.8, 
    keyheight = 0.8,
    label.theme = element_text(margin = margin(t = 10, b = 10))  # Add extra margin above and below each label
  ))

print(plot)


# Save the plot as a PNG file
ggsave("12_Publication_of_the_GEP.png", plot = plot, width = 10, height = 8, dpi = 300)

# Confirm saved file
print("12_Publication_of_the_GEP.png")
