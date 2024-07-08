# Load necessary libraries
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(stats)

perform_analysis <- function(filename) {
  df <- read_csv(filename)
  
  # Ensure the dataframe has 'x' and 'y' columns
  if (!("x" %in% colnames(df)) || !("y" %in% colnames(df))) {
    print("The CSV file must contain 'x' and 'y' columns.")
    return()
  }
  
  # Scatter plot without linear regression
  ggplot(df, aes(x = x, y = y)) +
    geom_point(color = "blue") +
    labs(x = "x", y = "y", title = "Scatter Plot without Linear Regression") +
    theme_minimal() +
    ggsave("scatter_plot_without_regression.png", width = 8, height = 6)
  
  # Scatter plot with linear regression
  model <- lm(y ~ x, data = df)
  r_sq <- summary(model)$r.squared
  print(paste("Coefficient of determination (R^2):", r_sq))
  
  ggplot(df, aes(x = x, y = y)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
    labs(x = "x", y = "y", title = "Scatter Plot with Linear Regression") +
    theme_minimal() +
    ggsave("scatter_plot_with_regression.png", width = 8, height = 6)
}

# Check if at least one command-line argument is provided
if (length(commandArgs(trailingOnly = TRUE)) < 1) {
  print("Give me a CSV file!")
} else {
  filename <- commandArgs(trailingOnly = TRUE)[1]
  
  # Check if the file is a CSV
  if (tools::file_ext(filename) == "csv") {
    perform_analysis(filename)
  } else {
    print("The file is not a CSV. Please provide a valid CSV file.")
  }
}
