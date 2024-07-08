library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(stats)
library(tools)

perform_analysis <- function(filename) {
  print(paste("Reading file:", filename))
  
  df <- read_csv(filename)
  print("CSV file loaded successfully.")
  
  # Ensure the dataframe has 'x' and 'y' columns
  if (!("x" %in% colnames(df)) || !("y" %in% colnames(df))) {
    print("The CSV file must contain 'x' and 'y' columns.")
    return()
  }
  
  # Scatter plot without linear regression
  p1 <- ggplot(df, aes(x = x, y = y)) +
    geom_point(color = "blue") +
    labs(x = "x", y = "y", title = "Scatter Plot without Linear Regression") +
    theme_minimal()
  print(p1)
  ggsave("scatter_plot_without_regression.png", plot = p1, width = 8, height = 6)
  
  # Scatter plot with linear regression
  model <- lm(y ~ x, data = df)
  r_sq <- summary(model)$r.squared
  print(paste("Coefficient of determination (R^2):", r_sq))
  
  p2 <- ggplot(df, aes(x = x, y = y)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +
    labs(x = "x", y = "y", title = "Scatter Plot with Linear Regression") +
    theme_minimal()
  print(p2)
  ggsave("scatter_plot_with_regression.png", plot = p2, width = 8, height = 6)
}

# Check if at least one command-line argument is provided
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  print("Give me a CSV file!")
} else {
  filename <- args[1]
  print(paste("Filename provided:", filename))
  
  # Check if the file is a CSV
  if (file_ext(filename) == "csv") {
    perform_analysis(filename)
  } else {
    print("The file is not a CSV. Please provide a valid CSV file.")
  }
}
