library(tidyverse)

#----------------------------------------------------------
# Function: summarize_expression()
# Purpose:  Compute summary statistics for each sample
# Input:    A tibble where the first column is 'gene_id' 
#           and the remaining columns are expression values
# Output:   A tibble with mean, median, sd, min, and max per sample
#----------------------------------------------------------

summarize_expression <- function(data) {
  data %>%
    rename(gene_id = 1) %>%
    pivot_longer(
      cols = -gene_id,
      names_to = "sample",
      values_to = "expression"
    ) %>%
    group_by(sample) %>%
    summarise(
      mean_expression   = mean(expression, na.rm = TRUE),
      median_expression = median(expression, na.rm = TRUE),
      sd_expression     = sd(expression, na.rm = TRUE),
      min_expression    = min(expression, na.rm = TRUE),
      max_expression    = max(expression, na.rm = TRUE),
      .groups = "drop"
    )
}

#----------------------------------------------------------
# Example usage:
#----------------------------------------------------------
# summary_stats <- summarize_expression(datos)
# print(summary_stats)
