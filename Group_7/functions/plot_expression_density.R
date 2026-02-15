library(tidyverse)

#----------------------------------------------------------
# Function: plot_expression_density()
# Purpose:  Plot density distributions of expression values per sample
# Input:    A tibble where:
#             - The first column is 'gene_id'
#             - Remaining columns are expression values per sample
# Output:   A ggplot2 density plot (log-scaled on x-axis)
#----------------------------------------------------------

plot_expression_density <- function(data) {
  data %>%
    pivot_longer(
      cols = -gene_id,
      names_to = "sample",
      values_to = "expression"
    ) %>%
    ggplot(aes(x = expression, fill = sample)) +
    geom_density(alpha = 0.3) +
    scale_x_log10() +
    theme_minimal() +
    labs(
      title = "Expression Distribution per Sample",
      x = "Expression (log10 scale)",
      y = "Density",
      fill = "Sample"
    )
}

#----------------------------------------------------------
# Example usage:
#----------------------------------------------------------
# density_plot <- plot_expression_density(datos)
# print(density_plot)
