library(tidyverse)

#----------------------------------------------------------
# Function: plot_expression_boxplot()
# Purpose:  Create a boxplot of expression values per sample
# Input:    A tibble where:
#             - The first column is the gene identifier
#             - Remaining columns are numeric expression values
# Output:   A ggplot2 boxplot (log-scaled on y-axis)
#----------------------------------------------------------

plot_expression_boxplot <- function(data) {
  # Automatically detect the first column name (gene IDs)
  first_col <- names(data)[1]
  
  data %>%
    pivot_longer(
      cols = -gene_id,
      names_to = "sample",
      values_to = "expression"
    ) %>%
    ggplot(aes(x = sample, y = expression)) +
    geom_boxplot(outlier.size = 0.5) +
    scale_y_log10() +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = "Expression Distribution per Sample (log10 scale)",
      x = "Sample",
      y = "Expression (log10 scale)"
    )
}

#----------------------------------------------------------
# Example usage:
#----------------------------------------------------------
# boxplot_fig <- plot_expression_boxplot(norm0)
# print(boxplot_fig)
