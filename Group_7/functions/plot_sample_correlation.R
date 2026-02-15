library(tidyverse)
library(reshape2)

#----------------------------------------------------------
# Function: plot_sample_correlation()
# Purpose:  Generate a heatmap showing correlations between samples
# Input:    A tibble where:
#             - The first column is the gene identifier
#             - Remaining columns are numeric expression values
# Output:   A ggplot2 heatmap representing sample-to-sample correlations
#----------------------------------------------------------

plot_sample_correlation <- function(data) {
  # Automatically detect the first column (gene IDs)
  first_col <- names(data)[1]
  
  # Convert to matrix with genes as rows and samples as columns
  expr_matrix <- data %>%
    column_to_rownames(var = first_col) %>%
    as.matrix()
  
  # Compute correlation matrix between samples
  cor_mat <- cor(expr_matrix, use = "pairwise.complete.obs")
  
  # Convert correlation matrix to long format for ggplot
  melted_cor <- melt(cor_mat)
  
  # Create correlation heatmap
  ggplot(melted_cor, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(name = "Correlation") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    labs(title = "Sample-to-Sample Correlation Heatmap")
}

#----------------------------------------------------------
# Example usage:
#----------------------------------------------------------
# correlation_plot <- plot_sample_correlation(norm0)
# print(correlation_plot)
