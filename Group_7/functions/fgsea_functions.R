create_stats <- function(df, pvalue_col, id_col = "groupID") {
  s <- -log10(df[[pvalue_col]])
  names(s) <- df[[id_col]]
  # In case of duplicate gene IDs, keep the strongest evidence
  s <- tapply(s, names(s), max)
  sort(s, decreasing = TRUE)
}

run_fgsea <- function(stats, pathways, minSize = 15, maxSize = 5000) {
  fgseaMultilevel(pathways = pathways, stats = stats,
                  scoreType = "pos", minSize = minSize, maxSize = maxSize) |>
    arrange(padj)
}

sig_paths <- function(fgsea_res, alpha = 0.05) {
  fgsea_res %>% filter(padj < alpha) %>% pull(pathway) %>% unique()
}
