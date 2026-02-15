augment_data <- function(deseq2_df, dexeq_df, alpha=0.05){
  dexseq_joined <- full_join(dexeq_df |> dplyr::select(groupID, pvalue),
                             deseq2_df |> dplyr::select(gene_id, pvalue),
                             by = c("groupID" = "gene_id")
  ) |> dplyr::rename(pvalue_dexseq = pvalue.x,
                     pvalue_deseq2 = pvalue.y) |> drop_na()
  
  
  augment <- dexseq_joined |>
    rowwise() |>
    mutate(
      joint_pval_fisher = metap::sumlog(c(pvalue_deseq2, pvalue_dexseq))$p
    ) |>
    ungroup() |>
    mutate( # This step is included here to check for unique genes
      padj_deseq2 = p.adjust(pvalue_deseq2, method = "BH"),
      padj_dexseq = p.adjust(pvalue_dexseq, method = "BH"),
      padj_joint = p.adjust(joint_pval_fisher, method = "BH"),
      sig_deseq2 = padj_deseq2 < alpha,
      sig_dexseq = padj_dexseq < alpha,
      sig_joint = padj_joint < alpha
    )
  
  return(augment)
}
