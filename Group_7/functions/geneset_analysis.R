geneset_analysis <- function(augmented_data, BP_list, MF_list){
  stats_deseq2 <- create_stats(augmented_data, "pvalue_deseq2")
  stats_dexseq <- create_stats(augmented_data, "pvalue_dexseq")
  stats_joint <- create_stats(augmented_data, "joint_pval_fisher")

  fgsea_deseq2 <- run_fgsea(stats_deseq2, c(BP_list, MF_list))
  fgsea_dexseq <- run_fgsea(stats_dexseq, c(BP_list, MF_list))
  fgsea_joint <- run_fgsea(stats_joint, c(BP_list, MF_list))

  sig_sets_deseq2 <- sig_paths(fgsea_deseq2)
  sig_sets_dexseq <- sig_paths(fgsea_dexseq)
  sig_sets_joint <- sig_paths(fgsea_joint)

  counts <- c(DESeq2 = length(sig_sets_deseq2),
              DEXSeq = length(sig_sets_dexseq),
              Joint = length(sig_sets_joint))

  unique_joint <- setdiff(sig_sets_joint,
                          union(sig_sets_deseq2,
                                sig_sets_dexseq)
  )
  return(list(
    unique_joint = unique_joint,
    fgsea_joint = fgsea_joint,
    fgsea_deseq2 = fgsea_deseq2,
    fgsea_dexseq = fgsea_dexseq
  ))

}
