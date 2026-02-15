gene_analysis <- function(augmented_data){
  sig_counts <- c(
    DESeq2 = sum(augmented_data$sig_deseq2),
    DEXSeq = sum(augmented_data$sig_dexseq),
    Joint = sum(augmented_data$sig_joint)
  )
  return(sig_counts)
}
