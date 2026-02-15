edgington <- function(df1, df2, alpha = 0.05) {

  # Join by gene ID, keeping joint_pval_fisher from each dataset
  study_joined <- full_join(
    df1 %>% dplyr::select(groupID, pval_fisher_1 = joint_pval_fisher),
    df2 %>% dplyr::select(groupID, pval_fisher_2 = joint_pval_fisher),
    by = "groupID"
  ) %>%
    drop_na()   # keep genes present in both datasets

  # Edgington: sum of p-values, converted to meta-analytic p-value
  augment <- study_joined %>%
    rowwise() %>%
    mutate(
      joint_pval_edgington = metap::sump(
        c(pval_fisher_1, pval_fisher_2)
      )$p
    ) %>%
    ungroup() %>%

    # FDR correction AFTER integration
    mutate(
      padj_edgington = p.adjust(joint_pval_edgington, method = "BH"),
      sig_edgington  = padj_edgington < alpha
    )

  return(augment)
}
