bound_emm_pairs <- list(
  # mixed effects
  # rt
  rt_pairs = bind_emmeans(rt_pairs),
  rt_pairs_resp = bind_emmeans(rt_pairs_resp),
  rt_pairs_diffs = bind_emmeans(rt_pairs_diffs),
  rt_pairs_diffs_resp = bind_emmeans(rt_pairs_diffs_resp),
  rt_pairs_diffs_asym = bind_emmeans(rt_pairs_diffs_asym),
  rt_pairs_diffs_asym_resp = bind_emmeans(rt_pairs_diffs_asym_resp),
  # accuracy
  accuracy_pairs = bind_emmeans(accuracy_pairs),
  accuracy_pairs_resp = bind_emmeans(accuracy_pairs_resp),
  accuracy_pairs_diffs = bind_emmeans(accuracy_pairs_diffs),
  accuracy_pairs_diffs_resp = bind_emmeans(accuracy_pairs_diffs_resp),
  accuracy_pairs_diffs_asym = bind_emmeans(accuracy_pairs_diffs_asym),
  accuracy_pairs_diffs_asym_resp = bind_emmeans(accuracy_pairs_diffs_asym_resp),
  # anova
  # rt
  anova_rt_pairs = bind_emmeans(anova_rt_pairs, .conditions = c("stroop", "language")),
  anova_rt_pairs_diffs = bind_emmeans(anova_rt_pairs_diffs, .conditions = c("stroop", "language")),
  # accuracy
  anova_accuracy_pairs = bind_emmeans(anova_accuracy_pairs, .conditions = c("stroop", "language")),
  anova_accuracy_pairs_diffs = bind_emmeans(anova_accuracy_pairs_diffs, .conditions = c("stroop", "language"))
)

save_list_to_file(
  bound_emm_pairs,
  here(
    "04_analysis", 
    "05_pairwise-tests"
  ),
  rds = FALSE
)

## NEED TO FIX THE FACT STROOP IS AN ADDITIONAL COL IN THE DIFFS ONES