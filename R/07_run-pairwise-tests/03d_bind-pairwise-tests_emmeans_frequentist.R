bound_emm_pairs <- list(
  rt_pairs = bind_emmeans(rt_pairs),
  rt_pairs_resp = bind_emmeans(rt_pairs_resp),
  accuracy_pairs = bind_emmeans(accuracy_pairs),
  accuracy_pairs_resp = bind_emmeans(accuracy_pairs_resp)
)

save_list_to_file(
  bound_emm_pairs,
  here(
    "04_analysis", 
    "05_pairwise-tests"
  ),
  rds = FALSE
)