bound_emmeans <- list(
  rt_emms = bind_emmeans(rt_emms),
  rt_emms_resp = bind_emmeans(rt_emms_resp),
  accuracy_emms = bind_emmeans(accuracy_emms),
  accuracy_emms_resp = bind_emmeans(accuracy_emms_resp)
)

save_list_to_file(
  bound_emmeans,
  here(
    "04_analysis", 
    "05_pairwise-tests"
  ),
  rds = FALSE
)
