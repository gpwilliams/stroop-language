bound_emmeans <- list(
  rt_emms = bind_emmeans(rt_emms),
  rt_emms_resp = bind_emmeans(
    rt_emms_resp |> 
      map( ~ .x |> as_tibble() |> rename(emmean = response))
  ),
  accuracy_emms = bind_emmeans(accuracy_emms),
  accuracy_emms_resp = bind_emmeans(accuracy_emms_resp),
  anova_rt_emms = bind_emmeans(anova_rt_emms, .conditions = c("stroop", "language")),
  anova_accuracy_emms = bind_emmeans(anova_accuracy_emms, .conditions = c("stroop", "language"))
)

save_list_to_file(
  bound_emmeans,
  here(
    "04_analysis", 
    "05_pairwise-tests"
  ),
  rds = FALSE
)
