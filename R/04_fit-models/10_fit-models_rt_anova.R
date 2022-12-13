# fit models ----

agg_models_rt <- map(
  .x = agg_rt$data,
  ~afex::aov_4(
    switch_cost_rt ~ stroop*language + (1 + stroop * language | subject_id),
    data = .
  )
)

names(agg_models_rt) <- paste(agg_rt$study, "rt", sep = "_")

save_list_to_file(
  agg_models_rt, 
  path = here("04_analysis", "03_models", "01_frequentist", "03_rt_anova"), 
  csv = FALSE
)
