# fit models ----

agg_models_accuracy <- map(
  .x = agg_accuracy$data,
  ~afex::aov_4(
    switch_cost_correct ~ stroop*language + (1 + stroop * language | subject_id),
    data = .
  )
)

names(agg_models_accuracy) <- paste(agg_accuracy$study, "accuracy", sep = "_")

save_list_to_file(
  agg_models_accuracy, 
  path = here("04_analysis", "03_models", "01_frequentist", "04_accuracy_anova"), 
  csv = FALSE
)
