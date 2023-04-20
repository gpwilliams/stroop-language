# fit models ----

# fit rt model on all studies
rt_model_freq_all <- mixed(
  log_rt ~ stroop * language * trial_type +
    (1 + stroop + language + trial_type + stroop:language | subject_id) + 
    (1 + trial_type | word_colour) +
    (1 + language | study),
  data = dat_rt,
  check_contrasts = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)

write_rds(
  rt_model_freq_all, 
  file = here(
    "04_analysis", 
    "06_additional-analyses", 
    "01_models",
    "mixed_freq_all-studies_rt.rds"
    )
)

# fit accuracy model on all studies
accuracy_model_freq_all <- mixed(
  correct ~ stroop * language * trial_type +
    (1 + stroop | subject_id) + 
    (1 | word_colour) +
    (1 | study),
  data = dat_accuracy,
  family = binomial(link = "logit"),
  expand_re = TRUE,
  check_contrasts = FALSE,
  control = glmerControl(
    optimizer = "bobyqa", 
    optCtrl = list(maxfun = 1e5)
  ),
  method = "LRT"
)

write_rds(
  accuracy_model_freq_all, 
  here(
    "04_analysis", 
    "06_additional-analyses", 
    "01_models",
    "mixed_freq_all-studies_accuracy.rds"
  )
)
