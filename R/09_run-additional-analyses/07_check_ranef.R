# fit rt model on Chinese data with different random effects structure ----

rt_freq_chinese_word_ranef <- mixed(
  log_rt ~ stroop * language * trial_type +
    (1 + stroop + language + trial_type +
       stroop:language + stroop:trial_type + trial_type:language || subject_id) + 
    (1 + trial_type | word),
  data = dat_rt_chinese,
  expand_re = TRUE,
  check_contrasts = FALSE,
  control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
)

write_rds(
  rt_freq_chinese_word_ranef, 
  here(
    "04_analysis", 
    "06_additional-analyses", 
    "mixed_freq_chinese_alt-ranef_rt.rds"
  )
)