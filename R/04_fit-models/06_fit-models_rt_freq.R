# fit models ----

rt_models_freq <- list()

for(i in seq_along(dat_rt$study)) {
  if(dat_rt$study[i] == "arabic_stroop") {
    rt_models_freq[[i]] <- mixed(
      log_rt ~ stroop * language * trial_type +
        (1 + stroop + language + trial_type +
           stroop:language + trial_type:language || subject_id) + 
        (1 + trial_type | word_colour),
      data = dat_rt$data[[i]],
      expand_re = TRUE,
      check_contrasts = FALSE,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
    )
  } else if(dat_rt$study[i] == "chinese_stroop") {
    rt_models_freq[[i]] <- mixed(
      log_rt ~ stroop * language * trial_type +
        (1 + stroop + language + trial_type +
           stroop:language + stroop:trial_type + trial_type:language || subject_id) + 
        (1 + trial_type | word_colour),
      data = dat_rt$data[[i]],
      expand_re = TRUE,
      check_contrasts = FALSE,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
    )
  } else if(dat_rt$study[i] == "dutch_stroop") {
    rt_models_freq[[i]] <- mixed(
      log_rt ~ stroop * language * trial_type +
        (1 + stroop + language + trial_type +
           stroop:language + trial_type:language | subject_id) + 
        (1 + trial_type | word_colour),
      data = dat_rt$data[[i]],
      expand_re = TRUE,
      check_contrasts = FALSE,
      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))
    )
  }
}

names(rt_models_freq) <- paste(dat_rt$study, "rt", sep = "_")

# save them
save_list_to_file(
  rt_models_freq, 
  path = here("04_analysis", "03_models", "01_frequentist", "01_rt_mixed"), 
  csv = FALSE
)