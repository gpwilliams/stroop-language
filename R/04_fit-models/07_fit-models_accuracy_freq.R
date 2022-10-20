# fit models ----

# issue with singularity for first data set
accuracy_models_freq <- list()

for(i in seq_along(dat_accuracy$study)) {
  if(dat_accuracy$study[i] == "arabic_stroop") {
    accuracy_models_freq[[i]] <- mixed(
      correct ~ stroop * trial_type * language +
        (1 | subject_id),
      data = dat_accuracy$data[[i]],
      family = binomial(link = "logit"),
      expand_re = TRUE,
      check_contrasts = FALSE,
      control = glmerControl(
        optimizer = "bobyqa", 
        optCtrl = list(maxfun = 1e5)
      ),
      method = "LRT"
    )
  } else if(dat_accuracy$study[i] == "chinese_stroop") {
    accuracy_models_freq[[i]] <- mixed(
      correct ~ stroop * trial_type * language +
        (1 + stroop + language | subject_id) + 
        (1 + trial_type | word_colour),
      data = dat_accuracy$data[[i]],
      family = binomial(link = "logit"),
      expand_re = TRUE,
      check_contrasts = FALSE,
      control = glmerControl(
        optimizer = "bobyqa", 
        optCtrl = list(maxfun = 1e5)
      ),
      method = "LRT"
    )
  } else if(dat_accuracy$study[i] == "dutch_stroop") {
    accuracy_models_freq[[i]] <- mixed(
      correct ~ stroop * trial_type * language +
        (1 + stroop + trial_type + language || subject_id) + 
        (1 | word_colour),
      data = dat_accuracy$data[[i]],
      family = binomial(link = "logit"),
      expand_re = TRUE,
      check_contrasts = FALSE,
      control = glmerControl(
        optimizer = "bobyqa", 
        optCtrl = list(maxfun = 1e5)
      ),
      method = "LRT"
    )
  }
}

names(accuracy_models_freq) <- paste(dat_accuracy$study, "accuracy", sep = "_")

# save them
save_list_to_file(
  accuracy_models_freq, 
  path = here("04_analysis", "03_models", "01_frequentist", "02_accuracy_mixed"), 
  csv = FALSE
)
