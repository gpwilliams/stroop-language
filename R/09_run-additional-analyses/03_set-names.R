# set names ----

dat_rt <- dat_rt |> 
  mutate(
    subject_id = as.factor(subject_id),
    word = as.factor(word),
    word_colour = as.factor(word_colour),
    study = str_replace(study, "_stroop", ""),
    study = factor(
      study,
      levels = c("dutch", "arabic", "chinese"),
      labels = c("Dutch", "Arabic", "Chinese")
    ),
    stroop = factor(
      stroop, 
      levels = c("Incongruent", "Neutral"),
    ),
    trial_type = factor(
      trial_type,
      levels = c("Switch", "Repetition"),
    ),
    language = case_when(
      language == "English" ~ "English",
      TRUE ~ "Other"
    ),
    language = factor(
      language,
      levels = c("Other", "English")
    )
  )

dat_rt_chinese <- dat_rt |> 
  filter(study == "Chinese") |> 
  mutate(
    subject_id = factor(subject_id),
    word = factor(word),
    word_colour = factor(word_colour)
  )


dat_accuracy <- dat_accuracy |> 
  mutate(
    subject_id = as.factor(subject_id),
    word = as.factor(word),
    word_colour = as.factor(word_colour),
    study = str_replace(study, "_stroop", ""),
    study = factor(
      study,
      levels = c("dutch", "arabic", "chinese"),
      labels = c("Dutch", "Arabic", "Chinese")
    ),
    stroop = factor(
      stroop, 
      levels = c("Incongruent", "Neutral"),
    ),
    trial_type = factor(
      trial_type,
      levels = c("Switch", "Repetition"),
    ),
    language = case_when(
      language == "English" ~ "English",
      TRUE ~ "Other"
    ),
    language = factor(
      language,
      levels = c("Other", "English")
    )
  )

