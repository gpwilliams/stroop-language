by_subj_rt <- dat_rt |> 
  group_by(study, subject_id, stroop, trial_type, language) |> 
  summarise(
    m = mean(rt), 
    sd = sd(rt),
    n = length(unique(word_colour))
  ) |> 
  mutate(study = str_replace_all(study, "_stroop", ""))

by_subj_rt$stroop <- factor(
  by_subj_rt$stroop,
  levels = c("Neutral", "Incongruent")
)

by_subj_rt$trial_type <- factor(
  by_subj_rt$trial_type,
  levels = c("Repetition", "Switch")
)

by_subj_rt <- by_subj_rt |>
  mutate(conditions = interaction(stroop, trial_type, sep = " × "))


# accuracy ----

by_subj_accuracy <- dat_accuracy |> 
  group_by(study, subject_id, stroop, trial_type, language) |> 
  summarise(
    m = mean(correct), 
    sd = sd(correct),
    n = length(unique(word_colour))
  ) |> 
  mutate(study = str_replace_all(study, "_stroop", ""))

by_subj_accuracy$stroop <- factor(
  by_subj_accuracy$stroop,
  levels = c("Neutral", "Incongruent")
)

by_subj_accuracy$trial_type <- factor(
  by_subj_accuracy$trial_type,
  levels = c("Repetition", "Switch")
)

by_subj_accuracy <- by_subj_accuracy |>
  mutate(conditions = interaction(stroop, trial_type, sep = " × "))
