# collect a record of removed participants
# dropped trials (percentages and N)
trial_numbers <- list(
    dat_accuracy = dat_accuracy, 
    dat_accuracy_filtered = dat_accuracy_filtered, 
    dat_rt = dat_rt, 
    dat_rt_filtered = dat_rt_filtered
  ) |> 
  map(~ .x |> group_by(study) |> summarise(n = n())) |> 
  bind_rows(.id = "dataset") |> 
  pivot_wider(
    names_from = "dataset",
    values_from = "n"
  ) |> 
  rename_with(~str_replace_all(., "dat", "n"), .cols = contains("dat")) |> 
  mutate(
    trials_removed_accuracy = n_accuracy - n_accuracy_filtered,
    trials_removed_rt = n_accuracy - n_rt_filtered,
    percent_removed_accuracy = (trials_removed_accuracy/n_accuracy)*100,
    percent_removed_rt = (trials_removed_rt/n_accuracy)*100
  ) |> 
  pivot_longer(
    names_to = "measure",
    values_to = "n",
    cols = !study
  )

participant_numbers <- list(
    dat_cleaned = dat_cleaned3,
    dat_accuracy = dat_accuracy, 
    dat_accuracy_filtered = dat_accuracy_filtered, 
    dat_rt = dat_rt, 
    dat_rt_filtered = dat_rt_filtered
  ) |> 
  map(~ .x |> group_by(study) |> summarise(n = length(unique(subject_id)))) |> 
  bind_rows(.id = "dataset") |> 
  pivot_wider(
    names_from = "dataset",
    values_from = "n"
  ) |> 
  mutate(original = c(50, 50, 48)) |> # original numbers
  select(study, original, everything())
