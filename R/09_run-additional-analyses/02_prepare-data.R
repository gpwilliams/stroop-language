# make aggregated data for ANOVAs ----

agg_rt <- dat_rt |> 
  select(study, subject_id, trial, language, stroop, trial_type, correct, rt) |> 
  ungroup() |> 
  group_by(study, subject_id, language, stroop, trial_type) |>  
  summarise(mean_rt = mean(rt, na.rm = TRUE)) |> 
  pivot_wider(names_from = trial_type, values_from = mean_rt) |> 
  mutate(switch_cost_rt = Switch - Repetition)

agg_accuracy <- dat_accuracy |> 
  select(study, subject_id, trial, language, stroop, trial_type, correct, rt) |> 
  ungroup() |> 
  group_by(study, subject_id, language, stroop, trial_type) |>  
  summarise(mean_correct = mean(correct, na.rm = TRUE)) |> 
  pivot_wider(names_from = trial_type, values_from = mean_correct) |> 
  mutate(switch_cost_correct = Switch - Repetition)

