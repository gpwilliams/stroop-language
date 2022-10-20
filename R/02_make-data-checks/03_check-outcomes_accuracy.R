## check RTs ----

accuracy_summary <- accuracy_dat |> 
  group_by(study) |> 
  summarise(
    n = length(unique(subject_id)),
    min = min(correct),
    lower_95 = quantile(correct, probs = .025),
    mean = mean(correct),
    median = median(correct),
    upper_95 = quantile(correct, probs = .0975),
    max = max(correct)
  ) |> 
  mutate(across(where(is.numeric), round, 3))

## plot mean correct ----

accuracy_plot <- accuracy_dat |> 
  group_by(study, subject_id) |> 
  mutate(study = str_to_title(str_replace(study, "_", " "))) |>
  summarise(mean_correct = mean(correct)) |> 
  ggplot(aes(x = mean_correct)) +
  geom_histogram(bins = 50, colour = "white") +
  facet_wrap(~study) +
  labs(x = "Mean proportion of correct responses", y = "Count")
