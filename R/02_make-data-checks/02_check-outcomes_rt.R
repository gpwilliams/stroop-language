## check RTs ----

rt_summary <- rt_dat |> 
  group_by(study) |> 
  summarise(
    n = length(unique(subject_id)),
    min = min(rt),
    lower_95 = quantile(rt, probs = .025),
    mean = mean(rt),
    median = median(rt),
    upper_95 = quantile(rt, probs = .0975),
    max = max(rt)
  ) |> 
  mutate(across(where(is.numeric), round, 3))

## plot RTs ----

rt_plot <- rt_dat |> 
  mutate(study = str_to_title(str_replace(study, "_", " "))) |> 
  ggplot(aes(x = rt)) +
  geom_histogram(bins = 50, colour = "white") +
  facet_wrap(~study) +
  labs(x = "Reaction Time [ms]", y = "Count")

## plot trial numbers per participant ----

rt_trial_count_plot <- rt_dat |> 
  group_by(study, subject_id) |> 
  count() |> 
  mutate(study = str_to_title(str_replace(study, "_", " "))) |> 
  ggplot(aes(x = n)) +
  geom_histogram(bins = 50, colour = "white") +
  facet_wrap(~study) +
  labs(x = "Number of Trials", y = "Count")
