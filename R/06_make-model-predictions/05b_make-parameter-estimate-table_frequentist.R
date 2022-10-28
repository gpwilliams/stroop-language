# Multilevel ----

## RT ----

rt_mixed_freq_table_est <- map_tidy(mixed_freq_rt, "full") |> 
  rename(Parameter = term) |> 
  mutate(study = str_replace(study, "_stroop", "")) |> 
  separate(
    study,
    into = c("Study", "Measure"),
    "_"
  ) |> 
  mutate(
    Study = str_to_title(Study),
    Measure = str_to_upper(Measure),
    Parameter = str_to_title(
      str_replace_all(
        Parameter,
        c(":" = " × ",
          "1" = "",
          "_" = " "
        )
      )
    )
  )

write_csv(
  rt_mixed_freq_table_est,
  here(
    "04_analysis","04_model-predictions", "01_frequentist", "rt_mixed_freq_table_est.csv"
  ))

## Accuracy ----

accuracy_mixed_freq_table_est <- map_tidy(mixed_freq_accuracy, "full") |> 
  rename(Parameter = term) |> 
  mutate(study = str_replace(study, "_stroop", "")) |> 
  separate(
    study,
    into = c("Study", "Measure"),
    "_"
  ) |> 
  mutate(
    Study = str_to_title(Study),
    Measure = str_to_title(Measure),
    Parameter = str_to_title(
      str_replace_all(
        Parameter,
        c(":" = " × ",
          "1" = "",
          "_" = " "
        )
      )
    )
  )

write_csv(
  accuracy_mixed_freq_table_est,
  here(
    "04_analysis","04_model-predictions", "01_frequentist", "accuracy_mixed_freq_table_est.csv"
  ))
