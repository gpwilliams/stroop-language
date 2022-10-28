# Mixed Effects ----

# RT ----

rt_mixed_freq_table_anova <- map_tidy(mixed_freq_rt, "ANOVA") |> 
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
  rt_mixed_freq_table_anova,
  here(
  "04_analysis", "04_model-predictions", "01_frequentist", "rt_mixed_freq_table_anova.csv"
))

# Accuracy ----

accuracy_mixed_freq_table_anova <- map_tidy(mixed_freq_accuracy, "ANOVA") |> 
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
  accuracy_mixed_freq_table_anova,
  here(
    "04_analysis","04_model-predictions", "01_frequentist", "accuracy_mixed_freq_table_anova.csv"
  ))

# ANOVA ----

## RT ----

rt_anova_freq_table <- anova_freq_rt |> 
  map_tidy() |> 
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
  rt_anova_freq_table,
  here(
    "04_analysis","04_model-predictions", "01_frequentist", "rt_anova_freq_table.csv"
  ))

## Accuracy ----

accuracy_anova_freq_table <- anova_freq_accuracy |> 
  map_tidy() |> 
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
  accuracy_anova_freq_table,
  here(
    "04_analysis","04_model-predictions", "01_frequentist", "accuracy_anova_freq_table.csv"
  ))
