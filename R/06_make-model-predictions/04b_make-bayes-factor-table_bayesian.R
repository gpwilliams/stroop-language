# make bayes factor table ----

bf_table_rt <- bfs_rt |> 
  bind_rows(.id = "Model") |> 
  as_tibble() |> 
  mutate(
    Parameter = str_replace(Parameter, "b_", ""),
    Model = str_replace(Model, "_model_prior", ""),
  ) |> 
  separate(
    Model,
    into = c("Study", "Measure", "Prior_Model"),
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
  ) |> 
  mutate(
    two_log_BF = 2*log_BF,
    BF = exp(log_BF),
    model = glue::glue("prior_{Prior_Model}")
  ) |> 
  left_join(
    reduced_rt_prior_summary |> 
      select(model, priors),
    by = "model"
  ) |> 
  select(-c(model, Component))

write_csv(
  bf_table_rt,
  here("04_analysis", "04_model-predictions", "02_bayesian", "rt_bayes-factors.csv")
)

# accuracy ----

bf_table_accuracy <- bfs_accuracy |> 
  bind_rows(.id = "Model") |> 
  as_tibble() |> 
  mutate(
    Parameter = str_replace(Parameter, "b_", ""),
    Model = str_replace(Model, "_model_prior", ""),
  ) |> 
  separate(
    Model,
    into = c("Study", "Measure", "Prior_Model"),
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
  ) |> 
  mutate(
    two_log_BF = 2*log_BF,
    BF = exp(log_BF),
    model = glue::glue("prior_{Prior_Model}")
  ) |> 
  left_join(
    reduced_accuracy_prior_summary |> 
      select(model, priors),
    by = "model"
  ) |> 
  select(-c(model, Component))

write_csv(
  bf_table_accuracy,
  here("04_analysis", "04_model-predictions", "02_bayesian", "accuracy_bayes-factors.csv")
)

