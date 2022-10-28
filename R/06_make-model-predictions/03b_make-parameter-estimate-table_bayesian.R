# plot fixed effects and bayes factors for all bayesian models

bayesian_fixef_table_rt <- bayesian_rt_fixef |> 
  mutate(Model = str_replace(study, "_model_prior", "")) |> 
  separate(
    Model,
    into = c("Study", "Measure", "Prior_Model"),
    "_"
  ) |> 
  rename(Parameter = parameter) |> 
  select(Study, Measure, Parameter, Prior_Model, Estimate:`Q97.5`) |> 
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
  left_join(
    reduced_rt_prior_summary |> 
      mutate(model = str_replace(model, "prior_", "")) |> 
      rename(Prior_Model = model, Priors = priors) |> 
      select(Prior_Model, Priors),
    by = "Prior_Model"
  )

write_csv(
  bayesian_fixef_table_rt,
  here("04_analysis", "04_model-predictions", "02_bayesian", "rt_fixef.csv")
)

bayesian_fixef_table_accuracy <- bayesian_accuracy_fixef |> 
  mutate(Model = str_replace(study, "_model_prior", ""),) |> 
  separate(
    Model,
    into = c("Study", "Measure", "Prior_Model"),
    "_"
  ) |> 
  rename(Parameter = parameter) |> 
  select(Study, Measure, Parameter, Prior_Model, Estimate:`Q97.5`) |> 
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
  left_join(
    reduced_rt_prior_summary |> 
      mutate(model = str_replace(model, "prior_", "")) |> 
      rename(Prior_Model = model, Priors = priors) |> 
      select(Prior_Model, Priors),
    by = "Prior_Model"
  )

write_csv(
  bayesian_fixef_table_accuracy,
  here("04_analysis", "04_model-predictions", "02_bayesian", "accuracy_fixef.csv")
)
