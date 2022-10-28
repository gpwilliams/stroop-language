# get prior titles ----

# get prior labels for each model; repeat x 3 (1 for each study)
rt_prior_titles <- map_dfr(seq_len(3), ~make_prior_titles(rt_priors))
accuracy_prior_titles <- map_dfr(seq_len(3), ~make_prior_titles(accuracy_priors))

# get reduced names for plots
reduced_rt_prior_summary <- rt_prior_titles |> 
  distinct() |> 
  mutate(
    prior_model = str_replace(model, "prior_", "Model "),
    priors = glue::glue("Main effect ~ {interaction_false}, Interaction ~ {interaction_true}"),
    plot_title = glue::glue("{prior_model}: Main effect ~ {interaction_false}, Interaction ~ {interaction_true}")
  )

reduced_accuracy_prior_summary <- accuracy_prior_titles |> 
  distinct() |> 
  mutate(
    prior_model = str_replace(model, "prior_", "Model "),
    priors = glue::glue("Main effect ~ {interaction_false}, Interaction ~ {interaction_true}"),
    plot_title = glue::glue("{prior_model}: Main effect ~ {interaction_false}, Interaction ~ {interaction_true}")
  )