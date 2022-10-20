make_prior_titles <- function(x) {
  x |> 
    bind_rows(.id = "model") |>
    filter(class == "b") |> 
    mutate(
      interaction = case_when(
        str_detect(coef, ":") ~ TRUE,
        TRUE ~ FALSE
      )
    ) |> 
    group_by(model, interaction) |> 
    summarise(prior = unique(prior), .groups = "drop") |>
    mutate(prior = str_to_title(prior)) |> 
    pivot_wider(
      names_from = interaction,
      values_from = prior,
      names_glue = "interaction_{tolower(interaction)}"
    ) |> 
    mutate(
      title = glue::glue(
        "Main effect prior of {interaction_false} and interaction prior of {interaction_true}."
      )
    )
}