# Mixed effects models ----

emms <- list()
pairwise <- list()

## RT ----

### log scale ----

emms$rt_stroop_emm <- emmeans(rt_model_freq_all$full_model, ~ stroop) |>
  as_tibble() |> 
  bind_rows()
emms$rt_trial_type_emm <- emmeans(rt_model_freq_all$full_model, ~ trial_type) |>
  as_tibble() |> 
  bind_rows()

### response scale ----

rt_model_freq_all_updated <- update(
  ref_grid(rt_model_freq_all$full_model), 
  tran = "log"
)

emms$rt_stroop_emm_resp <- emmeans(
  rt_model_freq_all_updated, ~ stroop, 
  type = "response"
) |>
  as_tibble() |> 
  bind_rows()

emms$rt_trial_type_emm_resp <- emmeans(
  rt_model_freq_all_updated, ~ trial_type, 
  type = "response"
) |>
  as_tibble() |> 
  bind_rows()

## Accuracy ----

### log scale ----

emms$accuracy_two_way_emm <- emmeans(
  accuracy_model_freq_all$full_model, 
  ~trial_type | stroop
)

pairwise$accuracy_two_way_pairs <- emms$accuracy_two_way_emm |> 
  pairs()|> 
  pairs(by = NULL) |> 
  as_tibble()

emms$accuracy_two_way_emm <- emms$accuracy_two_way_emm |> 
  as_tibble() |> 
  bind_rows()

emms$accuracy_three_way_emm <- emmeans(
  accuracy_model_freq_all$full_model, 
  ~trial_type | stroop * language
)

pairwise$accuracy_three_way_pairs <- emms$accuracy_three_way_emm |> 
  pairs() |> 
  pairs(by = "language") |> 
  rbind(adjust = "holm") |> 
  as_tibble()

emms$accuracy_three_way_emm <- emms$accuracy_three_way_emm |> 
  as_tibble() |> 
  bind_rows()

### response scale ----

emms$accuracy_two_way_emm_resp <- emmeans(
  accuracy_model_freq_all$full_model, 
  ~trial_type | stroop,
  type = "response"
)

pairwise$accuracy_two_way_pairs_resp <- emms$accuracy_two_way_emm_resp |> 
  pairs()|> 
  pairs(by = NULL) |> 
  as_tibble()

emms$accuracy_two_way_emm_resp <- emms$accuracy_two_way_emm_resp |> 
  as_tibble() |> 
  bind_rows()

emms$accuracy_three_way_emm_resp <- emmeans(
  accuracy_model_freq_all$full_model, 
  ~trial_type | stroop * language, 
  type = "response"
)

pairwise$accuracy_three_way_pairs_resp <- emms$accuracy_three_way_emm_resp |> 
  pairs() |> 
  pairs(by = "language") |> 
  rbind(adjust = "holm") |> 
  as_tibble()

emms$accuracy_three_way_emm_resp <- emms$accuracy_three_way_emm_resp |> 
  as_tibble() |> 
  bind_rows()

# save it ----

save_list_to_file(
  emms,
  here(
    "04_analysis",
    "06_additional-analyses", 
    "02_pairwise-tests"
  ),
  rds = FALSE
)

save_list_to_file(
  pairwise,
  here(
    "04_analysis",
    "06_additional-analyses", 
    "02_pairwise-tests"
  ),
  rds = FALSE
)
