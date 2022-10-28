options(
  emmeans = list(
    lmer.df = "satterthwaite"
  )
)

# RT ----

# update reference grid to allow calculation of rt on raw scale
mixed_freq_rt <- map(
  mixed_freq_rt,
  ~update(ref_grid(.x), tran = "log")
)

# arabic = main effects, language by trial type, chinese same, dutch main only

## log scale ----

rt_grid <- expand.grid(
  ref_grid = mixed_freq_rt, 
  formula = c(
    formula("~stroop"), 
    formula("~language"),
    formula("~trial_type"),
    formula("~language*trial_type")
  )
)

rt_emms <- map2(
  rt_grid$ref_grid, rt_grid$formula,
  ~emmeans(.x, .y)
)

rt_pairs <- map(
  rt_emms,
  ~pairs(.x)
)

## response scale ----

rt_emms_resp <- map(
  rt_emms,
  ~update(.x, type = "response")
)

rt_pairs_resp <- map(
  rt_emms_resp,
  ~pairs(.x)
)

# Accuracy

accuracy_grid <- expand.grid(
  ref_grid = mixed_freq_accuracy, 
  formula = c(
    formula("~stroop"), 
    formula("~language"),
    formula("~trial_type"),
    formula("~language*trial_type")
  )
)

accuracy_emms <- map2(
  accuracy_grid$ref_grid, accuracy_grid$formula,
  ~emmeans(.x, .y)
)

accuracy_pairs <- map(
  accuracy_emms,
  ~pairs(.x)
)

## response scale ----

accuracy_emms_resp <- map(
  accuracy_emms,
  ~update(.x, type = "response")
)

accuracy_pairs_resp <- map(
  accuracy_emms_resp,
  ~pairs(.x)
)
