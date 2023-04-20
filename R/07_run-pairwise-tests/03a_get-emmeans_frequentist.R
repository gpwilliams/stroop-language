# Mixed effects models ----

## RT ----

# update reference grid to allow calculation of rt on raw scale
mixed_freq_rt <- map(
  mixed_freq_rt,
  ~update(ref_grid(.x), tran = "log")
)

# arabic = main effects, language by trial type, chinese same, dutch main only

### log scale ----

rt_grid <- expand.grid(
  ref_grid = mixed_freq_rt, 
  formula = c(
    formula("~stroop"), 
    formula("~language"),
    formula("~trial_type"),
    formula("~trial_type|language"),
    formula("~trial_type|stroop*language")
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

# get formulae that contain a bar (for grouped pairs)
grouped_pairs_index_rt <- grep("\\|", rt_grid$formula)

# make difference in differences for grouped pairwise tests
rt_pairs_diffs <- rt_pairs[grouped_pairs_index_rt] |> 
  map(~pairs(.x, by = NULL, adjust = "tukey"))

# look for asymmetrical switch costs (get index)
asym_pairs_index_rt <- which("~trial_type | stroop * language" == rt_grid$formula)

# make difference in differences for asymmetrical switch costs
rt_pairs_diffs_asym <- rt_pairs[asym_pairs_index_rt] |> 
  map(~rbind(pairs(.x, by = "stroop"), adjust = "holm"))

### response scale ----

rt_emms_resp <- map(
  rt_emms,
  ~update(.x, type = "response")
)

rt_pairs_resp <- map(
  rt_emms_resp,
  ~pairs(.x)
)

rt_pairs_diffs_resp <- rt_pairs_resp[grouped_pairs_index_rt] |> 
  map(~pairs(.x, by = NULL, adjust = "tukey"))

rt_pairs_diffs_asym_resp <- rt_pairs_resp[asym_pairs_index_rt] |> 
  map(~rbind(pairs(.x, by = "stroop"), adjust = "holm"))

## Accuracy ----

accuracy_grid <- expand.grid(
  ref_grid = mixed_freq_accuracy, 
  formula = c(
    formula("~stroop"), 
    formula("~language"),
    formula("~trial_type"),
    formula("~trial_type|language"),
    formula("~trial_type|stroop*language")
  )
)

### log scale ---

accuracy_emms <- map2(
  accuracy_grid$ref_grid, accuracy_grid$formula,
  ~emmeans(.x, .y)
)

accuracy_pairs <- map(
  accuracy_emms,
  ~pairs(.x)
)

# get formulae that contain a bar (for grouped pairs)
grouped_pairs_index_accuracy <- grep("\\|", accuracy_grid$formula)

# make difference in differences for grouped pairwise tests
accuracy_pairs_diffs <- accuracy_pairs[grouped_pairs_index_accuracy] |> 
  map(~pairs(.x, by = NULL, adjust = "tukey"))

# look for asymmetrical switch costs (get index)
asym_pairs_index_accuracy <- which("~trial_type | stroop * language" == rt_grid$formula)

# make difference in differences for asymmetrical switch costs
accuracy_pairs_diffs_asym <- accuracy_pairs[asym_pairs_index_accuracy] |> 
  map(~rbind(pairs(.x, by = "stroop"), adjust = "holm"))

### response scale ----

accuracy_emms_resp <- map(
  accuracy_emms,
  ~update(.x, type = "response")
)

accuracy_pairs_resp <- map(
  accuracy_emms_resp,
  ~pairs(.x)
)

accuracy_pairs_diffs_resp <- accuracy_pairs_resp[grouped_pairs_index_accuracy] |> 
  map(~pairs(.x, by = NULL, adjust = "tukey"))

accuracy_pairs_diffs_asym_resp <- accuracy_pairs_resp[asym_pairs_index_accuracy] |> 
  map(~rbind(pairs(.x, by = "stroop"), adjust = "holm"))

# ANOVA ----

## RT ----

### response scale ----

anova_rt_grid <- expand.grid(
  ref_grid = anova_freq_rt, 
  formula = c(
    formula("~stroop"), 
    formula("~language"),
    formula("~stroop|language"),
    formula("~language|stroop")
  )
)

anova_rt_emms <- map2(
  anova_rt_grid$ref_grid, anova_rt_grid$formula,
  ~emmeans(.x, .y)
)

anova_rt_pairs <- map(
  anova_rt_emms,
  ~pairs(.x)
)

# get formulae that contain a bar (for grouped pairs)
anova_grouped_pairs_index_rt <- grep("\\|", anova_rt_grid$formula)

# make difference in differences for grouped pairwise tests
anova_rt_pairs_diffs <- anova_rt_pairs[anova_grouped_pairs_index_rt] |> 
  map(~pairs(.x, by = NULL, adjust = "tukey"))

## Accuracy ----

anova_accuracy_grid <- expand.grid(
  ref_grid = anova_freq_accuracy, 
  formula = c(
    formula("~stroop"), 
    formula("~language"),
    formula("~stroop|language"),
    formula("~language|stroop")
  )
)

### response scale ---

anova_accuracy_emms <- map2(
  anova_accuracy_grid$ref_grid, anova_accuracy_grid$formula,
  ~emmeans(.x, .y)
)

anova_accuracy_pairs <- map(
  anova_accuracy_emms,
  ~pairs(.x)
)

# get formulae that contain a bar (for grouped pairs)
anova_grouped_pairs_index_accuracy <- grep("\\|", anova_accuracy_grid$formula)

# make difference in differences for grouped pairwise tests
anova_accuracy_pairs_diffs <- anova_accuracy_pairs[anova_grouped_pairs_index_accuracy] |> 
  map(~pairs(.x, by = NULL, adjust = "tukey"))
