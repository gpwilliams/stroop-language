# set names
# NOTE: this assumes all studies and marginal means use the same contrasts
# for a given outcome since they were produced with expand.grid

replacements <- c(
  "_stroop" = "", 
  "trial_type" = "trial-type",
  " ~" = "_",
  " \\* " = "-by-",
  " \\| " = "-groupedby-"
)

# Mixed effects models ----

## RT ----

# get model names: set as data set by variables
rt_model_names <- paste(
  names(rt_emms), 
  as.character(rt_grid$formula)
) |> 
  str_replace_all(replacements)

names(rt_emms) <- rt_model_names
names(rt_pairs) <- rt_model_names
names(rt_pairs_diffs) <- rt_model_names[grouped_pairs_index_rt]
names(rt_pairs_diffs_asym) <- paste0(rt_model_names[asym_pairs_index_rt], "_asym")

names(rt_emms_resp) <- rt_model_names
names(rt_pairs_resp) <- rt_model_names
names(rt_pairs_diffs_resp) <- rt_model_names[grouped_pairs_index_rt]
names(rt_pairs_diffs_asym_resp) <- paste0(rt_model_names[asym_pairs_index_rt], "_asym")

## Accuracy ----

accuracy_model_names <- paste(
  names(accuracy_emms), 
  as.character(accuracy_grid$formula)
) |> 
  str_replace_all(replacements)

names(accuracy_emms) <- accuracy_model_names
names(accuracy_pairs) <- accuracy_model_names
names(accuracy_pairs_diffs) <- accuracy_model_names[grouped_pairs_index_accuracy]
names(accuracy_pairs_diffs_asym) <- paste0(accuracy_model_names[asym_pairs_index_accuracy], "_asym")

names(accuracy_emms_resp) <- accuracy_model_names
names(accuracy_pairs_resp) <- accuracy_model_names
names(accuracy_pairs_diffs_resp) <- accuracy_model_names[grouped_pairs_index_accuracy]
names(accuracy_pairs_diffs_asym_resp) <- paste0(accuracy_model_names[asym_pairs_index_accuracy], "_asym")

# ANOVA ----

anova_rt_model_names <- paste(
  names(anova_rt_emms), 
  as.character(anova_rt_grid$formula)
) |> 
  str_replace_all(replacements)

names(anova_rt_emms) <- anova_rt_model_names
names(anova_rt_pairs) <- anova_rt_model_names
names(anova_rt_pairs_diffs) <- anova_rt_model_names[anova_grouped_pairs_index_rt]

## Accuracy ----

anova_accuracy_model_names <- paste(
  names(anova_accuracy_emms), 
  as.character(anova_accuracy_grid$formula)
) |> 
  str_replace_all(replacements)

names(anova_accuracy_emms) <- anova_accuracy_model_names
names(anova_accuracy_pairs) <- anova_accuracy_model_names
names(anova_accuracy_pairs_diffs) <- anova_accuracy_model_names[anova_grouped_pairs_index_accuracy]
