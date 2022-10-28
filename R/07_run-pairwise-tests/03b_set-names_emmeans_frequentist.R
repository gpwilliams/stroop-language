# set names
# NOTE: this assumes all studies and marginal means use the same contrasts
# for a given outcome since they were produced with expand.grid

# get model names: set as data set by variables
rt_model_names <- paste(
  names(rt_emms), 
  as.character(rt_grid$formula)
) |> 
  str_replace_all(c(
    "_stroop" = "", 
    "trial_type" = "trial-type",
    " ~" = "_",
    " \\* " = "-by-"
  ))

names(rt_emms) <- rt_model_names
names(rt_pairs) <- rt_model_names
names(rt_emms_resp) <- rt_model_names
names(rt_pairs_resp) <- rt_model_names

accuracy_model_names <- paste(
  names(accuracy_emms), 
  as.character(accuracy_grid$formula)
) |> 
  str_replace_all(c(
    "_stroop" = "", 
    "trial_type" = "trial-type",
    " ~" = "_",
    " \\* " = "-by-"
  ))

names(accuracy_emms) <- accuracy_model_names
names(accuracy_pairs) <- accuracy_model_names
names(accuracy_emms_resp) <- accuracy_model_names
names(accuracy_pairs_resp) <- accuracy_model_names
