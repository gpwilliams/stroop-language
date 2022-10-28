# get bayesian fixed effects

bayesian_rt_fixef <- map_files_to_list(bayes_rt_mod_posterior_files) |> 
  map_fixef()

bayesian_accuracy_fixef <- map_files_to_list(bayes_accuracy_mod_posterior_files) |> 
  map_fixef()
