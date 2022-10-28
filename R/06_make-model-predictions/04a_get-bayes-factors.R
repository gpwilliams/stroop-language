# make bayes factor summaries ----

prior_models_rt <- map_files_to_list(bayes_rt_mod_prior_files)
posterior_models_rt <- map_files_to_list(bayes_rt_mod_posterior_files)

# get bayes factors
bfs_rt <- map2(
  posterior_models_rt,
  prior_models_rt,
  ~bayesfactor_parameters(posterior = .x, prior = .y)
)

# clean up
rm(prior_models_rt, posterior_models_rt)

# accuracy ----

prior_models_accuracy <- map_files_to_list(bayes_accuracy_mod_prior_files)
posterior_models_accuracy <- map_files_to_list(bayes_accuracy_mod_posterior_files)

# get bayes factors
bfs_accuracy <- map2(
  posterior_models_accuracy,
  prior_models_accuracy,
  ~bayesfactor_parameters(posterior = .x, prior = .y)
)

# clean up
rm(prior_models_accuracy, posterior_models_accuracy)
