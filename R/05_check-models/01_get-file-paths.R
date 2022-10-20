# get model file paths ----

# rt prior
bayes_rt_mod_prior_files <- list.files(here::here(
  "04_analysis", 
  "03_models",
  "02_bayesian",
  "01b_rt_bayesian_prior"
), full.names = TRUE)

# accuracy prior
bayes_accuracy_mod_prior_files <- list.files(here::here(
  "04_analysis", 
  "03_models",
  "02_bayesian",
  "02b_accuracy_bayesian_prior"
), full.names = TRUE)

# rt posterior 
bayes_rt_mod_posterior_files <- list.files(here::here(
  "04_analysis", 
  "03_models",
  "02_bayesian",
  "01a_rt_bayesian_posterior"
), full.names = TRUE)

# accuracy posterior
bayes_accuracy_mod_posterior_files <- list.files(here::here(
  "04_analysis", 
  "03_models",
  "02_bayesian",
  "02a_accuracy_bayesian_posterior"
), full.names = TRUE)
