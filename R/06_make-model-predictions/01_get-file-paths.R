# get model file paths ----

## Bayesian ----

# rt prior
bayes_rt_mod_prior_files <- here::here(
  "04_analysis", 
  "03_models",
  "02_bayesian",
  "01b_rt_bayesian_prior"
)

# accuracy prior
bayes_accuracy_mod_prior_files <- here::here(
  "04_analysis", 
  "03_models",
  "02_bayesian",
  "02b_accuracy_bayesian_prior"
)

# rt posterior 
bayes_rt_mod_posterior_files <- here::here(
  "04_analysis", 
  "03_models",
  "02_bayesian",
  "01a_rt_bayesian_posterior"
)

# accuracy posterior
bayes_accuracy_mod_posterior_files <- here::here(
  "04_analysis", 
  "03_models",
  "02_bayesian",
  "02a_accuracy_bayesian_posterior"
)

## Frequentist ----

# Multilevel

# rt
freq_rt_mod_mixed <- here::here(
  "04_analysis", 
  "03_models",
  "01_frequentist",
  "01_rt_mixed"
)

# accuracy
freq_accuracy_mod_mixed <- here::here(
  "04_analysis", 
  "03_models",
  "01_frequentist",
  "02_accuracy_mixed"
)

# ANOVA

# rt
freq_rt_mod_anova <- here::here(
  "04_analysis", 
  "03_models",
  "01_frequentist",
  "03_rt_anova"
)

# accuracy
freq_accuracy_mod_anova <- here::here(
  "04_analysis", 
  "03_models",
  "01_frequentist",
  "04_accuracy_anova"
)
