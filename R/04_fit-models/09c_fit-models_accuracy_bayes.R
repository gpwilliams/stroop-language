# fit models ----

## arabic ----

accuracy_models_bayesian_arabic <- fit_models(
  formula = accuracy_formula,
  data = dat_accuracy$data[[1]],
  family = bernoulli(link = "logit"),
  priors = accuracy_priors,
  analysis_options = analysis_options
)

names(accuracy_models_bayesian_arabic) <- paste0(
  "arabic_accuracy_", names(accuracy_models_bayesian_arabic)
)

save_list_to_file(
  accuracy_models_bayesian_arabic, 
  c("04_analysis", "03_models", "02_bayesian", "02a_accuracy_bayesian_posterior"),
  csv = FALSE
)
rm(accuracy_models_bayesian_arabic)

## chinese ----

accuracy_models_bayesian_chinese <- fit_models(
  formula = accuracy_formula,
  data = dat_accuracy$data[[2]],
  family = bernoulli(link = "logit"),
  priors = accuracy_priors,
  analysis_options = analysis_options
)

names(accuracy_models_bayesian_chinese) <- paste0(
  "chinese_accuracy_", names(accuracy_models_bayesian_chinese)
)

save_list_to_file(
  accuracy_models_bayesian_chinese, 
  c("04_analysis", "03_models", "02_bayesian", "02a_accuracy_bayesian_posterior"),
  csv = FALSE
)
rm(accuracy_models_bayesian_chinese)

## dutch ----

accuracy_models_bayesian_dutch <- fit_models(
  formula = accuracy_formula,
  data = dat_accuracy$data[[3]],
  family = bernoulli(link = "logit"),
  priors = accuracy_priors,
  analysis_options = analysis_options
)

names(accuracy_models_bayesian_dutch) <- paste0(
  "dutch_accuracy_", names(accuracy_models_bayesian_dutch)
)

save_list_to_file(
  accuracy_models_bayesian_dutch, 
  c("04_analysis", "03_models", "02_bayesian", "02a_accuracy_bayesian_posterior"),
  csv = FALSE
)
rm(accuracy_models_bayesian_dutch)
