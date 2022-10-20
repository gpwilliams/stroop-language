# fit models ----

## arabic ----

rt_models_bayesian_arabic <- fit_models(
  formula = rt_formula,
  data = dat_rt$data[[1]],
  family = lognormal(),
  priors = rt_priors,
  analysis_options = analysis_options
)

names(rt_models_bayesian_arabic) <- paste0(
  "arabic_rt_", names(rt_models_bayesian_arabic)
)

save_list_to_file(
  rt_models_bayesian_arabic, 
  c("04_analysis", "03_models", "02_bayesian", "01a_rt_bayesian_posterior"),
  csv = FALSE
)
rm(rt_models_bayesian_arabic)

## chinese ----


rt_models_bayesian_chinese <- fit_models(
  formula = rt_formula,
  data = dat_rt$data[[2]],
  family = lognormal(),
  priors = rt_priors,
  analysis_options = analysis_options
)

names(rt_models_bayesian_chinese) <- paste0(
  "chinese_rt_", names(rt_models_bayesian_chinese)
)

save_list_to_file(
  rt_models_bayesian_chinese, 
  c("04_analysis", "03_models", "02_bayesian", "01a_rt_bayesian_posterior"),
  csv = FALSE
)
rm(rt_models_bayesian_chinese)


## dutch ----

rt_models_bayesian_dutch <- fit_models(
  formula = rt_formula,
  data = dat_rt$data[[3]],
  family = lognormal(),
  priors = rt_priors,
  analysis_options = analysis_options
)

names(rt_models_bayesian_dutch) <- paste0(
  "dutch_rt_", names(rt_models_bayesian_dutch)
)

save_list_to_file(
  rt_models_bayesian_dutch, 
  c("04_analysis", "03_models", "02_bayesian", "01a_rt_bayesian_posterior"),
  csv = FALSE
)
rm(rt_models_bayesian_dutch)
