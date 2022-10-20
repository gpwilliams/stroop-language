# fit model ----

fit_models <- function(formula, data, family, priors, analysis_options){
  
  # create data holder
  models_list <- list()
  
  # loop through all supplied sd_vector values and
  # fit a model with that prior
  for(i in seq_along(priors)) {
    
    message(paste("Fitting model", i, "of", length(priors)))
    
    # fit the models
    models_list[[i]] <- brms::brm(
      formula = formula, 
      data = data,
      family = family,
      prior = priors[[i]],
      cores = analysis_options$cores,
      chains = analysis_options$chains,
      warmup = analysis_options$warmup,
      iter = analysis_options$iter,
      seed = analysis_options$rand_seed,
      control = list(
        adapt_delta = analysis_options$adapt_delta, 
        max_treedepth = analysis_options$max_treedepth
      ),
      sample_prior = TRUE,
      save_pars = save_pars(all = TRUE),
      backend = "cmdstanr"
    )
  }
  
  # set names for models
  names(models_list) <- paste0("model_prior_", seq_along(priors))
  models_list # return it
}
