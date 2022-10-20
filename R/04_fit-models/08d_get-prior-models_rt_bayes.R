# unupdate models (prior only models) for Bayes factors

file_path_in <- list.files(here(
  "04_analysis", 
  "03_models", 
  "02_bayesian", 
  "01a_rt_bayesian_posterior"
), full.names = TRUE)

file_path_out <- here(
  "04_analysis", 
  "03_models", 
  "02_bayesian", 
  "01b_rt_bayesian_prior"
)

file_names <- stringr::str_sub(list.files(here(
  "04_analysis", 
  "03_models", 
  "02_bayesian", 
  "01a_rt_bayesian_posterior"
)), 1, -5)

for(i in seq_along(file_path_in)) {
  message(paste(
    "Making prior model for reaction time model", 
    i, 
    "of", 
    length(file_path_in))
  )
  mod <- read_rds(file_path_in[[i]])
  unupdated_mod <- bayestestR::unupdate(mod)
  write_rds(
    unupdated_mod,
    here(file_path_out, paste0(file_names[i], ".rds"))
  )
}
