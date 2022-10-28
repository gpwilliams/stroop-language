# load priors

# read in models ----

# loads it to a list called rt_priors
source(here(
  "R",
  "04_fit-models",
  "08b_set-priors_rt_bayes.R"
))

# loads it to a list called accuracy_priors
source(here(
  "R",
  "04_fit-models",
  "09b_set-priors_accuracy_bayes.R"
))
