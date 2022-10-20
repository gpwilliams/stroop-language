# model options ----

# brms options
analysis_options <- list(
  cores = 4,
  chains = 4,
  warmup = 1000,
  iter = 11000,
  rand_seed = 1892,
  adapt_delta = 0.8,
  max_treedepth = 10
)

# ANOVA options
afex_options("include_aov" = TRUE)

