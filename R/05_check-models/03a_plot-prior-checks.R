# conduct prior checks ----

message("Making prior predictive checks.")

# rt prior ----

rt_prior_plots <- plot_ppc(
  bayes_rt_mod_prior_files,
  rt_prior_titles, 
  "Prior predictive checks",
  "Reaction Time"
)

png(here(
  "03_plots", 
  "01_data-checks", 
  "02_bayesian_prior-checks", 
  "rt_priors.png"
),
width = 1200, height = 800)
print(rt_prior_plots)
dev.off()

rm(rt_prior_plots)

# accuracy prior ----

accuracy_prior_plots <- plot_ppc(
  bayes_accuracy_mod_prior_files, 
  accuracy_prior_titles, 
  "Prior predictive checks",
  "Accuracy"
)

png(here(
  "03_plots", 
  "01_data-checks", 
  "02_bayesian_prior-checks", 
  "accuracy_priors.png"
),
width = 1200, height = 800)
print(accuracy_prior_plots)
dev.off()

rm(accuracy_prior_plots)
