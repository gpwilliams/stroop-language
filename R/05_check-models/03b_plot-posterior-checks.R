# rt posterior ----

rt_posterior_plots <- plot_ppc(
  bayes_rt_mod_posterior_files,
  rt_prior_titles, 
  "Posterior predictive checks",
  "Reaction Time"
)

png(here(
  "03_plots", 
  "01_data-checks", 
  "03_bayesian_posterior-checks", 
  "rt_posteriors.png"
),
width = 1200, height = 800)
print(rt_posterior_plots)
dev.off()

rm(rt_posterior_plots)

# accuracy posterior ----

accuracy_posterior_plots <- plot_ppc(
  bayes_accuracy_mod_posterior_files, 
  accuracy_prior_titles, 
  "Posterior predictive checks",
  "Accuracy"
)

png(here(
  "03_plots", 
  "01_data-checks", 
  "03_bayesian_posterior-checks", 
  "accuracy_posteriors.png"
),
width = 1200, height = 800)
print(accuracy_posterior_plots)
dev.off()

rm(accuracy_posterior_plots)
