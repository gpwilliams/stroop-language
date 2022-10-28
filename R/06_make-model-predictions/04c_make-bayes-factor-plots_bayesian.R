# make bf plots

# for intepretation of Bayes factors (on log scale) see:
# https://ptfonseca.github.io/pcal/reference/bfactor_log_interpret.html

bf_plot_rt <- bf_table_rt |> 
  filter(Parameter != "Intercept") |> 
  ggplot(aes(x = Prior_Model, y = log_BF, group = Study)) +
  facet_grid(Study ~ Parameter, scales = "free_y", labeller = label_wrap_gen()) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  annotate("rect", xmin = 0, xmax = 6, ymin = -log(3), ymax = log(3), alpha = .2) +
  labs(
    title = "Bayes Factor Sensitivity",
    subtitle = paste(reduced_rt_prior_summary$plot_title, collapse = "\n"),
    x = "Model", 
    y = "Log BF",
    caption = "Grey band indicates weak (Kass & Raftery, 1995) or anecdotal evidence (Jeffreys, 1961)."
  )

ggsave(
  here("03_plots", "02_sensitivity-checks", "02_bayesian", "rt_bayes-factors.png"),
  bf_plot_rt,
  height = 8,
  width = 12
)

bf_plot_accuracy <- bf_table_accuracy |> 
  filter(Parameter != "Intercept") |> 
  ggplot(aes(x = Prior_Model, y = log_BF, group = Study)) +
  facet_grid(Study ~ Parameter, scales = "free_y", labeller = label_wrap_gen()) +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  annotate("rect", xmin = 0, xmax = 6, ymin = -log(3), ymax = log(3), alpha = .2) +
  labs(
    title = "Bayes Factor Sensitivity",
    subtitle = paste(reduced_rt_prior_summary$plot_title, collapse = "\n"),
    x = "Model", 
    y = "Log BF",
    caption = "Grey band indicates weak (Kass & Raftery, 1995) or anecdotal evidence (Jeffreys, 1961)."
  )

ggsave(
  here("03_plots", "02_sensitivity-checks", "02_bayesian", "accuracy_bayes-factors.png"),
  bf_plot_accuracy,
  height = 8,
  width = 12
)

# TODO
# 5. make any predictions based on the primary models
# 6. make any additional plots needed based on primary models
