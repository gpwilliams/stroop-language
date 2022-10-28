# plot fixef

bayesian_fixef_plot_rt <- bayesian_fixef_table_rt |>
  filter(Parameter != "Intercept") |> 
  ggplot(aes(x = Prior_Model, y = Estimate, ymin = `Q2.5`, ymax = `Q97.5`, group = Study)) +
  facet_grid(Study~Parameter, labeller = label_wrap_gen()) +
  geom_line() +
  geom_point(colour = "white", pch = 16, size = 5) +
  geom_pointrange(fill = "black", pch = 21, size = 0.5) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Parameter Estimate Sensitivity",
    subtitle = paste(reduced_rt_prior_summary$plot_title, collapse = "\n"),
    x = "Model", 
    y = "β"
  )

ggsave(
  here("03_plots", "02_sensitivity-checks", "02_bayesian", "rt_fixef.png"),
  bayesian_fixef_plot_rt,
  height = 8,
  width = 12
)

bayesian_fixef_plot_accuracy <- bayesian_fixef_table_accuracy |>
  filter(Parameter != "Intercept") |> 
  ggplot(aes(x = Prior_Model, y = Estimate, ymin = `Q2.5`, ymax = `Q97.5`, group = Study)) +
  facet_grid(Study~Parameter, labeller = label_wrap_gen()) +
  geom_line() +
  geom_point(colour = "white", pch = 16, size = 5) +
  geom_pointrange(fill = "black", pch = 21, size = 0.5) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Parameter Estimate Sensitivity",
    subtitle = paste(reduced_accuracy_prior_summary$plot_title, collapse = "\n"),
    x = "Model", 
    y = "β"
  )

ggsave(
  here("03_plots", "02_sensitivity-checks", "02_bayesian", "accuracy_fixef.png"),
  bayesian_fixef_plot_accuracy,
  height = 8,
  width = 12
)
