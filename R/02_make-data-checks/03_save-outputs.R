# save outputs ----

write_csv(rt_summary, here("04_analysis", "01_data-checks", "rt_summary.csv"))
write_csv(accuracy_summary, here("04_analysis", "01_data-checks", "accuracy_summary.csv"))

ggsave(
  here("03_plots", "01_data-checks", "rt_plot.png"), 
  rt_plot, 
  width = 12, 
  height = 8
)

ggsave(
  here("03_plots", "01_data-checks", "rt_trial_count_plot.png"), 
  rt_trial_count_plot, 
  width = 12, 
  height = 8
)

ggsave(
  here("03_plots", "01_data-checks", "accuracy_plot.png"), 
  accuracy_plot, 
  width = 12, 
  height = 8
)
