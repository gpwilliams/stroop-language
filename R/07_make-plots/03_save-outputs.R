# save outputs ----

write_csv(
  underage_counts, 
  here("04_analysis", "02_descriptives", "underage_counts.csv")
)

write_csv(
  na_counts, 
  here("04_analysis", "02_descriptives", "na_counts.csv")
)

write_csv(
  gender_counts, 
  here("04_analysis", "02_descriptives", "gender_counts.csv")
)

write_csv(
  demo_summary, 
  here("04_analysis", "02_descriptives", "demo_summary.csv")
)
