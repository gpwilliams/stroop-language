frequentist_rt_mixed_plots <- map_files_to_list(
  here("04_analysis", "03_models", "01_frequentist", "01_rt_mixed")
) |> 
  map(~performance::check_model(.$full_model))

for(i in seq_along(frequentist_rt_mixed_plots)) {
  png(here(
    "03_plots", 
    "01_data-checks", 
    "01_frequentist_mixed-effects", 
    paste0(names(frequentist_rt_mixed_plots)[i], ".png")
  ),
  width = 1200, height = 800)
  print(frequentist_rt_mixed_plots[[i]])
  dev.off()
}

frequentist_accuracy_mixed_plots <- map_files_to_list(
  here("04_analysis", "03_models", "01_frequentist", "02_accuracy_mixed")
) |> 
  map(~performance::check_model(.$full_model))

for(i in seq_along(frequentist_accuracy_mixed_plots)) {
  png(here(
    "03_plots", 
    "01_data-checks", 
    "01_frequentist_mixed-effects", 
    paste0(names(frequentist_accuracy_mixed_plots)[i], ".png")
  ),
  width = 1200, height = 800)
  print(frequentist_accuracy_mixed_plots[[i]])
  dev.off()
}