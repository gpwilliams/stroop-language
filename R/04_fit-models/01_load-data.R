dat_rt <- read_csv(here("01_data", "03_filtered", "data_rt.csv")) |> 
  group_by(study) |> 
  nest()

dat_accuracy <- read_csv(here("01_data", "03_filtered", "data_accuracy.csv")) |> 
  group_by(study) |> 
  nest()