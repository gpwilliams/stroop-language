# load data, merge, and nest ----

dat <- read_csvs(
  here("01_data", "01_raw"), 
  .exclude = c("demographics", "performance_exclusions")
  ) |> 
  janitor::clean_names()

performance_exclusions <- read_csv(here("01_data", "01_raw", "performance_exclusions.csv"))

demo <- read_csvs(
  here("01_data", "01_raw"), 
  .skip_first = FALSE, 
  .include = "demographics"
  ) |> 
  janitor::clean_names() |> 
  mutate(study = str_replace(study, "_demographics", ""))
