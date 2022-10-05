library(tidyverse)
library(here)

# load functions ----

list.files(here("R", "00_load-functions"), full.names = TRUE) |> 
  purrr::walk(source)

# load data, merge, and nest ----

dat <- read_csvs(here("01_data", "01_raw")) |> 
  janitor::clean_names() |>
  group_by(study) |> 
  nest()
