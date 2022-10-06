library(tidyverse)
library(here)
library(testthat)

# load functions ----

list.files(here("R", "00_load-functions"), full.names = TRUE) |> 
  purrr::walk(source)

# load data, merge, and nest ----

dat <- read_csvs(here("01_data", "01_raw"), .exclude = "demographics") |> 
  janitor::clean_names()

demo <- read_csvs(
  here("01_data", "01_raw"), 
  .skip_first = FALSE, 
  .include = "demographics"
  ) |> 
  janitor::clean_names()
