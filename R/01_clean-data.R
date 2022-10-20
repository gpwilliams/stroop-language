# load libraries ----

library(tidyverse)
library(here)
library(testthat)

# load functions ----

list.files(here("R", "00_load-functions"), full.names = TRUE) |> 
  purrr::walk(source)

r_file_list <- list.files(
  here::here("R", "01_clean-data"), 
  full.names = TRUE
) |> purrr::walk(source)
