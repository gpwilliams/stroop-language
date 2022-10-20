# load libraries ----

library(tidyverse)
library(here)

# load functions ----

list.files(here("R", "00_load-functions"), full.names = TRUE) |> 
  purrr::walk(source)

r_file_list <- list.files(
  here::here("R", "02_make-data-checks"), 
  full.names = TRUE
) |> purrr::walk(source)
