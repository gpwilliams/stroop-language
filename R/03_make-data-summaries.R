# load libraries ----

library(tidyverse)
library(here)

# load functions ----

list.files(here("R", "00_load-functions"), full.names = TRUE) |> 
  purrr::walk(source)

list.files(
  here::here("R", "03_make-data-summaries"), 
  full.names = TRUE
) |> purrr::walk(source)
