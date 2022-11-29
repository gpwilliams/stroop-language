# load libraries ----

library(tidyverse)
library(here)
library(ggdist)
library(MetBrewer)
library(patchwork)

# load functions ----
list.files(here("R", "00_load-functions"), full.names = TRUE) |> 
  purrr::walk(source)

# run files ----

r_file_list <- list.files(
  here::here("R", "08_make-plots"), 
  full.names = TRUE
) |> purrr::walk(source)