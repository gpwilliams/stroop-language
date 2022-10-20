# load libraries ----

library(tidyverse)
library(here)
library(afex)
library(brms)
library(broom.mixed)
library(bayestestR)

# load functions ----

list.files(here("R", "00_load-functions"), full.names = TRUE) |> 
  purrr::walk(source)

r_file_list <- list.files(
  here::here("R", "04_fit-models"), 
  full.names = TRUE
) |> purrr::walk(source)
