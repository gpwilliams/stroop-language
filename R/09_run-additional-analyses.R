# load libraries ----

library(tidyverse)
library(here)
library(afex)
library(brms)
library(broom.mixed)
library(bayestestR)
library(emmeans)
library(MetBrewer)
library(patchwork)

emm_options(lmer.df = "asymptotic") # df calculation; z-to-t

# load functions ----

list.files(here("R", "00_load-functions"), full.names = TRUE) |> 
  purrr::walk(source)

list.files(
  here::here("R", "09_run-additional-analyses"), 
  full.names = TRUE
) |> purrr::walk(source)
