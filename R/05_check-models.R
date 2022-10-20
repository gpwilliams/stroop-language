# load libraries ----

library(tidyverse)
library(here)
library(afex)
library(brms)
library(broom.mixed)
library(bayestestR)
library(bayesplot)

# set plotting theme ----

# necessary to avoid a strange plotting error
bayesplot_theme_set(
  theme_default() + 
    theme(text = element_text(family = "Arial"))
)

# theme to make text larger
thm <- list(theme(text=element_text(size=rel(5))))

# load functions ----

list.files(here("R", "00_load-functions"), full.names = TRUE) |> 
  purrr::walk(source)

r_file_list <- list.files(
  here::here("R", "05_check-models"), 
  full.names = TRUE
) |> purrr::walk(source)
