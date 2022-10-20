library(tidyverse)
library(here)
library(ggdist)

# load data
dat_rt <- read_csv(here("01_data", "03_filtered", "data_rt.csv"))
dat_correct <- read_csv(here("01_data", "03_filtered", "data_accuracy.csv"))
