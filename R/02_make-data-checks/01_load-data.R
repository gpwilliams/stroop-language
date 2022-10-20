library(tidyverse)
library(here)

rt_dat <- read_csv(here("01_data", "03_filtered", "data_rt.csv"))
accuracy_dat <- read_csv(here("01_data", "03_filtered", "data_accuracy.csv"))
demo <- read_csv(here("01_data", "03_filtered", "demographics.csv"))