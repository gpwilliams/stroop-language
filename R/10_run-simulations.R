# load libraries ----

library(here)
library(quarto)

# render output
quarto_render(
  input = here("R", "10_run-simulations", "simulate-data.qmd"),
  output_file = here("05_reports", "simulations.html"),
  output_format = "html"
)