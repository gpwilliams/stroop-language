# load data
dat_rt <- read_csv(here("01_data", "03_filtered", "data_rt.csv"))
dat_accuracy <- read_csv(here("01_data", "03_filtered", "data_accuracy.csv"))

emms_rt <- read_csv(here("04_analysis", "05_pairwise-tests", "rt_emms_resp.csv")) |> 
  filter(analysis == "stroop-by-language-by-trial-type") |>
  separate(
    condition,
    into = c("stroop", "language", "trial_type"),
    sep = " "
  ) 

emms_rt$stroop <- factor(
  emms_rt$stroop,
  levels = c("Neutral", "Incongruent")
)

emms_rt$trial_type <- factor(
  emms_rt$trial_type,
  levels = c("Repetition", "Switch")
)

emms_rt <- emms_rt |>
  mutate(conditions = interaction(stroop, trial_type, sep = " × "))

emms_accuracy <- read_csv(here("04_analysis", "05_pairwise-tests", "accuracy_emms_resp.csv")) |> 
  filter(analysis == "stroop-by-language-by-trial-type") |>
  separate(
    condition,
    into = c("stroop", "language", "trial_type"),
    sep = " "
  ) |> 
  rename(emmean = prob)

emms_accuracy$stroop <- factor(
  emms_accuracy$stroop,
  levels = c("Neutral", "Incongruent")
)

emms_accuracy$trial_type <- factor(
  emms_accuracy$trial_type,
  levels = c("Repetition", "Switch")
)

emms_accuracy <- emms_accuracy |> 
  mutate(conditions = interaction(stroop, trial_type, sep = " × "))
