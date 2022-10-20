# write cleaned and cleaned & filtered data to file

# accuracy data ----
write_csv(dat_accuracy, here("01_data", "02_cleaned", "data_accuracy.csv"))
write_csv(dat_accuracy_filtered, here("01_data", "03_filtered", "data_accuracy.csv"))

# reaction time data ----
write_csv(dat_rt, here("01_data", "02_cleaned", "data_rt.csv"))
write_csv(dat_rt_filtered, here("01_data", "03_filtered", "data_rt.csv"))

# demographics ----

# note the same is used as filtered only refers to trials filtered 
# within the study, not participants filtered from it
# all cleaned and filtered data sets have participant exclusions
write_csv(demo_filtered, here("01_data", "02_cleaned", "demographics.csv"))
write_csv(demo_filtered, here("01_data", "03_filtered", "demographics.csv"))

# number of participant numbers and trials along with number of exclusions
write_csv(participant_numbers, here("04_analysis", "01_data-checks", "participant_numbers.csv"))
write_csv(trial_numbers, here("04_analysis", "01_data-checks", "trial_numbers.csv"))
