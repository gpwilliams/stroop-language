# write cleaned and cleaned & filtered data to file

write_csv(dat_cleaned3, here("01_data", "02_cleaned", "data.csv"))
write_csv(dat_filtered, here("01_data", "03_filtered", "data.csv"))

write_csv(demo_cleaned, here("01_data", "02_cleaned", "demographics.csv"))
write_csv(demo_filtered, here("01_data", "03_filtered", "demographics.csv"))
