
# create columns ----

dat_cleaned <- dat |>
  rename(trial = subject_trial_number) |> 
  mutate(
    correct = case_when(
      accuracy == "Correct" ~ 1,
      TRUE ~ 0
    ),
    log_rt = log(rt)
  )

# make first trial ID ----

# get the first trial in each block for each participant
# this is safer than assuming a fixed trial number in case of dropped
# trials during data collection
block_trial_ids <- dat_cleaned |> 
  group_by(subject_id, block) |> 
  summarise(trial = min(trial)) |> 
  ungroup() |> 
  mutate(first_trial_in_block = TRUE)

dat_cleaned2 <- left_join(
  dat_cleaned, 
  block_trial_ids, 
  by = c("subject_id", "block", "trial")
) |> 
  mutate(first_trial_in_block = replace_na(first_trial_in_block, FALSE))

# make recovery trials ----

# for each participant look to the previous trial
# if it was incorrect treat the current one as a recovery trial
# if not, it isn't a recovery trial
dat_cleaned3 <- dat_cleaned2 |> 
  group_by(subject_id) |> 
  mutate(
    recovery = case_when(
      lag(correct) == 0 ~ TRUE,
      TRUE ~ FALSE
    )
  )
