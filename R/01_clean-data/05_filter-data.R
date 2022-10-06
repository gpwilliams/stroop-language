# filter data ----

# get any participants with ages < 18
underage_participants <- demo_cleaned |> 
  filter(age < 18) |> 
  pull(subject_id)

# remove extreme RTs, incorrect trials, first ones in the block, 
# and recovery trials
# then remove latencies with 3 SD above the participant mean
dat_filtered <- dat_cleaned3 |> 
  filter(
    subject_id %nin% underage_participants,
    rt > 150 & rt < 2500, 
    accuracy == "Correct", 
    first_trial_in_block == FALSE,
    recovery == FALSE
  ) |> 
  group_by(subject_id) |> 
  filter(rt < (mean(rt, na.rm = TRUE) + 3*sd(rt, na.rm = TRUE))) |> 
  ungroup()

# keep only subjects who progressed to the final part
demo_filtered <- demo_cleaned |> 
  filter(
    subject_id %nin%underage_participants,
    subject_id %in% unique(dat_filtered$subject_id)
  )

# check numbers align
testthat::expect_equal(
  length(unique(dat_filtered$subject_id)),
  nrow(demo_filtered)
)
