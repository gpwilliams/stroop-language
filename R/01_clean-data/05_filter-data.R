# filter data ----

# participants who didn't complete the study correctly (see reasons later)
# note that those in the arabic study who didn't comply with instructions
# were removed prior to coding so aren't removed again here.
# though the reasons are stated in the summarise-exclusions.R file.

# get any participants with ages < 18
underage_subjects <- demo_cleaned |> 
  filter(age < 18) |> 
  select(study, subject_id) |> 
  mutate(reason = "under age")

exclusions <- bind_rows(underage_subjects, performance_exclusions)

# remove them from the data 
# (done separately to below as underage subjects should never be there)
dat_accuracy <- dat_cleaned3 |> 
  filter(subject_id %nin% exclusions$subject_id)

# remove the first trial in each block recovery trials
dat_accuracy_filtered <- dat_accuracy |> 
  filter(
    first_trial_in_block == FALSE,
    recovery == FALSE
  )

# keep only correct trials (for rts)
dat_rt <- dat_accuracy_filtered |> 
  filter(accuracy == "Correct")

# remove extreme RTs, incorrect trials, first ones in the block, 
# and recovery trials
# then remove latencies with 3 SD above the participant mean
dat_rt_filtered <- dat_rt |> 
  filter(rt > 150 & rt < 2500) |> 
  group_by(subject_id) |> 
  filter(rt < (mean(rt, na.rm = TRUE) + 3*sd(rt, na.rm = TRUE))) |> 
  ungroup()

# keep only subjects who progressed to the final part of the study
demo_filtered <- demo_cleaned |> 
  filter(
    subject_id %nin% exclusions$subject_id,
    subject_id %in% unique(dat_rt_filtered$subject_id)
  )

# check numbers align
testthat::expect_equal(
  length(unique(dat_rt_filtered$subject_id)),
  nrow(demo_filtered)
)
