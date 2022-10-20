# model options ----

rt_formula <- bf(
  rt ~ stroop * trial_type * language +
    (1 + stroop * trial_type * language | subject_id) + 
    (1 + trial_type | word_colour)
)
