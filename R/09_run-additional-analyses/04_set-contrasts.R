# set contrasts ----

# set contrasts for R
contrasts(dat_rt$study) <- contr.sum(3) # base = chinese
contrasts(dat_rt$stroop) <- contr.sum(2) # base = neutral
contrasts(dat_rt$trial_type) <- contr.sum(2) # base = repetition
contrasts(dat_rt$language) <- contr.sum(2) # base = english
contrasts(dat_rt$subject_id) <- contr.sum(length(unique(dat_rt$subject_id)))
contrasts(dat_rt$word_colour) <- contr.sum(length(unique(dat_rt$word_colour)))

# set contrasts for R for Chinese only
contrasts(dat_rt_chinese$study) <- contr.sum(3) # base = chinese
contrasts(dat_rt_chinese$stroop) <- contr.sum(2) # base = neutral
contrasts(dat_rt_chinese$trial_type) <- contr.sum(2) # base = repetition
contrasts(dat_rt_chinese$language) <- contr.sum(2) # base = english
contrasts(dat_rt_chinese$subject_id) <- contr.sum(length(unique(dat_rt_chinese$subject_id)))
contrasts(dat_rt_chinese$word_colour) <- contr.sum(length(unique(dat_rt_chinese$word_colour)))
contrasts(dat_rt_chinese$word) <- contr.sum(length(unique(dat_rt_chinese$word)))

# set contrasts for R
contrasts(dat_accuracy$study) <- contr.sum(3) # base = chinese
contrasts(dat_accuracy$stroop) <- contr.sum(2) # base = neutral
contrasts(dat_accuracy$trial_type) <- contr.sum(2) # base = repetition
contrasts(dat_accuracy$language) <- contr.sum(2) # base = english
contrasts(dat_accuracy$subject_id) <- contr.sum(length(unique(dat_accuracy$subject_id)))
contrasts(dat_accuracy$word_colour) <- contr.sum(length(unique(dat_accuracy$word_colour)))
