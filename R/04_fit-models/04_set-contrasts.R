# set contrasts ----

# set contrasts for R
for(i in seq_along(dat_rt$data)) {
  contrasts(dat_rt$data[[i]]$stroop) <- contr.sum(2) # base = neutral
  contrasts(dat_rt$data[[i]]$trial_type) <- contr.sum(2) # base = repetition
  contrasts(dat_rt$data[[i]]$language) <- contr.sum(2) # base = english
  contrasts(dat_rt$data[[i]]$subject_id) <- contr.sum(length(unique(dat_rt$data[[i]]$subject_id)))
  contrasts(dat_rt$data[[i]]$word_colour) <- contr.sum(length(unique(dat_rt$data[[i]]$word_colour)))
}

# set contrasts for R
for(i in seq_along(dat_accuracy$data)) {
  contrasts(dat_accuracy$data[[i]]$stroop) <- contr.sum(2) # base = neutral
  contrasts(dat_accuracy$data[[i]]$trial_type) <- contr.sum(2) # base = repetition
  contrasts(dat_accuracy$data[[i]]$language) <- contr.sum(2) # base = english
  contrasts(dat_accuracy$data[[i]]$subject_id) <- contr.sum(length(unique(dat_accuracy$data[[i]]$subject_id)))
  contrasts(dat_accuracy$data[[i]]$word_colour) <- contr.sum(length(unique(dat_accuracy$data[[i]]$word_colour)))
}
