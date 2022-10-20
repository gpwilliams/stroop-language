# set names ----

dat_rt <- dat_rt |> 
  mutate(
    data = map(
      data,
      ~ mutate(
        .x,
        subject_id = as.factor(subject_id),
        word_colour = as.factor(word_colour),
        stroop = factor(
          stroop, 
          levels = c("Incongruent", "Neutral"),
        ),
        trial_type = factor(
          trial_type,
          levels = c("Switch", "Repetition"),
        ),
        language = factor(
          language,
          levels = rev(unique(.x[["language"]]))
        )
      )
    ))

# confirm second level of each language is English
testthat::expect_equal(
  levels(dat_rt$data[[1]]$language)[2], 
  levels(dat_rt$data[[2]]$language)[2],
  levels(dat_rt$data[[3]]$language)[2]
)

dat_accuracy <- dat_accuracy |> 
  mutate(
    data = map(
      data,
      ~ mutate(
        .x,
        subject_id = as.factor(subject_id),
        word_colour = as.factor(word_colour),
        stroop = factor(
          stroop, 
          levels = c("Incongruent", "Neutral"),
        ),
        trial_type = factor(
          trial_type,
          levels = c("Switch", "Repetition"),
        ),
        language = factor(
          language,
          levels = rev(unique(.x[["language"]]))
        )
      )
    ))

# confirm second level of each language is English
testthat::expect_equal(
  levels(dat_accuracy$data[[1]]$language)[2], 
  levels(dat_accuracy$data[[2]]$language)[2],
  levels(dat_accuracy$data[[3]]$language)[2]
)
