# make aggregated data for ANOVAs ----

agg_rt <- dat_rt |> 
  mutate(
    data = map(
      data,
      ~mutate(
        .x |> 
          group_by(subject_id, stroop, language) |> 
          summarise(
            mean_log_rt = mean(log_rt)
          )
      )
    )
  )

agg_accuracy <- dat_accuracy |> 
  mutate(
    data = map(
      data,
      ~mutate(
        .x |> 
          group_by(subject_id, stroop, language) |> 
          summarise(
            mean_correct = mean(correct)
          )
      )
    )
  )
