make_descriptives_sentence <- function(.data, .condition, .outcome = "emmean", .ci_lower = "asymp.LCL", .ci_upper = "asymp.UCL") {
  paste0(
    "(M = ",
    .data |> filter(condition == .condition) |> pull(.outcome),
    ", *SE* = ",
    .data |> filter(condition == .condition) |> pull(SE),
    ", ",
    "95% CI = [",
    .data |> filter(condition == .condition) |> pull(.ci_lower),
    ", ",
    .data |> filter(condition == .condition) |> pull(.ci_upper),
    "])"
  )
}

make_pairwise_sentence <- function(.data, .contrast, .outcome = "ratio") {
  .data_subset <- .data |> filter(contrast == .contrast)
  
  if(.outcome == "ratio") {
    paste0(
      "(Ratio = ",
      .data_subset |> pull(.outcome),
      ", *SE* = ",
      .data_subset |> pull(SE),
      ", ",
      "*t*(",
      .data_subset |> pull(df),
      ") = ",
      .data_subset |> pull(t.ratio),
      ", *p* = ",
      .data_subset |> pull(p_value),
      ")"
    )
  } else if (.outcome == "estimate") {
    paste0(
      "(Est. = ",
      .data_subset |> pull(.outcome),
      ", *SE* = ",
      .data_subset |> pull(SE),
      ", ",
      "*z* = ",
      .data_subset |> pull(z.ratio),
      ", *p* = ",
      .data_subset |> pull(p_value),
      ")"
    )
  } else if (.outcome == "odds.ratio") {
    paste0(
      "(O.R. = ",
      .data_subset |> pull(.outcome),
      ", *SE* = ",
      .data_subset |> pull(SE),
      ", ",
      "*z* = ",
      .data_subset |> pull(z.ratio),
      ", *p* = ",
      .data_subset |> pull(p_value),
      ")"
    )
  }
}

bind_parameters_rt <- function(.freq, .bayes, .study) {
  accuracy_freq <- .freq |>
    filter(Study == .study, effect == "fixed") |> 
    select(Parameter, estimate, std.error, statistic, df, p_value, p_inconsistent)
  
  accuracy_bayes <- .bayes |> 
    filter(Study == .study) |> 
    select(Parameter, BF_01, prior_sensitive)
  
  full_join(accuracy_freq, accuracy_bayes, by = "Parameter")
}


bind_parameters_accuracy <- function(.freq, .bayes, .study) {
  accuracy_freq <- .freq |>
    filter(Study == .study, effect == "fixed") |> 
    select(c(Parameter, estimate, std.error, statistic, p_value, p_inconsistent))
  
  accuracy_bayes <- .bayes |> 
    filter(Study == .study) |> 
    select(Parameter, BF_01, prior_sensitive)
  
  full_join(accuracy_freq, accuracy_bayes, by = "Parameter")
}

apa_gt <- function(x) {
  gt(x) |> 
    tab_options(
      table.border.top.color = "white",
      heading.title.font.size = px(16),
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = 3,
      column_labels.border.bottom.color = "black",
      table_body.border.bottom.color = "black",
      table.border.bottom.color = "white",
      table.width = pct(100),
      table.background.color = "white"
    ) |> 
    cols_align(align="center") |> 
    tab_style(
      style = list(
        cell_borders(
          sides = c("top", "bottom"),
          color = "white",
          weight = px(1)
        ),
        cell_text(
          align="center"
        ),
        cell_fill(color = "white", alpha = NULL)
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      )
    )
}

parameters_rt_gt <- function(x) {
  apa_gt(x) |> 
    cols_label(
      estimate = html("&beta;"),
      std.error = md("*SE*"),
      statistic = md("*t*"),
      p_value = md("*p*"),
      BF_01 = html("BF<sub>01</sub>")
    ) |> 
    tab_footnote(
      footnote = md(
        "Indicates *p*-values where conclusion differ between the mixed effects model and by-participants ANOVA."
      ),
      locations = cells_body(
        columns = p_value,
        rows = p_inconsistent
      )
    ) |> 
    tab_footnote(
      footnote = md(
        "Indicates Bayes factors where conclusions are sensitive to prior specification."
      ),
      locations = cells_body(
        columns = BF_01,
        rows = prior_sensitive
      )
    ) |> 
    cols_hide(columns = c(prior_sensitive, p_inconsistent))
}

parameters_accuracy_gt <- function(x) {
  apa_gt(x) |> 
    cols_label(
      estimate = html("&beta;"),
      std.error = md("*SE*"),
      statistic = md("*z*"),
      p_value = md("*p*"),
      BF_01 = html("BF<sub>01</sub>")
    ) |> 
    tab_footnote(
      footnote = md(
        "Indicates *p*-values where conclusion differ between the mixed effects model and by-participants ANOVA."
      ),
      locations = cells_body(
        columns = p_value,
        rows = p_inconsistent
      )
    ) |> 
    tab_footnote(
      footnote = md(
        "Indicates Bayes factors where conclusions are sensitive to prior specification."
      ),
      locations = cells_body(
        columns = BF_01,
        rows = prior_sensitive
      )
    ) |> 
    cols_hide(columns = c(prior_sensitive, p_inconsistent))
}

report_participant_numbers <- function(.participant_numbers, .study) {
  .participant_numbers |> 
    filter(study == .study) |> 
    pull(original) |> 
    english() |> 
    str_to_sentence()
}

report_recording_failure <- function(.performance_exclusions, .study) {
  .performance_exclusions |> 
    filter(study == .study, reason == "recording failure") |> 
    summarise(n = n()) |> 
    pull(n) |> 
    english() |> 
    str_to_sentence()
}

report_performance_exclusions <- function(.performance_exclusions, .study) {
  .performance_exclusions |> 
    filter(study == .study, reason != "recording failure") |> 
    summarise(n = n()) |> 
    pull(n) |> 
    english()
}


report_under_age <- function(.additional_exclusions, .study) {
  .additional_exclusions |> 
    filter(study == .study, reason == "under age") |> 
    summarise(n = n()) |>
    pull(n) |> 
    english()
}

report_participant_counts <- function(.participant_numbers, .study) {
  
  total_n <- .participant_numbers |> 
    filter(study == .study) |> 
    pull(n) |> 
    sum()
  
  female_n <- .participant_numbers |> 
    filter(study == .study, gender == "female") |> 
    pull(n)
  
  male_n <- .participant_numbers |> 
    filter(study == .study, gender == "male") |> 
    pull(n)
  
  nonbinary_n <- .participant_numbers |> 
    filter(study == .study, gender == "non-binary") |> 
    pull(n)
  
  if(nonbinary_n == 0) {
    paste0(
      total_n, 
      " (",
      female_n,
      " female, ",
      male_n,
      " male)"
    )
  } else {
    paste0(
      total_n, 
      " (",
      female_n,
      " female, ",
      male_n,
      " male, ",
      nonbinary_n,
      " non-binary)"
    )
  }
}

report_participant_age <- function(.demo_summary, .study) {
  
  mean_age <- .demo_summary |> 
    filter(study == .study, parameter == "age", statistic == "mean") |> 
    pull(value)
  
  sd_age <- .demo_summary |> 
    filter(study == .study, parameter == "age", statistic == "sd") |> 
    pull(value)
  
  paste0(
    mean_age,
    " (*SD* = ",
    sd_age,
    ")"
  )
}

report_dropped_trial_numbers_accuracy <- function(.trial_numbers, .study) {
  .trial_numbers |> 
    filter(study == .study, measure == "percent_removed_accuracy") |> 
    pull() |> 
    round_pad()
}

report_dropped_trial_numbers_rt <- function(.trial_numbers, .study) {
  .trial_numbers |> 
    filter(study == .study, measure == "percent_removed_rt") |> 
    pull() |> 
    round_pad()
}