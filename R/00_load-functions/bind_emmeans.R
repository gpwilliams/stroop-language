bind_emmeans <- function(
    .data, 
    .conditions = c("stroop", "language", "trial_type"), 
    .iter_id = c("study", "response", "analysis")
) {
  .vars <- syms(.conditions)
  
  .data <- .data |> 
    map(~ as_tibble(.x)) |> 
    bind_rows(.id = ".xx") |> 
    separate(col = .xx, into = .iter_id, sep = "_")
  
  # check if it's a contrast
  if("contrast" %in% colnames(.data)) {
    .data
  } else {
    .data |> 
      unite("condition", !!!.vars, na.rm = TRUE, sep = " ")
  }
}