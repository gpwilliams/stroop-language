read_csvs <- function(.file_path, .skip_first = TRUE, .id = "study") {
  file_names <- list.files(path = .file_path)
  
  # decide if first column should be dropped or not
  if(.skip_first == TRUE) {
    skip_col <- 1
  } else {
    skip_col <- NULL
  }
  
  # load all files in folder ending in .csv
  dat <- list.files(
    path = .file_path, 
    full.names = TRUE,
    pattern = "*.csv"
  ) |> 
    map(~read_csv(
      ., 
      show_col_types = FALSE, 
      col_select = !skip_col
    ))
  
  # set names
  names(dat) <- sub("\\.csv$", "", file_names) 
  bind_rows(dat, .id = .id)
}
