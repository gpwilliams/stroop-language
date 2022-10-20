map_files_to_list <- function(
  file_path, 
  file_type = ".rds", 
  pattern = "*",
  read_function = readr::read_rds
  ) {
  
  # get full path
  full_file_path <- list.files(
    file_path,
    full.names = TRUE,
    pattern = paste0(pattern, file_type)
  )
  # read files
  files <- purrr::map(full_file_path, read_function)
  # add names to list
  names(files) <- stringr::str_sub(
    list.files(
      file_path,
      pattern = paste0(pattern, file_type)
    ), 1, -5)
  # return named list of files
  files
}