map_files_to_list <- function(
  file_path, 
  file_type = ".rds", 
  pattern = "*"
  ) {
  
  # get full path
  full_file_path <- list.files(
    file_path,
    full.names = TRUE,
    pattern = paste0(pattern, file_type)
  )
  
  if(file_type == ".rds") {
    read_function <- readr::read_rds
  } 
  if(file_type == ".csv") {
    read_function <- readr::read_csv
  }
  
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