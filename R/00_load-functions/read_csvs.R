read_csvs <- function(
    .file_path, 
    .skip_first = TRUE, 
    .id = "study", 
    .include = NULL, # string
    .exclude = NULL
    ) {
  #' Read all csvs in a directory into and bind together as a tibble. 
  #' Crete a column identifying the data source with a custom column label.
  #' Optionally skip the first column.
  #' Subset the files to those in .include only or not in .exclude.
  #' @return Tibble of merged csvs.
  #' @param .file_path string: The file path to the folder containing the csvs.
  #' @param .skip_first logical: Whether the first column should be excluded. 
  #' This is useful for cases where row numbers are included in the csv but not needed.
  #' @param .id string: the column name used to identify the data source (i.e. file).
  #' @param .include string/vector of strings or NULL: keep only files containing the string.
  #' @param .exclude string/vector of strings or NULL: remove any files containing the string.
  #' @examples
  #' read_csvs(here("path/to/file"), .exclude = "demographics")
  
  # decide if first column should be dropped or not
  if(.skip_first == TRUE) {
    skip_col <- 1
  } else {
    skip_col <- NULL
  }

 # get paths for all files in folder ending in .csv
 files <- list.files(
  path = .file_path, 
  full.names = TRUE,
  pattern = "*.csv"
  )
 # get their names
 file_names <- list.files(path = .file_path)
 
 # keep only things in include
 if(!is.null(.include)) {
   .include <- paste(.include, collapse = "|")
   files <- files[grepl(.include, files)]
   file_names <- file_names[grepl(.include, file_names)]
   }

 # remove things in exclude
 if(!is.null(.exclude)) {
   .exclude <- paste(.exclude, collapse = "|")
   files <- files[!grepl(.exclude, files)]
   file_names <- file_names[!grepl(.exclude, file_names)]
   }
 
  if(length(files) > 0) {
    suppressMessages(
      dat <- files |> 
        map(~read_csv(
          ., 
          col_types = cols(.default = "c"),
          show_col_types = FALSE, 
          col_select = !skip_col
        ))
    )
    # set names
    names(dat) <- sub("\\.csv$", "", file_names) 
    bind_rows(dat, .id = .id) |> 
      type_convert()
  } else {
    stop(
     paste(
       "No files match the inputted paramaters.",
       "Did you provide the same pattern to include and exclude?"
       )
    )
  }
}
