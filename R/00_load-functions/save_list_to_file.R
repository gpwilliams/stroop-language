save_list_to_file <- function(list = NULL, path = NULL, csv = TRUE, rds = TRUE, png = FALSE){
  #' Save a list of data frames to .csv, .rds, or both. 
  #' Inherits names from the list as file names.
  #' @return files saved to disc as .csv or .rds from a list.
  #' @param list list: list of data frames.
  #' @param path vector: a vector of strings indicating the (sub) folder to
  #' save the file using the here::here() package.
  #' @param csv logical: whether or not to save as a series of csv files.
  #' @param rds logical: whether or not to save as a series of rds files.
  #'  @param png logical: whether or not to save as a series of png files.
  #' @examples
  #' save_list_to_file(my_list, path = c("folder", "subfolder"))

    if(csv == TRUE) {
      for(i in seq_along(list)) {
        write_csv(
          list[[i]], 
          here::here(
            strsplit(path, ","), 
            paste0(names(list)[i], ".csv")
          )
        )
      }
    }
      
    if(rds == TRUE) {
      for(i in seq_along(list)) {
        write_rds(
          list[[i]], 
          here::here(
            strsplit(path, ","), 
            paste0(names(list[i]), ".rds")
          )
        )
      }
    }
  
  if(png == TRUE) {
    for(i in seq_along(list)) {
      png(
        here::here(
        strsplit(path, ","), 
        paste0(names(list[i]), ".png")
        ),
        width = 1200, 
        height = 800
      )
      print(list[[i]])
      dev.off()
    }
  }    
}

