map_fixef <- function(x) {
  extract_fixef <- function(x) {
    fixef(x) |> 
      as.data.frame() |> 
      rownames_to_column(.data = _, var = "parameter")
  }
  x |>  
    map(extract_fixef) |> 
    bind_rows(.id = "study")
}