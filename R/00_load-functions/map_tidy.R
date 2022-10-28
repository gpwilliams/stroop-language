map_tidy <- function(x, type = "ANOVA") {
  if(type == "ANOVA") {
    table <- "anova_table"
  } else {
    table <- "full_model"
  }
  if(type == "ANOVA") {
    for(i in seq_along(x)) {
      suppressWarnings(
        x[[i]] <- x[[i]][[table]] |>
          janitor::clean_names() |> 
          broom::tidy() |> 
          as.data.frame() 
      )
    }
  } else {
    for(i in seq_along(x)) {
      suppressWarnings(
        x[[i]] <- x[[i]][[table]] |>
          broom::tidy() |> 
          as.data.frame() 
      )
    }
  }
  x <- bind_rows(x, .id = "study")
  colnames(x)[grepl('pr|p.value',colnames(x))] <- 'p_value'
  x
}