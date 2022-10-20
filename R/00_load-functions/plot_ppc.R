plot_ppc <- function(
    files, 
    prior_titles = NULL, 
    title = NULL, 
    axis_label = NULL, 
    subtitle = paste(
      "Dark lines represent the observed data,",
      "while light lines represent draws from the model."
    )
  ) {
  prior_ppc <- list()
  file_names <- tools::file_path_sans_ext(basename(files))
  model_title <- file_names |> 
    str_replace_all("_", " ") |> 
    str_to_title() |> 
    str_replace_all("Rt", "RT")
  
  for(i in seq_along(files)) {
    message(glue::glue("Making PPC {i} of {length(files)}"))
    mod <- read_rds(files[i])
    prior_ppc[[i]] <- pp_check(mod, ndraws = 40) +
      labs(
        subtitle = str_wrap(
          glue::glue(
            model_title[i],
            prior_titles[['title']][i],
            .sep = ": "
          ), 
          width = 50
        ),
        x = axis_label
      ) +
      theme(
        plot.subtitle = element_text(size = 12),
        legend.position = "none"
      )
  }
  names(prior_ppc) <- file_names
  
  patchwork::wrap_plots(prior_ppc) +
    patchwork::plot_annotation(
      title = title,
      caption = subtitle
    ) +
    theme(
      plot.title = element_text(size = 36),
      plot.caption = element_text(size = 24)
    )
}
