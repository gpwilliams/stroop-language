make_plot <- function(by_subj_dat, emm_dat, colours, val_colours, breaks, y_label, nudge_x = .28, text_vjust = 5) {
  ggplot(by_subj_dat, aes(x = conditions, y = m, fill = conditions, colour = conditions)) +
    ggdist::stat_halfeye(
      width = .6,
      justification = -.25,
      # remove slab interval
      .width = 0,
      point_colour = NA
    ) +
    facet_wrap(~language) +
    geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(seed = 1, width = .1, height = .01)
    ) +
    geom_pointrange(
      data = emm_dat, 
      aes(y = emmean, ymin = asymp.LCL, ymax = asymp.UCL),
      position = position_nudge(x = 0.14),
      size = 1.5, 
      fatten = 2,
      colour = val_colours
    ) +
    geom_text(
      data = emm_dat,
      aes(
        x = conditions, 
        y = emmean, 
        label = sub("^0+", "", round(emmean, 2))
      ),
      colour = val_colours,
      size = 3.5,
      fontface = "bold",
      position = position_nudge(x = nudge_x),
      angle = 270
    ) +
    scale_fill_manual(values = colours) +
    scale_colour_manual(values = colours) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 14)) +
    scale_y_continuous(
      breaks = breaks, 
      labels = breaks
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 14,  family = "Lato"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(size = 1.5),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 14),
      axis.text.x = element_text(colour = colours, hjust = 0.2, vjust = 5),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      strip.text = element_text(size = 18)
    ) +
    coord_cartesian(ylim = c(min(breaks), max(breaks))) +
    labs(
      x = NULL,
      y = y_label,
      caption = paste0(
        "Points and densities represent by-participant means.\n",
        "Pointranges represent estimated marginal means and 95% CIs."
      )
    )
}

colours <- c(
  met.brewer("Pissaro")[3:2],
  met.brewer("Pissaro")[5:4]
)

val_colours <- c(
  "#2a708a", # 4
  "#284653", # 3
  "#2a708a", # 8
  "#284653", # 7
  "#26412e", # 2
  "#405c4f", # 1
  "#26412e", # 6
  "#405c4f" # 5
)

rt_breaks <- seq(700, 1900, by = 100)
rt_label <- "Reaction Time [ms]"

# Reaction Time ----

# arabic
arabic_rt_plot <- make_plot(
  by_subj_rt |> filter(study == "arabic"), 
  emms_rt |>filter(study == "arabic"), 
  colours, 
  val_colours,
  rt_breaks,
  rt_label
)

# dutch
dutch_rt_plot <- make_plot(
  by_subj_rt |> filter(study == "dutch"), 
  emms_rt |>filter(study == "dutch"), 
  colours, 
  val_colours,
  rt_breaks,
  rt_label
)

# chinese
chinese_rt_plot <- make_plot(
  by_subj_rt |> filter(study == "chinese"), 
  emms_rt |>filter(study == "chinese"), 
  colours, 
  val_colours,
  rt_breaks,
  rt_label
)

# Accuracy ----

accuracy_breaks <- seq(0, 1, by = 0.1)
accuracy_label <- "Proportion of Correct Responses"

# arabic
arabic_accuracy_plot <- make_plot(
  by_subj_accuracy |> filter(study == "arabic"), 
  emms_accuracy |>filter(study == "arabic"), 
  colours, 
  val_colours,
  accuracy_breaks,
  accuracy_label,
  nudge_x = .28,
  text_vjust = 0
)

# dutch
dutch_accuracy_plot <- make_plot(
  by_subj_accuracy |> filter(study == "dutch"), 
  emms_accuracy |>filter(study == "dutch"), 
  colours, 
  val_colours,
  accuracy_breaks,
  accuracy_label,
  nudge_x = .28,
  text_vjust = 0
)

# chinese
chinese_accuracy_plot <- make_plot(
  by_subj_accuracy |> filter(study == "chinese"), 
  emms_accuracy |>filter(study == "chinese"), 
  colours, 
  val_colours,
  accuracy_breaks,
  accuracy_label,
  nudge_x = .28,
  text_vjust = 0
)

# patchwork ----

dutch_patchwork <- dutch_rt_plot + 
  labs(caption = NULL) + 
dutch_accuracy_plot

arabic_patchwork <- arabic_rt_plot + 
  labs(caption = NULL) + 
  arabic_accuracy_plot

chinese_patchwork <- chinese_rt_plot + 
  labs(caption = NULL) + 
  chinese_accuracy_plot
  
# save ----
# rt
ggsave(
  here("03_plots", "03_descriptive", "dutch_rt.png"),
  dutch_rt_plot,
  height = 8,
  width = 12
)

ggsave(
  here("03_plots", "03_descriptive", "arabic_rt.png"),
  arabic_rt_plot,
  height = 8,
  width = 12
)

ggsave(
  here("03_plots", "03_descriptive", "chinese_rt.png"),
  chinese_rt_plot,
  height = 8,
  width = 12
)

# accuracy
ggsave(
  here("03_plots", "03_descriptive", "dutch_accuracy.png"),
  dutch_accuracy_plot,
  height = 8,
  width = 12
)

ggsave(
  here("03_plots", "03_descriptive", "arabic_accuracy.png"),
  arabic_accuracy_plot,
  height = 8,
  width = 12
)

ggsave(
  here("03_plots", "03_descriptive", "chinese_accuracy.png"),
  chinese_accuracy_plot,
  height = 8,
  width = 12
)

# patchwork
ggsave(
  here("03_plots", "03_descriptive", "dutch_patchwork.png"),
  dutch_patchwork,
  height = 8,
  width = 24
)

ggsave(
  here("03_plots", "03_descriptive", "arabic_patchwork.png"),
  arabic_patchwork,
  height = 8,
  width = 24
)

ggsave(
  here("03_plots", "03_descriptive", "chinese_patchwork.png"),
  chinese_patchwork,
  height = 8,
  width = 24
)

