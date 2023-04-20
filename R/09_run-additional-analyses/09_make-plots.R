# sort data ----

emms_rt <- emmeans(
  rt_model_freq_all_updated, 
  ~ stroop * language * trial_type,
  type = "response"
  ) |>
  as_tibble() |> 
  rename(emmean = response)

emms_rt$stroop <- factor(
  emms_rt$stroop,
  levels = c("Neutral", "Incongruent")
)

emms_rt$trial_type <- factor(
  emms_rt$trial_type,
  levels = c("Repetition", "Switch")
)

emms_rt <- emms_rt |>
  mutate(conditions = interaction(stroop, trial_type, sep = " × "))

emms_accuracy <- emmeans(
  accuracy_model_freq_all$full_model, 
  ~ stroop * language * trial_type,
  type = "response"
) |>
  as_tibble() |> 
  rename(emmean = prob)

emms_accuracy$stroop <- factor(
  emms_accuracy$stroop,
  levels = c("Neutral", "Incongruent")
)

emms_accuracy$trial_type <- factor(
  emms_accuracy$trial_type,
  levels = c("Repetition", "Switch")
)

emms_accuracy <- emms_accuracy |> 
  mutate(conditions = interaction(stroop, trial_type, sep = " × "))

# aggregate data ----

by_subj_rt <- dat_rt |> 
  group_by(study, subject_id, stroop, trial_type, language) |> 
  summarise(
    m = mean(rt), 
    sd = sd(rt),
    n = length(unique(word_colour))
  ) |> 
  mutate(study = str_replace_all(study, "_stroop", ""))

by_subj_rt$stroop <- factor(
  by_subj_rt$stroop,
  levels = c("Neutral", "Incongruent")
)

by_subj_rt$trial_type <- factor(
  by_subj_rt$trial_type,
  levels = c("Repetition", "Switch")
)

by_subj_rt <- by_subj_rt |>
  mutate(conditions = interaction(stroop, trial_type, sep = " × "))


# accuracy ----

by_subj_accuracy <- dat_accuracy |> 
  group_by(study, subject_id, stroop, trial_type, language) |> 
  summarise(
    m = mean(correct), 
    sd = sd(correct),
    n = length(unique(word_colour))
  ) |> 
  mutate(study = str_replace_all(study, "_stroop", ""))

by_subj_accuracy$stroop <- factor(
  by_subj_accuracy$stroop,
  levels = c("Neutral", "Incongruent")
)

by_subj_accuracy$trial_type <- factor(
  by_subj_accuracy$trial_type,
  levels = c("Repetition", "Switch")
)

by_subj_accuracy <- by_subj_accuracy |>
  mutate(conditions = interaction(stroop, trial_type, sep = " × "))


# make plots ----

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
accuracy_breaks <- seq(0, 1, by = 0.1)
accuracy_label <- "Proportion of Correct Responses"

# rt
rt_plot <- make_plot(
  by_subj_rt, 
  emms_rt, 
  colours, 
  val_colours,
  rt_breaks,
  rt_label
)

# accuracy
accuracy_plot <- make_plot(
  by_subj_accuracy, 
  emms_accuracy, 
  colours, 
  val_colours,
  accuracy_breaks,
  accuracy_label,
  nudge_x = .28,
  text_vjust = 0
)

# save it ----

patchwork <- rt_plot + 
  labs(caption = NULL) + 
  accuracy_plot

ggsave(
  here("03_plots", "03_descriptive", "pooled_rt.png"),
  rt_plot,
  height = 8,
  width = 12
)
ggsave(
  here("03_plots", "03_descriptive", "pooled_accuracy.png"),
  accuracy_plot,
  height = 8,
  width = 12
)
ggsave(
  here("03_plots", "03_descriptive", "pooled_patchwork.png"),
  patchwork,
  height = 8,
  width = 24
)