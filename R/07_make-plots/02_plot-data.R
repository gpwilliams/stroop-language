by_subj_rt <- dat_rt |> 
  group_by(study, subject_id, stroop, trial_type, language) |> 
  summarise(
    m = mean(log_rt), 
    sd = sd(log_rt),
    n = length(unique(word_colour))
  ) |> 
  nest(data = !study)


plot_rt <- ggplot(by_subj_rt, aes(x = stroop, y = m, fill = trial_type, colour = trial_type)) +
  ggdist::stat_halfeye(
    width = .6,
    justification = -.25,
    # remove slab interval
    .width = 0,
    point_colour = NA
  ) +
  facet_wrap(~ language) +
  geom_point(
    size = 1.3,
    alpha = .3,
    position = position_jitter(seed = 1, width = .1, height = .01)
  ) +
  geom_pointrange(
    data = emms, 
    aes(y = Probability, ymin = CI_low, ymax = CI_high),
    position = position_nudge(x = 0.14),
    size = 1.5, 
    fatten = 2,
    colour = "grey20"
  ) +
  geom_text(
    data = emms,
    aes(
      x = emotion, 
      y = Probability, 
      label = sub("^0+", "", round(emms$Probability, 2))
    ),
    colour = val_colours,
    size = 5,
    fontface = "bold",
    position = position_nudge(x = .25)
  ) +
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2), 
    labels = c("0", sub("^0+", "", seq(0.2, 1, by = 0.2)))
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 1.5),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(colour = colours, hjust = 0, vjust = 5),
    text=element_text(size=16,  family="Lato"),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))
  ) +
  labs(
    x = NULL,
    y = "Proportion of Up Clicks",
    caption = paste0(
      "Points and densities represent by-participant mean proportions.\n",
      "Pointranges represent estimated marginal means and 95% CIs."
    )
  )