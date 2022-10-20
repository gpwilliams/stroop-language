# get prior titles ----

# get prior labels for each model; repeat x 3 (1 for each study)
rt_prior_titles <- map_dfr(seq_len(3), ~make_prior_titles(rt_priors))
accuracy_prior_titles <- map_dfr(seq_len(3), ~make_prior_titles(accuracy_priors))
