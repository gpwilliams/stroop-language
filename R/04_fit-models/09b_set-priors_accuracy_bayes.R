# set priors ----

# In Liu et al. there's a very high grand mean accuracy of .93, this is
# qlogis(.93), or a logit of approximately 2.6
# Liu et al. found relatively strong block effects of around 2% difference
# this is qlogis(.93) - qlogis(.91), a logit of .27
# as with rection times we will assume this effect for all main effects,
# and one half as large for all interactions (.18)

# prior for main effects goes from: 0.09, 0.18, 0.27, 0.36, 0.45
# prior for interactions goes from: 0.045, 0.09, 0.18, 0.27, 0.36

accuracy_priors <- list(
  prior_1 = c(
    prior(normal(2.6, 0.2), class = Intercept),
    prior(normal(0, 0.09), class = b, coef = "stroop1"),
    prior(normal(0, 0.09), class = b, coef = "trial_type1"),
    prior(normal(0, 0.09), class = b, coef = "language1"),
    prior(normal(0, 0.045), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.045), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.045), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.045), class = b, coef = "stroop1:trial_type1:language1"),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.5), class = sd),
    prior(normal(0, 2), class = sd, coef = "Intercept", group = "subject_id")
  ),
  prior_2 = c(
    prior(normal(2.6, 0.2), class = Intercept),
    prior(normal(0, 0.18), class = b, coef = "stroop1"),
    prior(normal(0, 0.18), class = b, coef = "trial_type1"),
    prior(normal(0, 0.18), class = b, coef = "language1"),
    prior(normal(0, 0.09), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.09), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.09), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.09), class = b, coef = "stroop1:trial_type1:language1"),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.5), class = sd),
    prior(normal(0, 2), class = sd, coef = "Intercept", group = "subject_id")
  ),
  prior_3 = c( # main model
    prior(normal(2.6, 0.2), class = Intercept),
    prior(normal(0, 0.27), class = b, coef = "stroop1"),
    prior(normal(0, 0.27), class = b, coef = "trial_type1"),
    prior(normal(0, 0.27), class = b, coef = "language1"),
    prior(normal(0, 0.18), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.18), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.18), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.18), class = b, coef = "stroop1:trial_type1:language1"),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.5), class = sd),
    prior(normal(0, 2), class = sd, coef = "Intercept", group = "subject_id")
  ),
  prior_4 = c(
    prior(normal(2.6, 0.2), class = Intercept),
    prior(normal(0, 0.36), class = b, coef = "stroop1"),
    prior(normal(0, 0.36), class = b, coef = "trial_type1"),
    prior(normal(0, 0.36), class = b, coef = "language1"),
    prior(normal(0, 0.27), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.27), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.27), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.27), class = b, coef = "stroop1:trial_type1:language1"),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.5), class = sd),
    prior(normal(0, 2), class = sd, coef = "Intercept", group = "subject_id")
  ),
  prior_5 = c(
    prior(normal(2.6, 0.2), class = Intercept),
    prior(normal(0, 0.45), class = b, coef = "stroop1"),
    prior(normal(0, 0.45), class = b, coef = "trial_type1"),
    prior(normal(0, 0.45), class = b, coef = "language1"),
    prior(normal(0, 0.36), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.36), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.36), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.36), class = b, coef = "stroop1:trial_type1:language1"),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.5), class = sd),
    prior(normal(0, 2), class = sd, coef = "Intercept", group = "subject_id")
  )
)
