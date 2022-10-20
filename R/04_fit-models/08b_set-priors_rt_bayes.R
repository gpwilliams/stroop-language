# set priors ----

# The grand mean in Liu et al. (2019) is 1036.75ms (approx 6.9 on log scale)
# this is our intercept with a range of 800 - 1200ms normal(6.9, 0.1)
# Liu et al. found strong effects of language (partial eta squared of 0.4)
# the biggest difference between languages was around 100ms
# this is our prior of normal(0, 0.05): -220ms to 220ms.
# we assume an effect at least this big for stroop incongruent vs. neutral trials
# while Liu found effects of Stroop half as big as those for language,
# we anticipate equally large effects. Trial type wasn't calculated in Liu,
# so again we default to the wider prior for language.
# All interactions as in Liu are at least half the size of the main effects.

# The main model is the one with prior_3. We then vary scales around this.
# vary main effects from 0.01, 0.025, 0.05, 0.075, 0.1
# vary interactions from 0.005, 0.01, 0.025, 0.05, 0.075

rt_priors <- list(
  prior_1 = c(
    prior(normal(6.9, 0.1), class = Intercept),
    prior(normal(0, 0.01), class = b, coef = "stroop1"),
    prior(normal(0, 0.01), class = b, coef = "trial_type1"),
    prior(normal(0, 0.01), class = b, coef = "language1"),
    prior(normal(0, 0.005), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.005), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.005), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.005), class = b, coef = "stroop1:trial_type1:language1"),
    prior(normal(0, 0.1), class = sd),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.15), class = sigma)
  ),
  prior_2 = c(
    prior(normal(6.9, 0.1), class = Intercept),
    prior(normal(0, 0.025), class = b, coef = "stroop1"),
    prior(normal(0, 0.025), class = b, coef = "trial_type1"),
    prior(normal(0, 0.025), class = b, coef = "language1"),
    prior(normal(0, 0.01), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.01), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.01), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.01), class = b, coef = "stroop1:trial_type1:language1"),
    prior(normal(0, 0.1), class = sd),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.15), class = sigma)
  ),
  prior_3 = c( # target (used in reporting)
    prior(normal(6.9, 0.1), class = Intercept),
    prior(normal(0, 0.05), class = b, coef = "stroop1"),
    prior(normal(0, 0.05), class = b, coef = "trial_type1"),
    prior(normal(0, 0.05), class = b, coef = "language1"),
    prior(normal(0, 0.025), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.025), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.025), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.025), class = b, coef = "stroop1:trial_type1:language1"),
    prior(normal(0, 0.1), class = sd),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.15), class = sigma)
  ),
  prior_4 = c(
    prior(normal(6.9, 0.1), class = Intercept),
    prior(normal(0, 0.075), class = b, coef = "stroop1"),
    prior(normal(0, 0.075), class = b, coef = "trial_type1"),
    prior(normal(0, 0.075), class = b, coef = "language1"),
    prior(normal(0, 0.05), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.05), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.05), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.05), class = b, coef = "stroop1:trial_type1:language1"),
    prior(normal(0, 0.1), class = sd),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.15), class = sigma)
  ),
  prior_5 = c(
    prior(normal(6.9, 0.1), class = Intercept),
    prior(normal(0, 0.1), class = b, coef = "stroop1"),
    prior(normal(0, 0.1), class = b, coef = "trial_type1"),
    prior(normal(0, 0.1), class = b, coef = "language1"),
    prior(normal(0, 0.075), class = b, coef = "stroop1:trial_type1"),
    prior(normal(0, 0.075), class = b, coef = "stroop1:language1"),
    prior(normal(0, 0.075), class = b, coef = "trial_type1:language1"),
    prior(normal(0, 0.075), class = b, coef = "stroop1:trial_type1:language1"),
    prior(normal(0, 0.1), class = sd),
    prior(lkj(2), class = cor),
    prior(normal(0, 0.15), class = sigma)
  )
)
