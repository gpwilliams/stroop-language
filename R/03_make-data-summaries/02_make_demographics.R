underage_counts <- demo |> 
  group_by(study) |> 
  summarise(n_under_eighteen = sum(age < 18, na.rm = TRUE))

na_counts <- demo |>
  group_by(study) |> 
  summarise(across(
    .cols = c(
      "age", 
      "current_percent_english", 
      "current_percent_english_pair",
      "lex_tale_english", 
      "lex_tale_english_pair"
    ),
    \(x) sum(is.na(x))
  )) |> 
  pivot_longer(!study)

gender_counts <- demo |> 
  mutate(gender = factor(gender)) |> 
  group_by(study) |>
  count(gender, .drop = FALSE)

demo_summary <- demo |> 
  filter(age >= 18) |> 
  drop_na(
    age, 
    current_percent_english, 
    current_percent_english_pair, 
    lex_tale_english, 
    lex_tale_english_pair
  ) |> 
  group_by(study) |> 
  summarise(across(
    .cols = c(
      "age", 
      "current_percent_english", 
      "current_percent_english_pair",
      "lex_tale_english", 
      "lex_tale_english_pair"
    ),
    list(mean = mean, sd = sd, min = min, max = max)
  )) |> 
  pivot_longer(!study) |>
  extract(
    name,
    into = c("parameter", "statistic"),
    "(.*)_([^_]+)$"
  ) |> 
  mutate(across(where(is.numeric), round, 3))
