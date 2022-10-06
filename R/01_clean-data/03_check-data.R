# check unique subject IDs across studies ----

data_ids <- dat |> 
  group_by(study) |> 
  summarise(subject_id = length(unique(subject_id)))

# test we have enough unique IDs within each study as across them
testthat::expect_equal(
  sum(data_ids$subject_id), 
  length(unique(dat$subject_id))
)

# Check condition coding ----

## 1. language should have 2 labels that differ across studies on 1 label ----
#     (English, Arabic; English, Chinese; English, Dutch)

study_names <- unique(dat$study)

for(i in seq_along(study_names)) {
  study_data <- dat |> filter(study == study_names[i])
  
  if(study_names[i] == "arabic_stroop") {
    testthat::expect_setequal(
      unique(study_data$language),
      c("Arabic", "English")
    )
  } else if(study_names[i] == "chinese_stroop") {
    testthat::expect_setequal(
      unique(study_data$language),
      c("Chinese", "English")
    )
  } else if(study_names[i] == "dutch_stroop") {
    testthat::expect_setequal(
      unique(study_data$language),
      c("Dutch", "English")
    )
  } else {
    stop(
      paste(
        "Study names don't match expected.", 
        "(Perhaps file extension is included?)"
      )
    )
  }
}

## 2. accuracy should have 4 consistent labels across studies ----
#     (Correct, Incorrect Language, No/Other Sound, Incorrect Colour)

testthat::expect_setequal(
  unique(dat$accuracy),
  c("Correct", "Incorrect Language", "No/Other Sound", "Incorrect Colour")
)

## 3. stroop should have 2 consistent labels across studies ---- 
#     (Neutral, Incongruent)

testthat::expect_setequal(
  unique(dat$stroop),
  c("Neutral", "Incongruent")
)

## 4. block should have 2 consistent labels across studies ----
#     (Animals, Colours)

testthat::expect_setequal(
  unique(dat$block),
  c("Animals", "Colours")
)

## 5. trial_type should have 3 consistent labels across studies ----
#   (NA, Switch, Repetition)

testthat::expect_setequal(
  unique(dat$trial_type),
  c(NA_character_, "Switch", "Repetition")
)
