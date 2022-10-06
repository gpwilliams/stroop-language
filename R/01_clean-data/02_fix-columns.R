# note coalesce doesn't handle text well

demo_cleaned <- demo |> 
  rename(
    age = participant_age,
    gender = participant_gender,
    school_english = english_school,
    reading_english = english_reading,
    lex_tale_english = english_lex_tale
  ) |> 
  mutate(
    across(where(is.character), str_to_lower),
    age = parse_number(age),
    learning_english_pair = coalesce(learning_arabic, learning_chinese, learning_dutch),
    school_english_pair = coalesce(arabic_school, chinese_school, dutch_school),
    spoken_english_pair = coalesce(spoken_arabic, spoken_chinese, spoken_dutch),
    written_english_pair = coalesce(written_arabic, written_chinese, written_dutch),
    reading_english_pair = coalesce(arabic_reading, chinese_reading, dutch_reading),
    current_percent_english_pair = coalesce(current_percent_arabic, current_percent_chinese, current_percent_dutch),
    childhood_percent_english_pair = coalesce(childhood_percent_arabic, childhood_percent_chinese, childhood_percent_dutch),
    lex_tale_english_pair = coalesce(arabic_lex_tale, chinese_lex_tale, dutch_lex_tale)
  ) |> 
  select(
    study,
    subject_id,
    age,
    gender,
    contains("english")
  )
