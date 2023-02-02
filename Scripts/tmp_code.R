excluded_variables <- # Variables to remove from training data set
  c(
    "saturation_rest",
    "saturation_end",
    "saturation_delta",
    "study_name",
    "test",
    "data_type",
    "type",
    "activity",
    "altitude",
    "environment",
    "intensity"
  )

## Put labels in separated data frames
my_labels <-
  select(.data = my_summary, all_of(c("subject", "eih", "eih_severity")))
my_summary <-
  select(.data = my_summary, -all_of(c("eih", "eih_severity")))
## Labeling categorical variables
my_label_env_sex <- LabelEncoder.fit(my_summary$sex)
my_summary$sex <- transform(my_label_env_sex, my_summary$sex)
my_label_env_sport <- LabelEncoder.fit(my_summary$sport)
my_summary$sport <- transform(my_label_env_sport, my_summary$sport)
