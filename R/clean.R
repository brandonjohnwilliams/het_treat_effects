##############################################
# Author: Brandon Williams
# Date: 10/29/2024
# Description: 
#   Cleaning of data files to match those 
#   used in original paper replication
#
##############################################

# load data
studysample <- read.dta(here("replication/replication_docs/datasets/studysample_allmerged.dta"))
school_info <- read.dta(here("replication/replication_docs/datasets/school_info.dta"))


# Process the school_info dataset
school_info <- school_info %>%
  select(schoolid, schsize, HIVtreat, Utreat) %>%
  mutate(
    Honly = as.integer((HIVtreat == 1) & (Utreat == 0)),
    Uonly = as.integer((HIVtreat == 0) & (Utreat == 1)),
    UH = as.integer((HIVtreat == 1) & (Utreat == 1))
  )

# Sort by schoolid and give new name for consistency
school_small <- school_info %>%
  arrange(schoolid)


# Process and merge the studysample_allmerged dataset

studysample <- studysample %>%
  arrange(schoolid) %>%
  left_join(school_small, by = "schoolid") 


# Process age and year of birth
studysample <- studysample %>%
  mutate(
    yrbirth = ifelse(yrbirth > 1992 & Q_a3_8_date_of_birth < yrbirth & LOG_surveyed == 1 & !is.na(Q_a3_8_date_of_birth), Q_a3_8_date_of_birth, yrbirth),
    yrbirth = ifelse(yrbirth < 1987 & Q_a3_8_date_of_birth > yrbirth & LOG_surveyed == 1 & !is.na(Q_a3_8_date_of_birth), Q_a3_8_date_of_birth, yrbirth),
    yrbirth = ifelse(is.na(yrbirth) & !is.na(Q_a3_8_date_of_birth), Q_a3_8_date_of_birth, yrbirth),
    yrbirth = ifelse(yrbirth < 1985, 1985, yrbirth),
    yrbirth = ifelse(yrbirth > 1992, 1992, yrbirth),
    age2009 = 2009 - yrbirth,
    age2009 = ifelse(age2009 < 16 | age2009 > 22 & Q_a3_8_date_of_birth > 1986 & Q_a3_8_date_of_birth < 1993 & !is.na(Q_a3_8_date_of_birth), 2009 - Q_a3_8_date_of_birth, age2009),
    age2003 = age2009 - 6,
    age_survey = Q_year - yrbirth
  )

# Now process age_survey_all and handle missing values
studysample <- studysample %>%
  mutate(age_survey_all = age_survey) %>%
  group_by(sex) %>%
  mutate(
    r = mean(age_survey, na.rm = TRUE),
    age_survey_all = ifelse(LOG_surveyed == 1 & dead == 0 & is.na(age_survey_all), r, age_survey_all)
  ) %>%
  ungroup() %>%
  select(-r) %>%
  mutate(
    yrbirth_missing = is.na(yrbirth),
    yrbirth_all = yrbirth
  ) %>%
  group_by(sex) %>%
  mutate(
    mean = round(mean(yrbirth_all, na.rm = TRUE), 1),
    yrbirth_all = ifelse(yrbirth_missing, mean, yrbirth_all)
  ) %>%
  ungroup() %>%
  select(-mean)


# Generate new variables based on conditions
studysample <- studysample %>%
  mutate(
    HnoCT = Honly * (1 - sampleCT103v1),
    UHnoCT = UH * (1 - sampleCT103v1),
    HwithCT = Honly * sampleCT103v1,
    UHwithCT = UH * sampleCT103v1
  )

# Loop through the visits to generate 'presence' variables
for (visit in c("04v1", "04v2", "05v1", "05v2", "05v3")) {
  studysample <- studysample %>%
    mutate(
      !!paste0("presence", visit) := case_when(
        get(paste0("pres", visit)) == 2 ~ 0.0,  # Make 0 a floating-point number
        get(paste0("pres", visit)) == 3 ~ 0.5,
        get(paste0("pres", visit)) > 3 ~ NA_real_,
        TRUE ~ as.numeric(get(paste0("pres", visit)))  # Explicitly cast to numeric
      )
    )
}

# Adjust dropout variable based on conditions
studysample <- studysample %>%
  mutate(
    dropout05v3 = case_when(
      evdead05v3 == 1 ~ NA_real_,                # If the student is deceased, set to NA
      presence05v3 == 1 ~ 0,                     # If the student was present, set to 0
      TRUE ~ dropout05v3                         # Otherwise, retain the original value
    )
  )

# Calculate mean presence across specified visits
studysample <- studysample %>%
  rowwise() %>%
  mutate(presence = mean(c_across(starts_with("presence")), na.rm = TRUE)) %>%
  ungroup()

# Process events based on conditions for specific dates
for (date in c("05v3", "07v2")) {
  studysample <- studysample %>%
    mutate(
      !!paste0("evmar", date) := ifelse(is.na(get(paste0("evmar", date))) & get(paste0("evpreg", date)) == 1, 0, get(paste0("evmar", date))),
      !!paste0("evunpregmar", date) := (1 - get(paste0("evpreg", date))) * get(paste0("evmar", date)),
      !!paste0("evpregunmar", date) := get(paste0("evpreg", date)) * (1 - get(paste0("evmar", date))),
      !!paste0("marifchild", date) := ifelse(get(paste0("evpreg", date)) == 1, get(paste0("evmar", date)), NA)
    )
}

# Convert gender to numeric
studysample <- studysample %>%
  mutate(sex = as.character(sex),        # Convert to character to allow string manipulation
         sex = sub(" .*", "", sex))  %>% # Remove everything after the first space
  mutate(sex = as.numeric(sex))    


# Export studysample dataframe to CSV
write.csv(studysample, here("data/studysampleclean.csv"), row.names = FALSE)

