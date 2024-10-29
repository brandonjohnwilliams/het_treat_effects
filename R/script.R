##############################################
# Author: Brandon Williams
# Date: 10/29/2024
# Description: 
#   Heterogeneous Treatment Effects in "Edu-
#   cation, HIV, and Early Fertility" (2015)
#
##############################################

# newest version of GenericML not yet on CRAN so install from here (instead of include.R)
# devtools::install_github("mwelz/GenericML")
library("GenericML")

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

# Define global variable list
varlist <- c("presence", "evmar05v3", "evpreg05v3", "evpregunmar05v3", "evunpregmar05v3", 
             "dropout07v2", "evmar07v2", "evpreg07v2", "evpregunmar07v2", "evunpregmar07v2")


# Filter data for each gender and control group
fem_cont <- studysample %>% filter(sex == "2 Female",  group03v1 == "C")

# Obtain control averages for girls
mean_fem_dropout <- mean(fem_cont$dropout05v3, na.rm = TRUE)
mean_fem_evmar <- mean(fem_cont$evmar05v3, na.rm = TRUE)
mean_fem_presence <- mean(fem_cont$presence, na.rm = TRUE)

#### Pickup here: finish regression specification of Table 2 ####














# pre-process
df <- df %>% 
  data.matrix()

# Define global variables
treatmentdummies <- c("Uonly", "Honly", "UH")
treatmentdummiesCT2 <- c("Uonly", "HnoCT", "UHnoCT", "HwithCT", "UHwithCT")
controlsR <- c("yrbirth_all", "yrbirth_missing", "date05v3", "date07v2", "schsize", "stratum")
controls <- c("yrbirth_all", "yrbirth_missing", "schsize", "stratum")
controlsKAP <- c("age", "agemissing", "schsize", "stratum")
controlsB <- c("yrbirth_all", "yrbirth_missing")

# specify learners
learners <-
c("random_forest",                                                # Random Forest  
    "mlr3::lrn('cv_glmnet', s = 'lambda.min', alpha = 0.5)",      # Elastic Net Regularization on generalized linear model
    "mlr3::lrn('svm')",                                           # Support vector machine
    "mlr3::lrn('xgboost')")                                       # Gradient Boost

# GenericML

genML <- GenericML(
  Z = df,                                   # matrix
  D = Utreat,                               # treatment assignment 
  Y = Y,                      # outcome response variable
  learners_GenericML = learners,            # learners
  learner_propensity_score = "constant",    # = 0.5 (RCT)
  num_splits = 100L,                        # number splits
  quantile_cutoffs = c(0.2, 0.4, 0.6, 0.8), # grouping
  significance_level = 0.05,                # significance level
  X1_BLP = X1, X1_GATES = X1,               # regression setup
  vcov_BLP = vcov, vcov_GATES = vcov,       # covariance setup
  parallel = TRUE, num_cores = 6L,          # parallelization
  seed = 20220621)                          # RNG seed
