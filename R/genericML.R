##############################################
# Author: Brandon Williams
# Date: 10/29/2024
# Description: 
#   Heterogeneous Treatment Effects in "Edu-
#   cation, HIV, and Early Fertility" (2015)
#
##############################################


# load data
studysample <- read.csv(here("data/studysampleclean.csv"))

# Define global variable list
varlist <- c("presence", "evmar05v3", "evpreg05v3", "evpregunmar05v3", "evunpregmar05v3", "dropout05v3",
             "dropout07v2", "evmar07v2", "evpreg07v2", "evpregunmar07v2", "evunpregmar07v2")
treatlist <- c("Uonly", "Honly","UH")
controllist <- c("yrbirth_all", "yrbirth_missing", "date05v3", "date07v2", "schsize", "stratum")

#### Machine Learning ####

# newest version of GenericML not yet on CRAN so install from here (instead of include.R)
# devtools::install_github("mwelz/GenericML")
library("GenericML")

# pre-process
sampleselect <- studysample %>%             
  select(all_of(varlist),                   # select relevant columns
         all_of(controllist), 
         all_of(treatlist)) %>% 
  filter(complete.cases(.))                 # remove NAs

samplematrix <- data.matrix(sampleselect)                         # we need it in matrix form
Uonly <- sampleselect$Uonly                                       # We need D and Y in vector form
dropout05v3 <- sampleselect$dropout05v3

# specify learners
learners <-
  c("random_forest",                                              # Random Forest  
    "mlr3::lrn('cv_glmnet', s = 'lambda.min', alpha = 0.5)",      # Elastic Net Regularization on generalized linear model
    "mlr3::lrn('svm')",                                           # Support vector machine
    "mlr3::lrn('xgboost')")                                       # Gradient Boost

# Parameterize
seed <- 1833
cutoffs <- c(0.2, 0.4, 0.6, 0.8)
X1 <- setup_X1(funs_Z = c("B", "S")                    # include BCA and CATE controls
               # , fixed_effects = 
               )
vcov <- setup_vcov(
                   # estimator = "vcovCL"              # I was not able to get this to take the vcovCL argument
                   # , arguments = list(cluster = )    # what are we meant to cluster on?
                   )        
lps <- c("constant")
X1 <- setup_X1(funs_Z = c("B", "S")) 
num_splits <- 100L
sig_level <- 0.05
parallel <- TRUE
num_cores <- 6L

# GenericML

genML <- GenericML(
  Z = samplematrix,                         # matrix
  D = Uonly,                                # treatment assignment 
  Y = dropout05v3,                          # outcome response variable
  learners_GenericML = learners,            # learners
  learner_propensity_score = lps,           # = 0.5 (RCT)
  num_splits = num_splits,                  # number splits, L is useful for specifying integer and therefore optimizing memory
  quantile_cutoffs = cutoffs,               # grouping
  significance_level = sig_level,           # significance level
  X1_BLP = X1, X1_GATES = X1,               # regression setup
  vcov_BLP = vcov, vcov_GATES = vcov,       # covariance setup
  parallel = parallel,                      # parallelization
  # num_cores = num_cores, 
  seed = seed                               # RNG seed
)

## Interestingly, I'm getting propensity scores that would indicate a non-RCT

# BLP
results_BLP <- get_BLP(genML, plot = TRUE)
results_BLP       # print method
plot(results_BLP) # plot method

# GATES
results_GATES <- get_GATES(genML, plot = TRUE)
results_GATES
plot(results_GATES)

# CLAN
# results_CLAN <- get_CLAN(genML, variable = "head_age_bl", plot = TRUE)
# results_CLAN
# plot(results_CLAN)

# best learners
get_best(genML)


