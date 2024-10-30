##############################################
# Author: Brandon Williams
# Date: 10/29/2024
# Description: 
#   Heterogeneous Treatment Effects in "Edu-
#   cation, HIV, and Early Fertility" (2015)
#
##############################################

### First, let's recreate Table 2 and 3 results ###

# load data
studysample <- read.csv(here("data/studysampleclean.csv"))

# Define global variable list
varlist <- c("presence", "evmar05v3", "evpreg05v3", "evpregunmar05v3", "evunpregmar05v3", 
             "dropout07v2", "evmar07v2", "evpreg07v2", "evpregunmar07v2", "evunpregmar07v2")


# Fit regression model and store summary statistics

# Filter data for girls and control group
data_group_c <- studysample %>% filter(sex == 2, group03v1 == "C")
data_group <- studysample %>% filter(sex == 2)

# Summarize and store mean for dropout05v3
mean_dropout <- mean(data_group_c$dropout05v3, na.rm = TRUE)

# Fit regression model and store summary statistics
model <- lm(dropout05v3 ~ Uonly + Honly + UH + yrbirth_all + yrbirth_missing + date05v3 + date07v2 + schsize + stratum, 
            data = data_group)
coeftest_model <- coeftest(model, vcov = vcovCL, cluster = ~ sch03v1)
coeftest_model

# Export results to Excel
write.xlsx(list(coeftest_model = tidy(coeftest_model), mean = mean_dropout), 
           file = paste0("output/", "dropout","table2_", 2, ".xlsx"))

# Filter data for boys and control group
data_group_c <- studysample %>% filter(sex == 1, group03v1 == "C")
data_group <- studysample %>% filter(sex == 1)

# Summarize and store mean for dropout05v3
mean_dropout <- mean(data_group_c$dropout05v3, na.rm = TRUE)

# Fit regression model and store summary statistics
model <- lm(dropout05v3 ~ Uonly + Honly + UH + yrbirth_all + yrbirth_missing + date05v3 + date07v2 + schsize + stratum, 
            data = data_group)
coeftest_model <- coeftest(model, vcov = vcovCL, cluster = ~ sch03v1)
coeftest_model

# Export results to Excel
write.xlsx(list(coeftest_model = tidy(coeftest_model), mean = mean_dropout), 
           file = paste0("output/", "dropout","table2_", 1, ".xlsx"))

# Now let's do this in a loop and generate all Table 2 Specifications
# Set up a loop for each gender group
for (i in c(2, 1)) {
  
  # Filter data for each gender and control group
  data_group_c <- studysample %>% filter(sex == i, group03v1 == "C")
  data_group <- studysample %>% filter(sex == i)
  
  # Summarize and store mean for dropout05v3
  mean_dropout <- mean(data_group_c$dropout05v3, na.rm = TRUE)
  
  # Fit regression model and store summary statistics
  model <- lm(dropout05v3 ~ Uonly + Honly + UH + yrbirth_all + yrbirth_missing + date05v3 + date07v2 + schsize + stratum, 
              data = data_group)
  coeftest_model <- coeftest(model, vcov = vcovCL, cluster = ~ sch03v1)

  # Export results to Excel
  write.xlsx(list(coeftest_model = tidy(coeftest_model), mean = mean_dropout), 
             file = paste0("output/", "table2_", i, ".xlsx"))
  
  # Process each variable in varlist for the same operations
  for (var in varlist) {
    mean_var <- mean(data_group[[var]], na.rm = TRUE)
    
    if (!is.na(mean_var) && mean_var != 0) {
      model_var <- lm(as.formula(paste(var, "~ Uonly + Honly + UH + yrbirth_all + yrbirth_missing + date05v3 + date07v2 + schsize + stratum")), 
                      data = data_group)
      coeftest_var <- coeftest(model_var, vcov = vcovCL, cluster = ~ sch03v1)
      
      
    } else {
      p_values_var <- list(Uonly_UH = -99, Honly_UH = -99, Honly_Uonly = -99, UH_combination = -99)
    }
    
    # Append results to the Excel file
    write.xlsx(list(coeftest_var = tidy(coeftest_var), mean = mean_var), 
               file = paste0("output/", var, "table2_", i, ".xlsx"), append = TRUE)
  }
}

### Note that some of these values are not exactly the same as their table output. Why?


#### Machine Learning ####

# newest version of GenericML not yet on CRAN so install from here (instead of include.R)
# devtools::install_github("mwelz/GenericML")
library("GenericML")

url_data <-
  url(paste0(
    "https://github.com/mwelz/GenericML/blob/main/slides",
    "/data/morocco_preprocessed.Rdata?raw=true"
  ))
load(url_data)




# specify learners
learners <-
  c("random_forest",
    "mlr3::lrn('cv_glmnet', s = 'lambda.min', alpha = 0.5)",
    "mlr3::lrn('svm')",
    "mlr3::lrn('xgboost')")


# include BCA and CATE controls
# add fixed effects along variable "vil_pair"
X1 <- setup_X1(funs_Z = c("B", "S"),
               fixed_effects = vil_pair)


# calls functions from the "sandwich" package
# cluster standard errors along "demi_paire"
vcov <- setup_vcov(estimator = "vcovCL",
                   arguments = list(cluster = demi_paire))


# run GenericML()
# load("slides/replication/GenericML_object.Rdata") # uncomment if you want to load the object below
genML <- GenericML(
  Z = Z, D = D, Y = Y,                      # observed data
  learners_GenericML = learners,            # learners
  learner_propensity_score = "constant",    # = 0.5 (RCT)
  num_splits = 100L,                        # number splits
  quantile_cutoffs = c(0.2, 0.4, 0.6, 0.8), # grouping
  significance_level = 0.05,                # significance level
  X1_BLP = X1, X1_GATES = X1,               # regression setup
  vcov_BLP = vcov, vcov_GATES = vcov,       # covariance setup
  parallel = TRUE, num_cores = 6L,          # parallelization
  seed = 20220621)                          # RNG seed

# pre-process
samplematrix <- studysample %>% 
  data.matrix()
seed <- 1833

# specify learners
learners <-
  c("random_forest",                                              # Random Forest  
    "mlr3::lrn('cv_glmnet', s = 'lambda.min', alpha = 0.5)",      # Elastic Net Regularization on generalized linear model
    "mlr3::lrn('svm')",                                           # Support vector machine
    "mlr3::lrn('xgboost')")                                       # Gradient Boost

# Parameterize

cutoffs <- c(0.2, 0.4, 0.6, 0.8)
X1 <- c("B")
Uonly <- studysample$Uonly
dropout05v3 <- studysample$dropout05v3
# GenericML

genML <- GenericML(
  Z = samplematrix,                         # matrix
  D = Uonly,                                # treatment assignment 
  Y = dropout05v3,                          # outcome response variable
  learners_GenericML = learners,            # learners
  learner_propensity_score = "constant",    # = 0.5 (RCT)
  num_splits = 100L,                        # number splits
  quantile_cutoffs = cutoffs,               # grouping
  significance_level = 0.05,                # significance level
  X1_BLP = X1, X1_GATES = X1,               # regression setup
  vcov_BLP = vcov, vcov_GATES = vcov,       # covariance setup
  parallel = TRUE, num_cores = 6L,          # parallelization
  seed = seed)                              # RNG seed
