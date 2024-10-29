
##############################################
# Author: Brandon Williams
# Date: 10/29/2024
# Description: 
#   Simply run this file first before using 
#   other R scripts
#
##############################################

# set wd to this file path of this source file in order to successfully execute 'here' package. For example:
setwd("C:/Users/BJW95/Documents/GitHub/het_treat_effects")

# installs the librarian package if you don't have it
if (!("librarian" %in% rownames(utils::installed.packages()))) {
  utils::install.packages("librarian")
}

# put all of the packages that you import here
librarian::shelf( 
  cran_repo = "https://cran.microsoft.com/", # Dallas, TX
  ask = FALSE,
  stats, 
  here,
  tidyverse,
  foreign,
  haven,
  ranger, 
  glmnet, 
  e1071,
  xgboost, 
  devtools,
  broom,               # tidy tibbles for lm
  sandwich,            # robust standard errors
  lmtest,
  openxlsx
)

# tell here where we are so we can use it elsewhere
here::i_am("R/include.R")
