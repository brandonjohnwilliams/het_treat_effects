##############################################
# Author: Brandon Williams
# Date: 10/29/2024
# Description: 
#   Replication of Tables 2 and 3 from
#   Heterogeneous Treatment Effects in "Edu-
#   cation, HIV, and Early Fertility" (2015)
#
##############################################

### First, let's recreate Table 2 and 3 results ###

# load data
studysample <- read.csv(here("data/studysampleclean.csv"))

# Define global variable list
varlist <- c("presence", "evmar05v3", "evpreg05v3", "evpregunmar05v3", "evunpregmar05v3", "dropout05v3",
             "dropout07v2", "evmar07v2", "evpreg07v2", "evpregunmar07v2", "evunpregmar07v2")
treatlist <- c("Uonly", "Honly","UH")
controllist <- c("yrbirth_all", "yrbirth_missing", "date05v3", "date07v2", "schsize", "stratum")

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