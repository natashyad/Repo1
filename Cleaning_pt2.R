library(haven)
library(dplyr)
library(tidyr)
library(survey)
library(readr)
rm(list = ls())

lmds <- read_csv("lmdsa-2019-v1.1.csv")
# attempt 1
# # Employment Status - using indus variable as in Stata code
# lmds <- lmds %>%
#   mutate(worker_sim0 = case_when(
#     indus == 1 ~ 1,       # Agric
#     indus == 2 ~ 2,       # Mining
#     indus == 3 ~ 3,       # Manufacturing
#     indus >= 4 & indus <= 11 ~ 4,  # Services
#     indus == 5 ~ 5,       # unemployed
#     TRUE ~ NA_real_
#   )) %>%
#   mutate(worker_sim0 = ifelse(!is.na(Unempl_Status), 5, worker_sim0))  # unemployed in data

# Correct code for  Employment Status - Properly map all categories
lmds <- lmds %>%
  mutate(worker_sim0 = case_when(
    indus == 1 ~ 1,       # Agric
    indus == 2 ~ 2,       # Mining
    indus == 3 ~ 3,       # Manufacturing
    indus >= 4 & indus <= 11 ~ 4,  # Services
    Status == 2 | Status == 3 ~ 5, # Unemployed
    TRUE ~ NA_real_
  ))


# correct code for  Skilled Workers - Adjust education mapping
lmds <- lmds %>%
  mutate(
    skilled_sim0 = case_when(
      Q17EDUCATION %in% c("Bachelors Degree", "Honours Degree", 
                          "Higher Degree (Masters/Phd)", "Post Higher Diploma") ~ 1,
      grepl("Grade [1-9]|Grade 1[0-2]", Q17EDUCATION) ~ 0,
      TRUE ~ 0
    )
  )

# Skill level
lmds <- lmds %>%
  mutate(skilled_sim0 = ifelse(Q17EDUCATION >= 13 & 
                                 (Q17EDUCATION <= 18 | 
                                    (Q17EDUCATION >= 19 & Q17EDUCATION <= 28)), 1, 0))

# Identifiers
lmds <- lmds %>%
  mutate(hhid = UQNO,
         pid = PERSONNO)

# Other demographics
lmds <- lmds %>%
  mutate(age_sim0 = Q14AGE,
         age2_sim0 = Q14AGE * Q14AGE,
         male = ifelse(Q13GENDER == 1, 1, 0))

# Marital status dummies
lmds <- lmds %>%
  mutate(married = ifelse(Q16MARITALSTATUS == 1, 1, 0),
         living_together = ifelse(Q16MARITALSTATUS == 2, 1, 0),
         widowed = ifelse(Q16MARITALSTATUS == 3, 1, 0),
         divorced = ifelse(Q16MARITALSTATUS == 4, 1, 0),
         never_married = ifelse(Q16MARITALSTATUS == 5, 1, 0))


# (yrschool)
lmds <- lmds %>%
  mutate(
    yrschool = case_when(
      # Primary and secondary education
      Q17EDUCATION == "No schooling" ~ 0,
      Q17EDUCATION == "Grade R/0" ~ 0,
      Q17EDUCATION == "Grade 1/Sub A" ~ 1,
      Q17EDUCATION == "Grade 2/Sub B" ~ 2,
      Q17EDUCATION == "Grade 3/Standard 1" ~ 3,
      Q17EDUCATION == "Grade 4/Standard 2" ~ 4,
      Q17EDUCATION == "Grade 5/Standard 3" ~ 5,
      Q17EDUCATION == "Grade 6/Standard 4" ~ 6,
      Q17EDUCATION == "Grade 7/Standard 5" ~ 7,
      Q17EDUCATION == "Grade 8/Standard 6/Form 1" ~ 8,
      Q17EDUCATION == "Grade 9/Standard 7/Form 2" ~ 9,
      Q17EDUCATION == "Grade 10/Standard 8/Form 3" ~ 10,
      Q17EDUCATION == "Grade 11/Standard 9/Form 4" ~ 11,
      Q17EDUCATION == "Grade 12/Standard 10/Form 5/Matric" ~ 12,
      
      # Technical and vocational
      Q17EDUCATION %in% c("NTC l/N1/NIC/(v) Level 2", "NTC II/N2/NIC/(v) Level 3") ~ 12,
      Q17EDUCATION %in% c("NTC III/N3/NIC/(v) Level 4", "N4/NTC 4", "N5/NTC 5") ~ 13,
      Q17EDUCATION == "N6/NTC 6" ~ 14,
      
      # Higher education
      Q17EDUCATION == "Certificate with less than Grade 12/Std 10" ~ 10,
      Q17EDUCATION == "Certificate with Grade 12/Std 10" ~ 12,
      Q17EDUCATION == "Diploma with less than Grade 12/Std 10" ~ 12,
      Q17EDUCATION == "Diploma with Grade 12/Std 10" ~ 14,
      Q17EDUCATION == "Higher Diploma" ~ 14,
      Q17EDUCATION == "Bachelors Degree" ~ 16,
      Q17EDUCATION == "Honours Degree" ~ 17,
      Q17EDUCATION == "Bachelors Degree and Post Graduate Diploma" ~ 17,
      Q17EDUCATION == "Higher Degree (Masters/Phd)" ~ 18,
      Q17EDUCATION == "Post Higher Diploma (Masters; Doctoral Diploma)" ~ 18,
      
      # Other cases
      Q17EDUCATION %in% c("Do not know", "Unspecified", "Other") ~ NA_real_,
      TRUE ~ NA_real_  # Default for any unexpected values
    )
  )
# set the years of schooling to zero and start from there 
# (yrschool1) - alternative mapping
lmds <- lmds %>%
  mutate(
    yrschool1 = case_when(
      # Primary and secondary education (same as yrschool)
      Q17EDUCATION == "No schooling" ~ 0,
      Q17EDUCATION == "Grade R/0" ~ 0,
      Q17EDUCATION == "Grade 1/Sub A" ~ 1,
      Q17EDUCATION == "Grade 2/Sub B" ~ 2,
      Q17EDUCATION == "Grade 3/Standard 1" ~ 3,
      Q17EDUCATION == "Grade 4/Standard 2" ~ 4,
      Q17EDUCATION == "Grade 5/Standard 3" ~ 5,
      Q17EDUCATION == "Grade 6/Standard 4" ~ 6,
      Q17EDUCATION == "Grade 7/Standard 5" ~ 7,
      Q17EDUCATION == "Grade 8/Standard 6/Form 1" ~ 8,
      Q17EDUCATION == "Grade 9/Standard 7/Form 2" ~ 9,
      Q17EDUCATION == "Grade 10/Standard 8/Form 3" ~ 10,
      Q17EDUCATION == "Grade 11/Standard 9/Form 4" ~ 11,
      Q17EDUCATION == "Grade 12/Standard 10/Form 5/Matric" ~ 12,
      
      # Technical and vocational (alternative mapping)
      Q17EDUCATION %in% c("NTC l/N1/NIC/(v) Level 2", "NTC II/N2/NIC/(v) Level 3") ~ 11,
      Q17EDUCATION %in% c("NTC III/N3/NIC/(v) Level 4", "N4/NTC 4") ~ 12,
      Q17EDUCATION %in% c("N5/NTC 5", "N6/NTC 6") ~ 13,
      
      # Higher education (alternative mapping)
      Q17EDUCATION == "Certificate with less than Grade 12/Std 10" ~ 9,
      Q17EDUCATION == "Certificate with Grade 12/Std 10" ~ 11,
      Q17EDUCATION == "Diploma with less than Grade 12/Std 10" ~ 11,
      Q17EDUCATION == "Diploma with Grade 12/Std 10" ~ 13,
      Q17EDUCATION == "Higher Diploma" ~ 14,
      Q17EDUCATION == "Bachelors Degree" ~ 15,
      Q17EDUCATION == "Honours Degree" ~ 16,
      Q17EDUCATION == "Bachelors Degree and Post Graduate Diploma" ~ 16,
      Q17EDUCATION == "Higher Degree (Masters/Phd)" ~ 18,
      Q17EDUCATION == "Post Higher Diploma (Masters; Doctoral Diploma)" ~ 18,
      
      # Other cases
      Q17EDUCATION %in% c("Do not know", "Unspecified", "Other") ~ NA_real_,
      TRUE ~ NA_real_  # Default for any unexpected values
    )
  )

# Children variables
lmds <- lmds %>%
  mutate(children = ifelse(Q14AGE <= 15, 1, 0)) %>%
  group_by(UQNO, Qtr) %>%
  mutate(n_children = sum(children, na.rm = TRUE),
         hhsize = n()) %>%
  ungroup()

# Province dummies
lmds <- lmds %>%
  mutate(wCape = ifelse(Province == 1, 1, 0),
         eCape = ifelse(Province == 2, 1, 0),
         nCape = ifelse(Province == 3, 1, 0),
         Fstate = ifelse(Province == 4, 1, 0),
         Kwznatal = ifelse(Province == 5, 1, 0),
         nWest = ifelse(Province == 6, 1, 0),
         Gauteng = ifelse(Province == 7, 1, 0),
         Mpml = ifelse(Province == 8, 1, 0),
         Lim = ifelse(Province == 9, 1, 0))

# Rural/urban dummies
lmds <- lmds %>%
  mutate(urban = ifelse(Geo_type_code == 1, 1, 0),
         trad = ifelse(Geo_type_code == 2, 1, 0),
         farms = ifelse(Geo_type_code == 3, 1, 0))

# Income
lmds <- lmds %>%
  mutate(wages = ifelse(!is.na(Q54a_monthly), Q54a_monthly, Q57a_monthly))

# Check outliers
sum(lmds$wages > 1000000 & !is.na(lmds$wages)) # outliers
sum(lmds$wages == 0, na.rm = TRUE) # zeros

# Log wages by skill
lmds <- lmds %>%
  mutate(ln_wage_1 = ifelse(skilled_sim0 == 1 & !is.na(wages), log(wages + 1), NA_real_),
         ln_wage_0 = ifelse(skilled_sim0 == 0 & !is.na(wages), log(wages + 1), NA_real_))

# Race
lmds <- lmds %>%
  mutate(race = Q15POPULATION)

# Keep selected variables (same as Stata)
# lmds_clean <- lmds %>%
#   select(worker_sim0, skilled_sim0, hhid, pid, age_sim0, age2_sim0, male, married, 
#          living_together, widowed, divorced, never_married, Q16MARITALSTATUS, race, 
#          yrschool, yrschool1, children, n_children, wCape, eCape, nCape, Fstate, 
#          Kwznatal, nWest, Gauteng, Mpml, Lim, Province, urban, farms, Geo_type_code, 
#          wages, Weight, hhsize, ln_wage_0, ln_wage_1, Qtr, Status)

# attempt2

# Keep selected variables - ADD Q17EDUCATION for verification
lmds_clean <- lmds %>%
  select(worker_sim0, skilled_sim0, hhid, pid, age_sim0, age2_sim0, male, married, 
         living_together, widowed, divorced, never_married, Q16MARITALSTATUS, race, 
         Q17EDUCATION, yrschool, yrschool1, children, n_children, wCape, eCape, nCape, Fstate, 
         Kwznatal, nWest, Gauteng, Mpml, Lim, Province, urban, farms, Geo_type_code, 
         wages, Weight, hhsize, ln_wage_0, ln_wage_1, Qtr, Status)


# Correct code foe Age Range - Check Status variable name
summary(lmds_clean$age_sim0[lmds_clean$Q14AGE >= 15 & lmds_clean$worker_sim0 %in% 1:4])
# CHECKS

# 1. Population size check
weighted_total <- sum(lmds_clean$Weight, na.rm = TRUE)
print(paste("Weighted population total:", weighted_total/1e6, "million"))
# Compare to Stats SA 58.78 million

# 2. Employment figures check
employed <- lmds_clean %>%
  filter(age_sim0 >= 15) %>%
  group_by(worker_sim0) %>%
  summarize(total = sum(Weight, na.rm = TRUE))

print(employed)

# 3. Age range of employed
summary(lmds_clean$age_sim0[lmds_clean$Status == 1])

# 4. Education mapping check
education_check <- lmds_clean %>%
  count(Q17EDUCATION, yrschool, yrschool1) %>%
  arrange(Q17EDUCATION)

print(education_check, n = Inf)

# IMPROVED CHECKS 

# 1. Population size check
weighted_total <- sum(lmds_clean$Weight, na.rm = TRUE)
print(paste("Weighted population total:", round(weighted_total/1e6, 2), "million"))
# Compare to Stats SA 58.78 million

# 2. Employment figures check - improved
employed <- lmds_clean %>%
  filter(age_sim0 >= 15) %>%
  mutate(employment_status = case_when(
    worker_sim0 %in% c(1, 2, 3, 4) ~ "Employed",
    worker_sim0 == 5 ~ "Unemployed",
    is.na(worker_sim0) ~ "Not in labor force"
  )) %>%
  group_by(employment_status) %>%
  summarize(total = sum(Weight, na.rm = TRUE))

print(employed)

# 3. Age range of employed - fixed
employed_ages <- lmds_clean %>% 
  filter(Status == 1) %>% 
  pull(age_sim0) %>% 
  summary()
print("Age range of employed individuals:")
print(employed_ages)

# 4. Education mapping check - fixed
if("Q17EDUCATION" %in% names(lmds_clean)) {
  education_check <- lmds_clean %>%
    count(Q17EDUCATION, yrschool, yrschool1) %>%
    arrange(Q17EDUCATION)
  
  print("Education mapping verification:")
  print(education_check, n = 20)
} else {
  print("Q17EDUCATION not found in dataset - cannot verify education mapping")
}

# Additional check: Compare with Stata results
print("Worker_sim0 distribution:")
table(lmds_clean$worker_sim0, useNA = "always")

print("Skilled_sim0 distribution:")
table(lmds_clean$skilled_sim0, useNA = "always")

table(lmds$worker_sim0, useNA = "always")
# Should show 1-5 and NA

table(lmds$skilled_sim0, lmds$Q17EDUCATION) 
# Should show 1s for higher education

# Compare with Stata's weighted counts
# lmds_clean %>%
#   as_survey_design(weights = Weight) %>%
#   group_by(worker_sim0) %>%
#   summarize(total = survey_total())


# FIX 1: Employment Status - Verify variable names and values
lmds <- lmds %>%
  mutate(worker_sim0 = case_when(
    indus == 1 ~ 1,       # Agric
    indus == 2 ~ 2,       # Mining
    indus == 3 ~ 3,       # Manufacturing
    indus >= 4 & indus <= 11 ~ 4,  # Services
    Status == 2 | Status == 3 ~ 5, # Unemployed
    TRUE ~ NA_real_
  ))

# First check what values exist in indus and Status
table(lmds$indus, useNA = "always")
table(lmds$Status, useNA = "always")

# FIX 2: Age Range - Use correct variable name
# You created age_sim0 from Q14AGE, so use age_sim0
summary(lmds_clean$age_sim0[lmds_clean$worker_sim0 %in% 1:4])

# FIX 3: Skilled Workers - Ensure proper mapping
# Your education mapping looks correct in the verification table
# See what values exist in key variables
table(lmds$indus, useNA = "always")
table(lmds$Status, useNA = "always")

# Check worker_sim0 creation
lmds %>% count(indus, worker_sim0)

lmds <- lmds %>%
  mutate(worker_sim0 = case_when(
    sector1 == 1 | sector1 == 2 | sector1 == 4 ~ 1, # non-agric
    sector1 == 3 ~ 2,  # agric
    Status == 2 | Status == 3 ~ 3, # unemployed
    TRUE ~ NA_real_
  ))


# Check what percentage of cases are being caught
lmds %>% 
  summarize(
    agric = mean(indus == 1, na.rm = TRUE),
    mining = mean(indus == 2, na.rm = TRUE),
    manufacturing = mean(indus == 3, na.rm = TRUE),
    services = mean(indus >= 4 & indus <= 11, na.rm = TRUE),
    unemployed = mean(Status %in% 2:3, na.rm = TRUE)
  )


# # attempt 2
# library(haven)
# library(dplyr)
# library(tidyr)
# 
# # Clear workspace
# rm(list = ls())
# 
# # Read data - adjust path as needed
# lmds <- read_csv("lmdsa-2019-v1.1.csv")
# 
# # Employment Status - using sector1 as in original Stata code
# lmds <- lmds %>%
#   mutate(worker_sim0 = case_when(
#     sector1 == 1 | sector1 == 2 | sector1 == 4 ~ 1,  # non-agric
#     sector1 == 3 ~ 2,                                # agric
#     Status == 2 | Status == 3 ~ 3,                   # unemployed
#     Q14AGE >= 15 & Q14AGE <= 65 & Status == 4 ~ 3,   # discouraged job seeker
#     Status %in% c(2, 3, 4) & Q14AGE >= 15 ~ 3,       # unemployed or discouraged
#     TRUE ~ NA_real_
#   )) %>%
#   # Set to missing if Status == 4 and not already classified
#   mutate(worker_sim0 = ifelse(Status == 4 & is.na(worker_sim0), NA, worker_sim0)
# 
# # Skill level - using numeric codes as in Stata
# lmds <- lmds %>%
#   mutate(skilled_sim0 = ifelse(Q17EDUCATION >= 13 & 
#                                (Q17EDUCATION <= 18 | 
#                                 (Q17EDUCATION >= 19 & Q17EDUCATION <= 28)), 1, 0)
# 
# # Identifiers
# lmds <- lmds %>%
#   mutate(hhid = UQNO,
#          pid = PERSONNO)
# 
# # Other demographics
# lmds <- lmds %>%
#   mutate(age_sim0 = Q14AGE,
#          age2_sim0 = Q14AGE * Q14AGE,
#          male = ifelse(Q13GENDER == 1, 1, 0))
# 
# # Marital status dummies
# lmds <- lmds %>%
#   mutate(married = ifelse(Q16MARITALSTATUS == 1, 1, 0),
#          living_together = ifelse(Q16MARITALSTATUS == 2, 1, 0),
#          widowed = ifelse(Q16MARITALSTATUS == 3, 1, 0),
#          divorced = ifelse(Q16MARITALSTATUS == 4, 1, 0),
#          never_married = ifelse(Q16MARITALSTATUS == 5, 1, 0))
# 
# # Years of schooling - using numeric codes as in Stata
# lmds <- lmds %>%
#   mutate(
#     yrschool = case_when(
#       Q17EDUCATION %in% c(31, 30) ~ NA_real_,
#       Q17EDUCATION == 1 ~ 1,
#       Q17EDUCATION == 2 ~ 2,
#       Q17EDUCATION == 3 ~ 3,
#       Q17EDUCATION == 4 ~ 4,
#       Q17EDUCATION == 5 ~ 5,
#       Q17EDUCATION == 6 ~ 6,
#       Q17EDUCATION == 7 ~ 7,
#       Q17EDUCATION == 8 ~ 8,
#       Q17EDUCATION == 9 ~ 9,
#       Q17EDUCATION %in% c(10, 13, 16) ~ 10,
#       Q17EDUCATION %in% c(11, 14, 17) ~ 11,
#       Q17EDUCATION %in% c(12, 15, 18) ~ 12,
#       Q17EDUCATION %in% 19:23 ~ 13,
#       Q17EDUCATION == 24 ~ 15,
#       Q17EDUCATION == 25 ~ 16,
#       Q17EDUCATION %in% 26:28 ~ 17,
#       TRUE ~ 0
#     ),
#     
#     yrschool1 = case_when(
#       Q17EDUCATION %in% c(98, 0) ~ 0,
#       Q17EDUCATION == 1 ~ 1,
#       Q17EDUCATION == 2 ~ 2,
#       Q17EDUCATION == 3 ~ 3,
#       Q17EDUCATION == 4 ~ 4,
#       Q17EDUCATION == 5 ~ 5,
#       Q17EDUCATION == 6 ~ 6,
#       Q17EDUCATION == 7 ~ 7,
#       Q17EDUCATION == 8 ~ 8,
#       Q17EDUCATION == 9 ~ 9,
#       Q17EDUCATION == 10 ~ 10,
#       Q17EDUCATION == 11 ~ 11,
#       Q17EDUCATION == 12 ~ 12,
#       Q17EDUCATION > 12 & Q17EDUCATION < 19 ~ 13,
#       Q17EDUCATION > 18 & Q17EDUCATION < 23 ~ 12,
#       Q17EDUCATION == 23 ~ 13,
#       Q17EDUCATION %in% 24:27 ~ 16,
#       Q17EDUCATION == 28 ~ 19,
#       TRUE ~ NA_real_
#     )
#   )
# 
# # Children variables
# lmds <- lmds %>%
#   mutate(children = ifelse(Q14AGE <= 15, 1, 0)) %>%
#   group_by(UQNO, Qtr) %>%
#   mutate(n_children = sum(children, na.rm = TRUE),
#          hhsize = n()) %>%
#   ungroup()
# 
# # Province dummies
# lmds <- lmds %>%
#   mutate(wCape = ifelse(Province == 1, 1, 0),
#          eCape = ifelse(Province == 2, 1, 0),
#          nCape = ifelse(Province == 3, 1, 0),
#          Fstate = ifelse(Province == 4, 1, 0),
#          Kwznatal = ifelse(Province == 5, 1, 0),
#          nWest = ifelse(Province == 6, 1, 0),
#          Gauteng = ifelse(Province == 7, 1, 0),
#          Mpml = ifelse(Province == 8, 1, 0),
#          Lim = ifelse(Province == 9, 1, 0))
# 
# # Rural/urban dummies
# lmds <- lmds %>%
#   mutate(urban = ifelse(Geo_type_code == 1, 1, 0),
#          trad = ifelse(Geo_type_code == 2, 1, 0),
#          farms = ifelse(Geo_type_code == 3, 1, 0))
# 
# # Income
# lmds <- lmds %>%
#   mutate(wages = ifelse(!is.na(Q54a_monthly), Q54a_monthly, Q57a_monthly)) %>%
#   mutate(ln_wage_1 = ifelse(skilled_sim0 == 1 & !is.na(wages), log(wages + 1), NA_real_),
#          ln_wage_0 = ifelse(skilled_sim0 == 0 & !is.na(wages), log(wages + 1), NA_real_))
# 
# # Race
# lmds <- lmds %>%
#   mutate(race = Q15POPULATION)
# 
# # Keep selected variables (same as Stata) - ADD Q17EDUCATION for verification
# lmds_clean <- lmds %>%
#   select(worker_sim0, skilled_sim0, hhid, pid, age_sim0, age2_sim0, male, married, 
#          living_together, widowed, divorced, never_married, Q16MARITALSTATUS, race, 
#          Q17EDUCATION, yrschool, yrschool1, children, n_children, wCape, eCape, nCape, Fstate, 
#          Kwznatal, nWest, Gauteng, Mpml, Lim, Province, urban, farms, Geo_type_code, 
#          wages, Weight, hhsize, ln_wage_0, ln_wage_1, Qtr, Status)
# 
# 
# # Verification checks
# # 1. Population size
# weighted_total <- sum(lmds_clean$Weight, na.rm = TRUE)
# print(paste("Weighted population total:", round(weighted_total/1e6, 2), "million"))
# 
# # 2. Employment figures
# employed <- lmds_clean %>%
#   filter(age_sim0 >= 15) %>%
#   group_by(worker_sim0) %>%
#   summarize(total = sum(Weight, na.rm = TRUE))
# 
# print(employed)
# 
# # 3. Age range of employed
# summary(lmds_clean$age_sim0[lmds_clean$Status == 1])
# 
# # 4. Education mapping check
# education_check <- lmds_clean %>%
#   count(Q17EDUCATION, yrschool, yrschool1) %>%
#   arrange(Q17EDUCATION)
# 
# print(education_check, n = 20)
# Key changes made: