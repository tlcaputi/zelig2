#!/usr/bin/env Rscript
# Generate real output for zelig2 documentation vignettes
# Uses Census Household Pulse Survey (Week 62) data

devtools::load_all("/Users/theo/MIT Dropbox/Theodore Caputi/packages/zelig2/r")
library(survey)

# --- Load and prepare data ---------------------------------------------------
pulse_raw <- read.csv(
  "~/DropboxMIT/research/census-pulse/data/week62/pulse2023_puf_62.csv"
)

cat("=== RAW DATA DIMENSIONS ===\n")
cat("Rows:", nrow(pulse_raw), "Cols:", ncol(pulse_raw), "\n\n")

# Build analysis dataset
pulse <- data.frame(
  # Mental health: days feeling anxious (1=not at all, 2=several, 3=more than half, 4=nearly every day)
  anxious  = pulse_raw$ANXIOUS,
  down     = pulse_raw$DOWN,
  interest = pulse_raw$INTEREST,
  worry    = pulse_raw$WORRY,
  # Demographics
  age      = 2023 - pulse_raw$TBIRTH_YEAR,
  educ     = pulse_raw$EEDUC,   # 1-7 education levels
  income   = pulse_raw$INCOME,  # 1-8 income brackets
  # State and region

  state    = pulse_raw$EST_ST,
  region   = pulse_raw$REGION,
  # Health insurance
  has_insurance = ifelse(pulse_raw$HLTHINS1 == 1 | pulse_raw$HLTHINS2 == 1, 1L, 0L),
  private_ins   = ifelse(pulse_raw$PRIVHLTH == 1, 1L, 0L),
  public_ins    = ifelse(pulse_raw$PUBHLTH == 1, 1L, 0L),
  # Food insecurity (1=enough, 2=enough but not always, 3=sometimes not enough, 4=often not enough)
  food_insecurity = pulse_raw$CURFOODSUF,
  # Survey weights
  pweight  = pulse_raw$PWEIGHT,
  # Race (1=White, 2=Black, 3=Asian, 4=Other)
  race     = pulse_raw$RRACE,
  hispanic = pulse_raw$RHISPANIC
)

# Recode education to meaningful levels
pulse$educ_cat <- factor(
  pulse$educ,
  levels = 1:7,
  labels = c("Less than HS", "Some HS", "HS Diploma",
             "Some College", "Associate's", "Bachelor's", "Graduate")
)

# Create a binary: college degree or not
pulse$college <- ifelse(pulse$educ >= 6, 1L, 0L)

# Create race category
pulse$race_cat <- factor(
  pulse$race,
  levels = 1:4,
  labels = c("White", "Black", "Asian", "Other")
)

# Recode food insecurity as binary (sometimes/often not enough = 1)
pulse$food_insecure <- ifelse(pulse$food_insecurity %in% c(3, 4), 1L, 0L)

# Mental health: create PHQ-style score (recode 1-4 to 0-3 and sum)
pulse$anxiety_score <- (pulse$anxious - 1) + (pulse$worry - 1)
pulse$depression_score <- (pulse$interest - 1) + (pulse$down - 1)
pulse$mh_score <- pulse$anxiety_score + pulse$depression_score

# Create region factor
pulse$region_cat <- factor(
  pulse$region,
  levels = 1:4,
  labels = c("Northeast", "South", "Midwest", "West")
)

# Create state factor
pulse$state_fct <- factor(pulse$state)

# Income as numeric (midpoints of brackets in $1000s)
pulse$income_k <- ifelse(pulse$income == 1, 12.5,
                  ifelse(pulse$income == 2, 25,
                  ifelse(pulse$income == 3, 37.5,
                  ifelse(pulse$income == 4, 55,
                  ifelse(pulse$income == 5, 87.5,
                  ifelse(pulse$income == 6, 125,
                  ifelse(pulse$income == 7, 175,
                  ifelse(pulse$income == 8, 250, NA))))))))

# Drop rows with -99, -88 sentinel values and NAs
pulse_clean <- pulse[complete.cases(pulse[, c("mh_score", "age", "college",
                                                "income_k", "food_insecure",
                                                "has_insurance", "region_cat",
                                                "state_fct", "race_cat",
                                                "pweight")]), ]
pulse_clean <- pulse_clean[pulse_clean$age > 0 & pulse_clean$age < 120, ]
pulse_clean <- pulse_clean[pulse_clean$mh_score >= 0, ]
pulse_clean <- pulse_clean[pulse_clean$income_k > 0, ]
pulse_clean <- pulse_clean[pulse_clean$pweight > 0, ]

cat("=== CLEAN DATA DIMENSIONS ===\n")
cat("Rows:", nrow(pulse_clean), "Cols:", ncol(pulse_clean), "\n\n")
cat("=== VARIABLE SUMMARIES ===\n")
cat("mh_score:", summary(pulse_clean$mh_score), "\n")
cat("age:", summary(pulse_clean$age), "\n")
cat("college:", table(pulse_clean$college), "\n")
cat("food_insecure:", table(pulse_clean$food_insecure), "\n")
cat("has_insurance:", table(pulse_clean$has_insurance), "\n")
cat("region_cat:", table(pulse_clean$region_cat), "\n")
cat("race_cat:", table(pulse_clean$race_cat), "\n\n")

set.seed(42)

# ============================================================================
# VIGNETTE 1: OLS
# ============================================================================
cat("==============================================================\n")
cat("VIGNETTE 1: OLS — Mental health score ~ age + college + income\n")
cat("==============================================================\n\n")

cat("--- Model Fitting ---\n")
z_ols <- zelig2(
  mh_score ~ age + college + income_k,
  model = "ls",
  data = pulse_clean,
  num = 1000L
)
print(z_ols)
cat("\n--- Summary ---\n")
summary(z_ols)

cat("\n--- Point scenario: 40-year-old college grad, $75k income ---\n")
z_ols <- setx(z_ols, age = 40, college = 1, income_k = 75)
z_ols <- sim(z_ols)
summary(z_ols)

cat("\n--- First Differences: college vs no college ---\n")
z_ols_fd <- zelig2(
  mh_score ~ age + college + income_k,
  model = "ls",
  data = pulse_clean,
  num = 1000L
)
z_ols_fd <- setx(z_ols_fd, age = 40, college = 0, income_k = 75)
z_ols_fd <- setx1(z_ols_fd, age = 40, college = 1, income_k = 75)
z_ols_fd <- sim(z_ols_fd)
summary(z_ols_fd)

cat("\n--- Range scenario: income from $25k to $250k ---\n")
z_ols_range <- zelig2(
  mh_score ~ age + college + income_k,
  model = "ls",
  data = pulse_clean,
  num = 1000L
)
z_ols_range <- setx(z_ols_range, age = 40, college = 1,
                     income_k = seq(25, 250, by = 25))
z_ols_range <- sim(z_ols_range)
summary(z_ols_range)

cat("\n--- Robust SEs ---\n")
z_ols_robust <- zelig2(
  mh_score ~ age + college + income_k,
  model = "ls",
  data = pulse_clean,
  vcov_type = "HC1"
)
summary(z_ols_robust)

# ============================================================================
# VIGNETTE 2: FIXED EFFECTS
# ============================================================================
cat("\n\n==============================================================\n")
cat("VIGNETTE 2: FIXED EFFECTS — Mental health score ~ age + college | state\n")
cat("==============================================================\n\n")

cat("--- Model Fitting ---\n")
z_fe <- zelig2(
  mh_score ~ age + college + income_k | state_fct,
  model = "ls",
  data = pulse_clean,
  num = 1000L
)
print(z_fe)
cat("\n--- Summary ---\n")
summary(z_fe)

cat("\n--- Point scenario with state FE (California=6) ---\n")
z_fe_ca <- setx(z_fe, age = 40, college = 1, income_k = 75, state_fct = "6")
z_fe_ca <- sim(z_fe_ca)
summary(z_fe_ca)

cat("\n--- Point scenario averaging over states ---\n")
z_fe_avg <- setx(z_fe, age = 40, college = 1, income_k = 75)
z_fe_avg <- sim(z_fe_avg)
summary(z_fe_avg)

cat("\n--- First Differences with FE: college vs no college ---\n")
z_fe_fd <- zelig2(
  mh_score ~ age + college + income_k | state_fct,
  model = "ls",
  data = pulse_clean,
  num = 1000L
)
z_fe_fd <- setx(z_fe_fd, age = 40, college = 0, income_k = 75)
z_fe_fd <- setx1(z_fe_fd, age = 40, college = 1, income_k = 75)
z_fe_fd <- sim(z_fe_fd)
summary(z_fe_fd)

cat("\n--- Cluster-robust SEs ---\n")
z_fe_cluster <- zelig2(
  mh_score ~ age + college + income_k | state_fct,
  model = "ls",
  data = pulse_clean,
  vcov_type = "cluster",
  cluster = ~state_fct
)
summary(z_fe_cluster)

# ============================================================================
# VIGNETTE 3: LOGISTIC REGRESSION
# ============================================================================
cat("\n\n==============================================================\n")
cat("VIGNETTE 3: LOGISTIC — Food insecurity ~ age + college + income\n")
cat("==============================================================\n\n")

cat("--- Model Fitting ---\n")
z_logit <- zelig2(
  food_insecure ~ age + college + income_k,
  model = "logit",
  data = pulse_clean,
  num = 1000L
)
print(z_logit)
cat("\n--- Summary ---\n")
summary(z_logit)

cat("\n--- Point scenario: 35-year-old, no college, $50k income ---\n")
z_logit <- setx(z_logit, age = 35, college = 0, income_k = 50)
z_logit <- sim(z_logit)
summary(z_logit)

cat("\n--- First Differences: $50k vs $100k income ---\n")
z_logit_fd <- zelig2(
  food_insecure ~ age + college + income_k,
  model = "logit",
  data = pulse_clean,
  num = 1000L
)
z_logit_fd <- setx(z_logit_fd, age = 35, college = 0, income_k = 50)
z_logit_fd <- setx1(z_logit_fd, age = 35, college = 0, income_k = 100)
z_logit_fd <- sim(z_logit_fd)
summary(z_logit_fd)

cat("\n--- Range scenario: income from $25k to $250k ---\n")
z_logit_range <- zelig2(
  food_insecure ~ age + college + income_k,
  model = "logit",
  data = pulse_clean,
  num = 1000L
)
z_logit_range <- setx(z_logit_range, age = 35, college = 0,
                       income_k = seq(25, 250, by = 25))
z_logit_range <- sim(z_logit_range)
summary(z_logit_range)

# ============================================================================
# VIGNETTE 4: SURVEY OLS
# ============================================================================
cat("\n\n==============================================================\n")
cat("VIGNETTE 4: SURVEY OLS — Mental health score with survey weights\n")
cat("==============================================================\n\n")

cat("--- Model Fitting ---\n")
z_svy_ols <- zelig2(
  mh_score ~ age + college + income_k,
  model = "ls",
  data = pulse_clean,
  weights = pulse_clean$pweight,
  num = 1000L
)
print(z_svy_ols)
cat("\n--- Summary ---\n")
summary(z_svy_ols)

cat("\n--- Comparison: unweighted vs weighted coefficients ---\n")
cat("Unweighted:\n")
print(coef(z_ols))
cat("\nWeighted:\n")
print(coef(z_svy_ols))

cat("\n--- Point scenario: 40-year-old college grad, $75k income ---\n")
z_svy_ols <- setx(z_svy_ols, age = 40, college = 1, income_k = 75)
z_svy_ols <- sim(z_svy_ols)
summary(z_svy_ols)

cat("\n--- First Differences: college vs no college ---\n")
z_svy_fd <- zelig2(
  mh_score ~ age + college + income_k,
  model = "ls",
  data = pulse_clean,
  weights = pulse_clean$pweight,
  num = 1000L
)
z_svy_fd <- setx(z_svy_fd, age = 40, college = 0, income_k = 75)
z_svy_fd <- setx1(z_svy_fd, age = 40, college = 1, income_k = 75)
z_svy_fd <- sim(z_svy_fd)
summary(z_svy_fd)

# ============================================================================
# VIGNETTE 5: SURVEY LOGISTIC
# ============================================================================
cat("\n\n==============================================================\n")
cat("VIGNETTE 5: SURVEY LOGISTIC — Food insecurity with survey weights\n")
cat("==============================================================\n\n")

cat("--- Model Fitting ---\n")
z_svy_logit <- zelig2(
  food_insecure ~ age + college + income_k,
  model = "logit",
  data = pulse_clean,
  weights = pulse_clean$pweight,
  num = 1000L
)
print(z_svy_logit)
cat("\n--- Summary ---\n")
summary(z_svy_logit)

cat("\n--- Comparison: unweighted vs weighted coefficients ---\n")
cat("Unweighted:\n")
print(coef(z_logit))
cat("\nWeighted:\n")
print(coef(z_svy_logit))

cat("\n--- Point scenario: 35-year-old, no college, $50k income ---\n")
z_svy_logit <- setx(z_svy_logit, age = 35, college = 0, income_k = 50)
z_svy_logit <- sim(z_svy_logit)
summary(z_svy_logit)

cat("\n--- First Differences: $50k vs $100k income ---\n")
z_svy_logit_fd <- zelig2(
  food_insecure ~ age + college + income_k,
  model = "logit",
  data = pulse_clean,
  weights = pulse_clean$pweight,
  num = 1000L
)
z_svy_logit_fd <- setx(z_svy_logit_fd, age = 35, college = 0, income_k = 50)
z_svy_logit_fd <- setx1(z_svy_logit_fd, age = 35, college = 0, income_k = 100)
z_svy_logit_fd <- sim(z_svy_logit_fd)
summary(z_svy_logit_fd)

cat("\n--- Range scenario: income from $25k to $250k ---\n")
z_svy_logit_range <- zelig2(
  food_insecure ~ age + college + income_k,
  model = "logit",
  data = pulse_clean,
  weights = pulse_clean$pweight,
  num = 1000L
)
z_svy_logit_range <- setx(z_svy_logit_range, age = 35, college = 0,
                            income_k = seq(25, 250, by = 25))
z_svy_logit_range <- sim(z_svy_logit_range)
summary(z_svy_logit_range)

cat("\n\n=== ALL VIGNETTE OUTPUT COMPLETE ===\n")
