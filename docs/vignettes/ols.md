# OLS Regression

## Overview

Ordinary Least Squares (OLS) regression models the relationship between a continuous outcome and one or more predictors. In `zelig2`, OLS is specified with `model = "ls"` (following the original Zelig convention from Imai, King, and Lau 2007, 2008). After estimation, simulation-based inference via `setx()` and `sim()` --- implementing the approach of King, Tomz, and Wittenberg (2000, *AJPS*) --- translates coefficient estimates into substantively meaningful quantities: expected values, predicted values, and first differences with full uncertainty quantification.

## Data

This vignette uses data from the U.S. Census Bureau's **Household Pulse Survey** (Week 62), a nationally representative survey tracking social and economic impacts on American households during and after the COVID-19 pandemic. After cleaning, the dataset contains **N = 58,202** respondents.

The outcome variable is a mental health symptom score constructed from PHQ-4 items, ranging from 0 (no symptoms) to 12 (severe symptoms). Higher scores indicate worse mental health. Predictors include:

- **age**: Respondent age in years (18--88)
- **college**: Binary indicator (1 = bachelor's degree or higher)
- **income_k**: Annual household income in thousands of dollars (midpoints of bracket categories)

## Model Estimation

```r
library(zelig2)

z <- zelig2(mh_score ~ age + college + income_k,
            model = "ls",
            data = pulse,
            num = 1000L)
```

## Interpreting the Results

```r
summary(z)
```

```
zelig2:  Least Squares (Linear Regression)
Formula:  mh_score ~ age + college + income_k
N:  58202

Coefficients:
               Estimate  Std. Error z value  Pr(>|z|)
(Intercept)  7.46232840  0.05248010 142.194 < 2.2e-16 ***
age         -0.05766141  0.00084954 -67.874 < 2.2e-16 ***
college     -0.53992615  0.02916059 -18.516 < 2.2e-16 ***
income_k    -0.01049659  0.00019970 -52.562 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

All three predictors are statistically significant:

- **Age**: Each additional year of age is associated with a 0.058-point decrease in mental health symptom scores. Older adults report fewer symptoms on average.
- **College education**: Having a bachelor's degree is associated with a 0.540-point reduction in symptom scores, holding age and income constant.
- **Income**: Each additional $1,000 in household income is associated with a 0.010-point decrease in symptom scores.

## Setting Scenarios and Simulating

### Point Scenario

What is the expected mental health score for a 40-year-old college graduate earning $75,000?

```r
z <- setx(z, age = 40, college = 1, income_k = 75)
z <- sim(z)
summary(z)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 3.8283  SD: 0.0229  [3.7833, 3.8726]
```

The expected symptom score is **3.83** (95% CI: 3.78, 3.87) --- a moderate-low level of symptoms. The narrow confidence interval reflects the large sample size.

## First Differences

First differences --- a central quantity of interest in the Zelig framework (King, Tomz, and Wittenberg 2000) --- quantify the expected change in the outcome when one covariate changes while others are held constant. Here we compare college graduates to non-graduates:

```r
z <- zelig2(mh_score ~ age + college + income_k,
            model = "ls", data = pulse, num = 1000L)

z <- setx(z, age = 40, college = 0, income_k = 75)
z <- setx1(z, age = 40, college = 1, income_k = 75)
z <- sim(z)
summary(z)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 4.3690  SD: 0.0237  [4.3236, 4.4131]
First Differences:
  Mean: -0.5399  SD: 0.0285  [-0.5952, -0.4857]
```

For 40-year-olds earning $75,000:

- **Without a college degree**: Expected score of 4.37 (95% CI: 4.32, 4.41)
- **With a college degree**: Expected score of 3.83 (95% CI: 3.78, 3.87)
- **First difference**: -0.54 (95% CI: -0.60, -0.49)

Having a college degree is associated with a **0.54-point reduction** in mental health symptom scores, approximately a 12% reduction relative to the non-college baseline.

## Range Scenarios

To see how expected values change across a continuous predictor, pass a vector of values to `setx()`:

```r
z <- zelig2(mh_score ~ age + college + income_k,
            model = "ls", data = pulse, num = 1000L)

z <- setx(z, age = 40, college = 1,
          income_k = seq(25, 250, by = 25))
z <- sim(z)
summary(z)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values (range):
      mean     sd 2.5%.2.5% 97.5%.97.5%
25  4.3526 0.0295    4.2942      4.4083
50  4.0903 0.0262    4.0393      4.1396
75  3.8279 0.0234    3.7825      3.8746
100 3.5655 0.0215    3.5232      3.6069
125 3.3032 0.0207    3.2629      3.3426
150 3.0408 0.0210    2.9986      3.0820
175 2.7784 0.0225    2.7332      2.8224
200 2.5161 0.0249    2.4672      2.5649
225 2.2537 0.0280    2.1966      2.3092
250 1.9913 0.0315    1.9277      2.0519
```

As household income increases from $25,000 to $250,000, expected symptom scores decline from **4.35 to 1.99** --- a reduction of 2.36 points (54%). Plotting these results produces a ribbon plot showing the confidence band:

```r
plot(z)
```

## Robust Standard Errors

When heteroskedasticity is a concern, use `vcov_type` to specify robust standard errors:

```r
z_robust <- zelig2(mh_score ~ age + college + income_k,
                   model = "ls", data = pulse,
                   vcov_type = "HC1")
summary(z_robust)
```

```
zelig2:  Least Squares (Linear Regression)
Formula:  mh_score ~ age + college + income_k
N:  58202
Vcov type:  HC1

Coefficients:
               Estimate  Std. Error z value  Pr(>|z|)
(Intercept)  7.46232840  0.05623047 132.710 < 2.2e-16 ***
age         -0.05766141  0.00084374 -68.340 < 2.2e-16 ***
college     -0.53992615  0.02966020 -18.204 < 2.2e-16 ***
income_k    -0.01049659  0.00018864 -55.642 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Comparing standard and robust SEs:

| Variable | Standard SE | Robust SE (HC1) | Change |
|---|---|---|---|
| (Intercept) | 0.05248 | 0.05623 | +7.1% |
| age | 0.00085 | 0.00084 | -0.7% |
| college | 0.02916 | 0.02966 | +1.7% |
| income_k | 0.00020 | 0.00019 | -5.5% |

The differences are modest, suggesting limited heteroskedasticity. Available options include `"HC0"` through `"HC4"`, `"cluster"`, and `"bootstrap"`.

!!! tip "When to Use Robust Standard Errors"
    Robust SEs are recommended when working with cross-sectional survey data, when diagnostic plots suggest non-constant variance, or when you want inference that does not rely on the homoskedasticity assumption. They are widely accepted in applied research and generally conservative.
