# Survey-Weighted OLS

## Overview

Survey weights correct for differential selection probabilities and nonresponse in complex survey data. Ignoring weights can produce biased coefficient estimates and underestimated standard errors.

The original [Zelig](https://github.com/IQSS/Zelig) package (Imai, King, and Lau 2007, 2008) required separate model types for survey-weighted estimation (e.g., `"ls.survey"` instead of `"ls"`). `zelig2` simplifies this: pass a numeric weight vector to `weights` and the model is automatically estimated via `survey::svyglm` (Lumley 2004). No separate model type is needed.

This vignette uses data from the U.S. Census Bureau's **Household Pulse Survey** (Week 62, N = 58,202), a nationally representative survey with person-level probability weights (`PWEIGHT`).

## Model Estimation with Survey Weights

```r
library(zelig2)

z_weighted <- zelig2(
  mh_score ~ age + college + income_k,
  model = "ls",
  data = pulse,
  weights = pulse$pweight,
  num = 1000L
)

summary(z_weighted)
```

```
zelig2:  Least Squares (Linear Regression)
Formula:  mh_score ~ age + college + income_k
N:  58202
Survey-weighted: yes

Coefficients:
               Estimate  Std. Error  z value  Pr(>|z|)
(Intercept)  7.08844445  0.12424980  57.0499 < 2.2e-16 ***
age         -0.05369919  0.00194374 -27.6267 < 2.2e-16 ***
college     -0.40248340  0.05548744  -7.2536 4.059e-13 ***
income_k    -0.01016999  0.00038834 -26.1883 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Standard errors reflect the survey design and are generally larger than unweighted estimates.

## Comparing Weighted vs. Unweighted Estimates

```r
z_unweighted <- zelig2(mh_score ~ age + college + income_k,
                       model = "ls", data = pulse)

coef(z_unweighted)
```

```
(Intercept)         age     college    income_k
 7.46232840 -0.05766141 -0.53992615 -0.01049659
```

```r
coef(z_weighted)
```

```
(Intercept)         age     college    income_k
 7.08844445 -0.05369919 -0.40248340 -0.01016999
```

!!! warning "Substantial Differences"
    The college effect is **-0.540 unweighted** but **-0.402 weighted** --- a 34% difference. The unweighted sample overrepresents college-educated respondents, who differ systematically in mental health outcomes.

    Survey-weighted standard errors are also larger (college SE: 0.029 unweighted vs. 0.055 weighted). This reflects the effective sample size reduction from weighting and properly accounts for the survey design.

## Simulating Quantities of Interest

Set covariates and simulate expected values from the weighted model:

```r
z_weighted <- setx(z_weighted, age = 40, college = 1, income_k = 75)
z_weighted <- sim(z_weighted)
summary(z_weighted)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 3.7779  SD: 0.0451  [3.6871, 3.8663]
```

A 40-year-old college graduate earning $75,000 has an expected mental health score of **3.78** (95% CI: 3.69, 3.87). The wider confidence interval compared to the unweighted model (SD 0.045 vs. 0.023) reflects the additional uncertainty from survey weighting.

## First Differences

Compare college graduates to non-graduates under the weighted model:

```r
z_weighted <- zelig2(mh_score ~ age + college + income_k,
                     model = "ls", data = pulse,
                     weights = pulse$pweight, num = 1000L)

z_weighted <- setx(z_weighted, age = 40, college = 0, income_k = 75)
z_weighted <- setx1(z_weighted, age = 40, college = 1, income_k = 75)
z_weighted <- sim(z_weighted)
summary(z_weighted)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 4.1784  SD: 0.0499  [4.0852, 4.2774]
First Differences:
  Mean: -0.4016  SD: 0.0539  [-0.5137, -0.3021]
```

College graduates have mental health scores **0.40 points lower** (better) than non-graduates (95% CI: -0.51, -0.30). Note that the weighted first difference (-0.40) is attenuated relative to the unweighted estimate (-0.54), reflecting the correction for sample composition.

## Alternative Ways to Specify Weights

### Pre-built Survey Design

Pass an existing `survey::svydesign` object:

```r
library(survey)

pulse_design <- svydesign(ids = ~1, weights = ~pweight, data = pulse)

z <- zelig2(mh_score ~ age + college + income_k,
            model = "ls", data = pulse,
            survey_design = pulse_design)
```

### Complex Designs

For surveys with clustering, stratification, or finite population corrections:

```r
z <- zelig2(mh_score ~ age + college + income_k,
            model = "ls", data = pulse,
            weights = pulse$pweight,
            ids = ~cluster_id,
            strata = ~stratum)
```

These arguments are passed to `survey::svydesign` internally.

!!! tip "When to Use Survey Weights"
    Use weights whenever your data come from a complex survey design: national surveys (NHANES, CPS, ACS), multi-stage cluster samples, stratified samples, or surveys with oversampling or differential nonresponse.

## References

- **King, G., Tomz, M., and Wittenberg, J.** (2000). "Making the Most of Statistical Analyses: Improving Interpretation and Presentation." *American Journal of Political Science*, 44(2), 347--361.
- **Imai, K., King, G., and Lau, O.** (2008). "Toward a Common Framework for Statistical Analysis and Development." *Journal of Computational and Graphical Statistics*, 17(4), 892--913.
- **Lumley, T.** (2004). "Analysis of Complex Survey Samples." *Journal of Statistical Software*, 9(8), 1--19.
