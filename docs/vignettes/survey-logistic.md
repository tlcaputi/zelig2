# Survey-Weighted Logistic Regression

## Introduction

For binary outcomes, ignoring survey weights can produce biased coefficient estimates and severely underestimated standard errors. In the original [Zelig](https://github.com/IQSS/Zelig) package (Imai, King, and Lau 2007, 2008), survey-weighted logistic regression required specifying `model = "logit.survey"` as a separate model type. `zelig2` simplifies this: pass the weight vector to `weights` and the model is automatically estimated via `survey::svyglm` (Lumley 2004) with `family = binomial(link = "logit")`.

This vignette uses data from the U.S. Census Bureau's **Household Pulse Survey** (Week 62, N = 58,202) to examine predictors of food insecurity.

## Data

- **Outcome**: `food_insecure` (binary) --- household experienced food insufficiency in the past 7 days
- **Predictors**: `age` (years), `college` (bachelor's or higher, binary), `income_k` (household income in $1000s)
- **Survey weight**: `pweight` --- person-level probability weight

The Household Pulse Survey uses stratification by state and unequal selection probabilities. Probability weights ensure estimates reflect the U.S. household population.

## Model Estimation

```r
library(zelig2)

z_weighted <- zelig2(
  food_insecure ~ age + college + income_k,
  model = "logit",
  data = pulse,
  weights = pulse$pweight,
  num = 1000L
)

summary(z_weighted)
```

```
zelig2:  Logistic Regression
Formula:  food_insecure ~ age + college + income_k
N:  58202
Survey-weighted: yes

Coefficients:
               Estimate  Std. Error  z value Pr(>|z|)
(Intercept)  0.20445025  0.09842872   2.0771  0.03779 *
age         -0.01950071  0.00158885 -12.2735  < 2e-16 ***
college     -0.83557721  0.07313967 -11.4244  < 2e-16 ***
income_k    -0.01761030  0.00092506 -19.0369  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

!!! note "Survey-Weighted Standard Errors"
    Weighted standard errors account for the survey design and are typically larger than naive unweighted SEs.

## Comparing Weighted vs. Unweighted Estimates

```r
z_unweighted <- zelig2(food_insecure ~ age + college + income_k,
                       model = "logit", data = pulse)

coef(z_unweighted)
```

```
(Intercept)         age     college    income_k
 0.52286307 -0.02426752 -0.89757810 -0.02082702
```

```r
coef(z_weighted)
```

```
(Intercept)         age     college    income_k
 0.20445025 -0.01950071 -0.83557721 -0.01761030
```

!!! warning "Substantial Differences"
    The weighted intercept (0.204) is much smaller than the unweighted estimate (0.523), suggesting that the unweighted sample overstates baseline food insecurity risk. The age and income coefficients are attenuated with weights. The college effect is similar (-0.836 vs. -0.898).

    Standard errors are approximately 2x larger: college SE is 0.073 (weighted) vs. 0.038 (unweighted). Ignoring survey design leads to false precision.

## Predicted Probabilities

```r
z_weighted <- setx(z_weighted, age = 35, college = 0, income_k = 50)
z_weighted <- sim(z_weighted)
summary(z_weighted)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 0.2044  SD: 0.0068  [0.1914, 0.2171]
```

A 35-year-old without a college degree earning $50,000 has a **20.4% probability** of food insecurity (95% CI: 19.1%, 21.7%).

## First Differences

How much does doubling income from $50,000 to $100,000 reduce food insecurity risk?

```r
z_weighted <- zelig2(food_insecure ~ age + college + income_k,
                     model = "logit", data = pulse,
                     weights = pulse$pweight, num = 1000L)

z_weighted <- setx(z_weighted, age = 35, college = 0, income_k = 50)
z_weighted <- setx1(z_weighted, age = 35, college = 0, income_k = 100)
z_weighted <- sim(z_weighted)
summary(z_weighted)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 0.2045  SD: 0.0071  [0.1908, 0.2188]
First Differences:
  Mean: -0.1081  SD: 0.0048  [-0.1177, -0.0987]
```

Moving from $50,000 to $100,000 in income reduces food insecurity probability by **10.8 percentage points** (95% CI: -11.8, -9.9). The unweighted model estimates 12.0 pp, overstating the effect by about 11%.

## Range Scenarios

Simulate across the full income distribution:

```r
z_weighted <- zelig2(food_insecure ~ age + college + income_k,
                     model = "logit", data = pulse,
                     weights = pulse$pweight, num = 1000L)

z_weighted <- setx(z_weighted, age = 35, college = 0,
                   income_k = seq(25, 250, by = 25))
z_weighted <- sim(z_weighted)
summary(z_weighted)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values (range):
      mean     sd 2.5%.2.5% 97.5%.97.5%
25  0.2858 0.0089    0.2683      0.3031
50  0.2047 0.0068    0.1918      0.2181
75  0.1421 0.0062    0.1303      0.1545
100 0.0963 0.0058    0.0850      0.1082
125 0.0642 0.0052    0.0545      0.0748
150 0.0424 0.0043    0.0344      0.0510
175 0.0277 0.0035    0.0216      0.0349
200 0.0181 0.0027    0.0134      0.0238
225 0.0117 0.0020    0.0083      0.0161
250 0.0076 0.0015    0.0051      0.0108
```

The income gradient is steep at lower incomes:

- At **$25,000**: 28.6% probability of food insecurity
- At **$100,000**: 9.6% (19 pp reduction from $25k)
- At **$250,000**: 0.8% (near zero risk)

The steepest marginal reductions occur between $25k and $100k. Above $100k, the marginal effect diminishes as baseline risk becomes very low.

```r
plot(z_weighted)
```

![Survey-weighted predicted probability of food insecurity across income levels, with 95% confidence band](../assets/survey-logit-range.png)

## Summary

Following the Zelig framework (Imai, King, and Lau 2007, 2008), `zelig2` makes survey-weighted logistic regression accessible through the same `zelig2() -> setx() -> sim()` pipeline used for unweighted models. Key takeaways:

1. **Simple syntax**: `weights = your_weight_vector` --- no separate model type needed.
2. **Coefficients change**: Survey weights can substantially affect estimates. The unweighted intercept overstates baseline risk.
3. **Larger standard errors**: Weighted SEs are typically 1.5--3x larger, reflecting design effects.
4. **Probability-scale inference**: `setx()` and `sim()` produce predicted probabilities, which are far more interpretable than log-odds.
5. **First differences**: Quantify effects as changes in probability at specific covariate profiles.
6. **Range scenarios**: Reveal the nonlinear income--food insecurity gradient, steepest at low incomes.
