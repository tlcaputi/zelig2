# Logistic Regression

Logistic regression models binary outcomes as a function of predictor variables. Unlike linear regression, logistic regression ensures predicted probabilities remain between 0 and 1 by modeling the log-odds of the outcome. In `zelig2`, logistic regression is specified with `model = "logit"` (following the original Zelig naming convention from Imai, King, and Lau 2007, 2008).

## Data: Food Insecurity in America

This vignette uses data from the U.S. Census Bureau's **Household Pulse Survey** (Week 62, N = 58,202). The outcome is food insecurity, defined as a binary indicator (1 = sometimes or often not enough food to eat, 0 = enough food). Predictors include:

- **age**: Respondent age in years
- **college**: Binary indicator for bachelor's degree (1 = yes, 0 = no)
- **income_k**: Household income in thousands of dollars

## Model Estimation

```r
library(zelig2)

z <- zelig2(food_insecure ~ age + college + income_k,
            model = "logit",
            data = pulse,
            num = 1000L)

summary(z)
```

```
zelig2:  Logistic Regression
Formula:  food_insecure ~ age + college + income_k
N:  58202

Coefficients:
               Estimate  Std. Error  z value  Pr(>|z|)
(Intercept)  0.52286307  0.05693662   9.1832 < 2.2e-16 ***
age         -0.02426752  0.00097111 -24.9894 < 2.2e-16 ***
college     -0.89757810  0.03818323 -23.5071 < 2.2e-16 ***
income_k    -0.02082702  0.00047535 -43.8138 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

All three predictors are highly significant and negatively associated with food insecurity.

## Interpreting Coefficients

Coefficients are on the **log-odds scale**. Exponentiating gives odds ratios:

- **College degree**: exp(-0.898) = 0.41. A college degree is associated with 59% lower odds of food insecurity.
- **Income**: exp(-0.0208) = 0.979. Each additional $1,000 in income is associated with 2.1% lower odds.
- **Age**: exp(-0.0243) = 0.976. Each additional year of age is associated with 2.4% lower odds.

!!! note "Log-Odds vs. Probabilities"
    Odds ratios describe multiplicative effects on odds, not probabilities. As King, Tomz, and Wittenberg (2000) emphasize, predicted probabilities and first differences (shown below) are far more interpretable for substantive conclusions.

## Predicted Probabilities

Following the Zelig framework (King, Tomz, and Wittenberg 2000), `setx()` and `sim()` convert log-odds to the probability scale (0--1), making results directly interpretable:

```r
z <- setx(z, age = 35, college = 0, income_k = 50)
z <- sim(z)
summary(z)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 0.2029  SD: 0.0040  [0.1952, 0.2107]
```

A 35-year-old without a college degree earning $50,000 has approximately a **20.3% probability** of food insecurity (95% CI: 19.5%, 21.1%).

## First Differences

First differences show how predicted probabilities change between two scenarios. Here we compare $50,000 vs. $100,000 in household income:

```r
z <- zelig2(food_insecure ~ age + college + income_k,
            model = "logit", data = pulse, num = 1000L)

z <- setx(z, age = 35, college = 0, income_k = 50)
z <- setx1(z, age = 35, college = 0, income_k = 100)
z <- sim(z)
summary(z)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 0.2028  SD: 0.0038  [0.1955, 0.2104]
First Differences:
  Mean: -0.1204  SD: 0.0026  [-0.1254, -0.1154]
```

Increasing household income from $50,000 to $100,000 reduces the probability of food insecurity by approximately **12 percentage points** (from 20.3% to 8.2%).

!!! tip "Interpreting First Differences"
    First differences are expressed as changes in probability (on a 0--1 scale). A first difference of -0.12 means a 12 percentage point decrease, not a 12% relative decrease.

## Range Scenarios

To see how predicted probabilities change across the full income distribution, pass a vector of values:

```r
z <- zelig2(food_insecure ~ age + college + income_k,
            model = "logit", data = pulse, num = 1000L)

z <- setx(z, age = 35, college = 0,
          income_k = seq(25, 250, by = 25))
z <- sim(z)
summary(z)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values (range):
      mean     sd 2.5%.2.5% 97.5%.97.5%
25  0.3001 0.0051    0.2901      0.3095
50  0.2029 0.0038    0.1954      0.2103
75  0.1313 0.0032    0.1254      0.1376
100 0.0824 0.0027    0.0774      0.0878
125 0.0507 0.0022    0.0465      0.0552
150 0.0307 0.0016    0.0276      0.0342
175 0.0185 0.0012    0.0163      0.0210
200 0.0111 0.0008    0.0095      0.0128
225 0.0066 0.0006    0.0056      0.0078
250 0.0039 0.0004    0.0032      0.0048
```

The range scenario reveals the characteristic nonlinearity of logistic regression:

1. **Largest effects at low incomes**: Moving from $25k to $50k reduces probability by 10 percentage points (30.0% to 20.3%), while moving from $225k to $250k reduces it by only 0.3 percentage points.
2. **Diminishing returns**: Additional income produces smaller reductions in food insecurity risk as income grows.
3. **Full range**: For a 35-year-old without a college degree, food insecurity probability ranges from 30% at $25k to under 0.4% at $250k.

```r
plot(z)
```

![Predicted probability of food insecurity across income levels, with 95% confidence band](../assets/logit-range.png)

!!! warning "Extrapolation"
    Ensure covariate values fall within the observed data range. Predictions far outside the data may be unreliable.
