# Fixed Effects

## Introduction

Fixed effects models control for unobserved heterogeneity across groups (e.g., states, individuals, or time periods), allowing researchers to isolate within-group variation. The original [Zelig](https://github.com/IQSS/Zelig) package (Imai, King, and Lau 2007, 2008) did not support fixed effects. `zelig2` adds this capability through the `fixest` package (Bergé 2018), combining computational efficiency with the simulation-based inference approach of King, Tomz, and Wittenberg (2000).

This vignette demonstrates fixed effects modeling using data from the U.S. Census Bureau's **Household Pulse Survey** (Week 62, N = 58,202), examining mental health outcomes across 51 states and territories.

## Specifying Fixed Effects

### Pipe Syntax (Recommended)

Use `|` to separate predictors from fixed effects:

```r
library(zelig2)

# State fixed effects via pipe syntax
z <- zelig2(mh_score ~ age + college + income_k | state_fct,
            model = "ls", data = pulse)
```

The left side of `|` contains the outcome and covariates; the right side lists the fixed effect variables. Multiple fixed effects are supported: `y ~ x1 + x2 | fe1 + fe2`.

### fixef Argument

Alternatively, use the `fixef` argument with a one-sided formula or character vector:

```r
# One-sided formula
z <- zelig2(mh_score ~ age + college + income_k,
            model = "ls", data = pulse,
            fixef = ~state_fct)

# Character vector
z <- zelig2(mh_score ~ age + college + income_k,
            model = "ls", data = pulse,
            fixef = "state_fct")
```

Both approaches produce identical results.

## Model Estimation

```r
z <- zelig2(mh_score ~ age + college + income_k | state_fct,
            model = "ls", data = pulse, num = 1000L)

summary(z)
```

```
zelig2:  Least Squares (Linear Regression)
Formula:  mh_score ~ age + college + income_k
Full formula:  mh_score ~ age + college + income_k | state_fct
N:  58202
Fixed effects:  state_fct

Coefficients:
            Estimate  Std. Error z value  Pr(>|z|)
age      -0.05798140  0.00085191 -68.061 < 2.2e-16 ***
college  -0.53324388  0.02922396 -18.247 < 2.2e-16 ***
income_k -0.01054457  0.00020256 -52.056 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

!!! note "Absorbed Intercept"
    No intercept appears in the coefficient table. Each state has its own implicit intercept, which absorbs the overall constant.

## Interpreting Results

The coefficients represent within-state associations:

- **Age** (-0.058): Each additional year of age is associated with a 0.058-point decrease in mental health symptom scores, within the same state.
- **College** (-0.533): A college degree is associated with 0.533 fewer symptom points, comparing individuals in the same state with the same age and income.
- **Income** (-0.011): Each additional $1,000 in income is associated with a 0.011-point reduction.

The state fixed effects control for all time-invariant state-level characteristics (regional culture, policies, healthcare infrastructure) that might confound these relationships.

## Setting Scenarios with Fixed Effects

### Specific State

To predict outcomes for a specific state, pass the state level to `setx()`. Here we predict for California (FIPS code 6):

```r
z <- setx(z, age = 40, college = 1, income_k = 75, state_fct = "6")
z <- sim(z)
summary(z)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 4.0908  SD: 0.0442  [4.0014, 4.1785]
```

### Averaging Over States

Omitting the fixed effect variable averages predictions across all state levels:

```r
z <- setx(z, age = 40, college = 1, income_k = 75)
z <- sim(z)
summary(z)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 3.8190  SD: 0.0457  [3.7309, 3.9043]
```

The California-specific prediction (4.09) is higher than the population average (3.82), indicating that California has a higher baseline level of mental health symptoms relative to the average state.

!!! tip "Choosing Specific vs. Averaged Fixed Effects"
    Use **specific levels** when making predictions for a particular group (e.g., California residents). Use **averaged levels** for general population-level predictions or when the specific group is not theoretically meaningful.

## First Differences with Fixed Effects

Compare college graduates to non-graduates, averaging over states:

```r
z <- zelig2(mh_score ~ age + college + income_k | state_fct,
            model = "ls", data = pulse, num = 1000L)

z <- setx(z, age = 40, college = 0, income_k = 75)
z <- setx1(z, age = 40, college = 1, income_k = 75)
z <- sim(z)
summary(z)
```

```
--- Simulation Summary ( 1000  draws) ---
Expected Values:
  Mean: 4.3484  SD: 0.0391  [4.2694, 4.4232]
First Differences:
  Mean: -0.5316  SD: 0.0293  [-0.5895, -0.4714]
```

A college degree is associated with a **0.53-point reduction** in mental health symptom scores (95% CI: -0.59, -0.47), after controlling for state-level unobserved heterogeneity.

## Cluster-Robust Standard Errors

With state fixed effects, standard errors should be clustered at the state level to account for within-state error correlation:

```r
z_cluster <- zelig2(mh_score ~ age + college + income_k | state_fct,
                    model = "ls", data = pulse,
                    vcov_type = "cluster", cluster = ~state_fct)

summary(z_cluster)
```

```
zelig2:  Least Squares (Linear Regression)
Formula:  mh_score ~ age + college + income_k
Full formula:  mh_score ~ age + college + income_k | state_fct
N:  58202
Fixed effects:  state_fct
Vcov type:  cluster

Coefficients:
            Estimate  Std. Error z value  Pr(>|z|)
age      -0.05798140  0.00118716 -48.840 < 2.2e-16 ***
college  -0.53324388  0.03369631 -15.825 < 2.2e-16 ***
income_k -0.01054457  0.00026454 -39.861 < 2.2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

!!! warning "Standard Errors Increase with Clustering"
    | Variable | Default SE | Cluster SE | Ratio |
    |---|---|---|---|
    | age | 0.00085 | 0.00119 | 1.40x |
    | college | 0.02922 | 0.03370 | 1.15x |
    | income_k | 0.00020 | 0.00026 | 1.31x |

    Cluster-robust SEs are 15--40% larger because they account for within-state correlation. Failing to cluster can lead to overstated statistical significance.

## Supported Models

Fixed effects via `fixest` are supported for:

| Model | `model =` | fixest function |
|---|---|---|
| Linear regression | `"ls"` | `fixest::feols()` |
| Logistic regression | `"logit"` | `fixest::feglm()` |
| Probit regression | `"probit"` | `fixest::feglm()` |
| Poisson regression | `"poisson"` | `fixest::feglm()` |
| Negative binomial | `"negbin"` | `fixest::fenegbin()` |
| Gamma regression | `"gamma"` | `fixest::feglm()` |

!!! info "Models Without Fixed Effects Support"
    **Tobit** and **quantile regression** do not support fixed effects. Attempting to use `|` or `fixef` with these models produces an informative error.
