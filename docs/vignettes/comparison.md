# Comparison with the Original Zelig

## Background

`zelig2` is a reimplementation of the original [Zelig](https://github.com/IQSS/Zelig) R package developed at Harvard's Institute for Quantitative Social Science (King, Tomz, and Wittenberg 2000; Imai, King, and Lau 2007, 2008). The original Zelig introduced a unified framework for statistical estimation, counterfactual simulation, and presentation of results. `zelig2` preserves the core `zelig() -> setx() -> sim()` workflow while modernizing the implementation.

This page demonstrates two things:

1. **Identical results**: `zelig2` produces the same coefficient estimates as the original Zelig (and base R), because all three use `glm()` as the underlying estimation engine.
2. **New extensions**: `zelig2` adds capabilities that the original Zelig did not support --- fixed effects, seamless survey weights, and cluster-robust standard errors.

All examples use data from the U.S. Census Bureau's **Household Pulse Survey** (Week 62, N = 58,202).

---

## Part A: Identical Results

### Estimation with `zelig2`

```r
library(zelig2)

z <- zelig2(mh_score ~ age + college + income_k,
            model = "ls", data = pulse, num = 1000L)
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

### Estimation with base R `glm()`

```r
fit <- glm(mh_score ~ age + college + income_k, data = pulse)
summary(fit)
```

```
Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)  7.4623284  0.0524801  142.19   <2e-16 ***
age         -0.0576614  0.0008495  -67.87   <2e-16 ***
college     -0.5399261  0.0291606  -18.52   <2e-16 ***
income_k    -0.0104966  0.0001997  -52.56   <2e-16 ***
```

### Coefficients match exactly

| Variable | `zelig2` | `glm()` |
|---|---|---|
| (Intercept) | 7.46232840 | 7.46232840 |
| age | -0.05766141 | -0.05766141 |
| college | -0.53992615 | -0.53992615 |
| income_k | -0.01049659 | -0.01049659 |

The point estimates are identical to the last decimal place. This is expected: `zelig2` uses `glm()` internally for standard (non-fixed-effects) models, just as the original Zelig did. Standard errors also match.

![Coefficient comparison: zelig2 vs. base R glm()](../assets/comparison-coef.png)

### What `zelig2` adds: simulation-based inference

While `glm()` stops at coefficient estimates, `zelig2` implements the King, Tomz, and Wittenberg (2000) framework for translating those estimates into substantively meaningful quantities of interest:

```r
z <- setx(z, age = 40, college = 1,
          income_k = seq(25, 250, by = 25))
z <- sim(z)
plot(z)
```

![Expected mental health scores across income levels](../assets/comparison-range.png)

This range plot shows how expected mental health symptom scores decline from 4.35 to 1.99 as household income increases from $25,000 to $250,000, for a 40-year-old college graduate. The ribbon represents the 95% confidence band from 1,000 simulated draws.

---

## Part B: Extensions Beyond the Original Zelig

### 1. Fixed Effects

The original Zelig did not support fixed effects. `zelig2` integrates the `fixest` package (Berge 2018), allowing fixed effects via `|` in the formula:

```r
z_fe <- zelig2(mh_score ~ age + college + income_k | state_fct,
               model = "ls", data = pulse, num = 1000L)
summary(z_fe)
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
```

The 51 state fixed effects absorb all time-invariant state-level confounders. The `setx() -> sim()` workflow works identically:

```r
z_fe <- setx(z_fe, age = 40, college = 1,
             income_k = seq(25, 250, by = 25))
z_fe <- sim(z_fe)
plot(z_fe)
```

![Expected values with state fixed effects](../assets/fe-range.png)

The wider confidence band (compared to Part A) reflects the additional uncertainty from averaging over state-level intercepts.

### 2. Survey Weights

The original Zelig required separate model types for survey-weighted estimation (e.g., `model = "ls.survey"` instead of `model = "ls"`). In `zelig2`, pass the weight vector directly --- no model type change needed:

```r
z_svy <- zelig2(mh_score ~ age + college + income_k,
                model = "ls", data = pulse,
                weights = pulse$pweight, num = 1000L)
summary(z_svy)
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
```

Survey weighting substantially changes the estimates --- the college effect shrinks from -0.540 to -0.402 (a 26% reduction), and standard errors approximately double to reflect the survey design.

```r
z_svy <- setx(z_svy, age = 40, college = 1,
              income_k = seq(25, 250, by = 25))
z_svy <- sim(z_svy)
plot(z_svy)
```

![Expected values with survey weights](../assets/survey-range.png)

### 3. Cluster-Robust Standard Errors

The original Zelig did not support robust or clustered standard errors. `zelig2` provides HC0--HC4, cluster-robust, and bootstrap SEs via `vcov_type`:

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
```

Clustering at the state level increases standard errors by 15--40%:

| Variable | Default SE | Cluster SE | Ratio |
|---|---|---|---|
| age | 0.00085 | 0.00119 | 1.40x |
| college | 0.02922 | 0.03370 | 1.15x |
| income_k | 0.00020 | 0.00026 | 1.31x |

---

## Summary

| Feature | Original Zelig | `zelig2` |
|---|---|---|
| Point estimates | Via `glm()` | Via `glm()` (identical) |
| `setx() -> sim()` | Yes | Yes |
| Fixed effects | Not supported | `y ~ x \| fe` via `fixest` |
| Survey weights | Separate model types (`"ls.survey"`, `"logit.survey"`) | `weights = ...` on any model |
| Robust / clustered SEs | Not supported | `vcov_type = "HC1"`, `"cluster"`, `"bootstrap"` |
| Plotting | Base R | `ggplot2` + `patchwork` |

## References

- **King, G., Tomz, M., and Wittenberg, J.** (2000). "Making the Most of Statistical Analyses: Improving Interpretation and Presentation." *American Journal of Political Science*, 44(2), 347--361.
- **Imai, K., King, G., and Lau, O.** (2007). "Zelig: Everyone's Statistical Software." R package.
- **Imai, K., King, G., and Lau, O.** (2008). "Toward a Common Framework for Statistical Analysis and Development." *Journal of Computational and Graphical Statistics*, 17(4), 892--913.
- **Berge, L.** (2018). "Efficient Estimation of Maximum Likelihood Models with Multiple Fixed-Effects: the R Package fixest." CREA Discussion Paper.
