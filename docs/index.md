# zelig2

A modern R package for statistical model estimation, counterfactual simulation, and visualization --- building on the [Zelig](https://github.com/IQSS/Zelig) framework developed at Harvard's Institute for Quantitative Social Science (IQSS).

## Background

The original **Zelig** package (Imai, King, and Lau 2007, 2008) introduced a powerful idea: unify statistical model estimation behind a single interface --- `zelig() -> setx() -> sim() -> plot()` --- so that researchers could move seamlessly from model fitting to substantively meaningful quantities of interest (expected values, predicted values, and first differences). Zelig operationalized the simulation-based approach to statistical interpretation described in King, Tomz, and Wittenberg (2000, *AJPS*).

`zelig2` is a ground-up reimplementation of Zelig that preserves its core workflow while modernizing the internals. The estimation--simulation--visualization pipeline remains the same; `zelig2` updates the package architecture, plotting system, and feature set for contemporary applied research.

## The zelig2 Workflow

Every analysis in `zelig2` follows four steps:

```r
library(zelig2)

z <- zelig2(y ~ x1 + x2, model = "ls", data = mydata)   # 1. Estimate
z <- setx(z, x1 = 10)                                     # 2. Set scenario
z <- sim(z)                                                # 3. Simulate
plot(z)                                                    # 4. Visualize
```

1. **Estimate** a model with `zelig2()`.
2. **Set** counterfactual covariate values with `setx()` (and optionally `setx1()` for contrasts).
3. **Simulate** quantities of interest --- expected values, predicted values, and first differences --- with `sim()`.
4. **Visualize** the results with `plot()`.

## Why zelig2?

Zelig (Imai, King, and Lau) was last updated in 2020 and relies on `setRefClass`, base R graphics, and 16 dependencies. `zelig2` retains Zelig's workflow while addressing these limitations:

| Feature | Zelig (Imai, King, and Lau) | zelig2 |
|---|---|---|
| Survey weights | Separate model types (`normal.survey`); [buggy](vignettes/comparison.md#2-survey-weights) | Seamless `weights =`; matches `svyglm()` exactly |
| Fixed effects | Not supported | Built-in via `fixest` |
| Robust SEs | Not supported | HC0--HC4, cluster, bootstrap |
| Plotting | Base R | ggplot2 + patchwork |
| Dependencies | 16 | 7 |
| OOP style | setRefClass (mutable) | S3 (functional, immutable) |
| Last updated | 2020 | Active |

## Supported Models

| Model | `model =` | Category |
|---|---|---|
| Least Squares (OLS) | `"ls"` | Continuous |
| Logistic Regression | `"logit"` | Binary |
| Probit Regression | `"probit"` | Binary |
| Poisson Regression | `"poisson"` | Count |
| Negative Binomial | `"negbin"` | Count |
| Gamma Regression | `"gamma"` | Continuous |
| Tobit (Censored) | `"tobit"` | Continuous |
| Quantile Regression | `"quantile"` | Continuous |

All models except tobit and quantile support fixed effects via `fixest`.

## Quick Example

Using data from the U.S. Census Bureau's Household Pulse Survey, we estimate the relationship between household income and food insecurity:

```r
library(zelig2)

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

Moving from \$50k to \$100k in household income reduces the predicted probability of food insecurity by approximately 12 percentage points (95% CI: [-0.125, -0.115]).

## Installation

```r
# Install from GitHub
devtools::install_github("tlcaputi/zelig2", subdir = "r")
```

## Acknowledgments

`zelig2` builds directly on the intellectual foundation of the **Zelig** project at Harvard's Institute for Quantitative Social Science (IQSS). The core idea --- estimate a model, set counterfactual scenarios, simulate quantities of interest, and visualize --- originates with:

- **King, G., Tomz, M., and Wittenberg, J.** (2000). "Making the Most of Statistical Analyses: Improving Interpretation and Presentation." *American Journal of Political Science*, 44(2), 347--361.
- **Imai, K., King, G., and Lau, O.** (2007). "Zelig: Everyone's Statistical Software." [https://github.com/IQSS/Zelig](https://github.com/IQSS/Zelig)
- **Imai, K., King, G., and Lau, O.** (2008). "Toward a Common Framework for Statistical Analysis and Development." *Journal of Computational and Graphical Statistics*, 17(4), 892--913.

The Zelig package was subsequently maintained by Choirat, Honaker, Imai, King, and Lau. `zelig2` would not exist without their foundational work.
