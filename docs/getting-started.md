# Getting Started

`zelig2` follows the same four-step workflow introduced by the original [Zelig](https://github.com/IQSS/Zelig) package (Imai, King, and Lau 2007, 2008): **estimate**, **set scenarios**, **simulate**, and **visualize**. If you have used Zelig before, the interface will be familiar.

## Installation

Install `zelig2` from GitHub:

```r
devtools::install_github("tlcaputi/zelig2", subdir = "r")
```

### Dependencies

`zelig2` requires the following packages (installed automatically):

| Package | Purpose |
|---|---|
| `ggplot2` | Visualization |
| `MASS` | Parameter simulation (mvrnorm), negative binomial models |
| `sandwich` | Robust and clustered standard errors |
| `survey` | Survey-weighted estimation |
| `patchwork` | Composing multi-panel plots |
| `AER` | Tobit models |
| `quantreg` | Quantile regression |
| `fixest` | Fixed effects models |

## Core Workflow

### Step 1: Estimate a Model

```r
library(zelig2)
data(mtcars)

z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
```

`zelig2()` accepts a formula, a model name, and a data frame --- mirroring the original `zelig()` interface from King, Tomz, and Wittenberg (2000). It returns a `zelig2` object containing the fitted model and all metadata needed for simulation.

### Step 2: Set a Counterfactual Scenario

```r
z <- setx(z, hp = 150, wt = 3)
```

`setx()` specifies the covariate values at which to compute quantities of interest. Variables not explicitly set default to their sample median (numeric) or mode (factor).

### Step 3: Simulate Quantities of Interest

```r
z <- sim(z)
```

`sim()` implements the simulation-based approach to inference from King, Tomz, and Wittenberg (2000). It draws parameter vectors from $\mathcal{N}(\hat{\beta}, \hat{V})$ and computes:

- **Expected values (EV)**: $E[Y \mid X]$, the systematic component.
- **Predicted values (PV)**: Draws from the stochastic component, incorporating observation-level noise.

### Step 4: Examine Results

```r
summary(z)
plot(z)
```

## First Differences

To estimate the causal effect of changing one covariate, set a baseline scenario with `setx()` and a contrast scenario with `setx1()`:

```r
z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
z <- setx(z, hp = 100, wt = 3)
z <- setx1(z, hp = 200, wt = 3)
z <- sim(z)
summary(z)
```

The first difference is $\text{EV}(\texttt{setx1}) - \text{EV}(\texttt{setx})$.

## Range Scenarios

Pass a vector of values to one covariate to see how predictions change across its range:

```r
z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars)
z <- setx(z, hp = seq(50, 300, by = 50), wt = 3)
z <- sim(z)
plot(z)
```

This produces a ribbon plot showing the expected value and its 95% confidence band across the range of `hp`.

## Pipe-Friendly

All functions return `zelig2` objects, so the workflow chains naturally with R's pipe operator:

```r
z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars) |>
  setx(hp = 150, wt = 3) |>
  sim()

summary(z)
plot(z)
```

## Robust Standard Errors

Specify `vcov_type` to use heteroskedasticity-consistent standard errors:

```r
z <- zelig2(mpg ~ hp + wt, model = "ls", data = mtcars,
            vcov_type = "HC1")
```

Available options: `"HC0"`, `"HC1"` (alias `"robust"`), `"HC2"`, `"HC3"`, `"HC4"`, `"cluster"`, `"bootstrap"`.

## Survey Weights

Pass a numeric weight vector to `weights`:

```r
z <- zelig2(y ~ x1 + x2, model = "ls", data = mydata,
            weights = mydata$sampling_weight)
```

For full survey design specifications, use `survey_design`, or the convenience arguments `ids`, `strata`, and `fpc`.

## Fixed Effects

Use the pipe syntax in the formula or the `fixef` argument:

```r
# Pipe syntax
z <- zelig2(y ~ x1 + x2 | state + year, model = "ls", data = panel)

# fixef argument
z <- zelig2(y ~ x1 + x2, model = "ls", data = panel,
            fixef = ~ state + year)
```

Fixed effects are estimated via `fixest` and support cluster-robust standard errors.
