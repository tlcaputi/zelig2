# API Reference

Complete reference for all user-facing functions in the `zelig2` package. The core workflow --- `zelig2() -> setx() -> sim() -> plot()` --- mirrors the original [Zelig](https://github.com/IQSS/Zelig) interface (Imai, King, and Lau 2007, 2008), implementing the simulation-based approach to statistical interpretation from King, Tomz, and Wittenberg (2000, *AJPS*).

## Core Functions

### zelig2()

Main entry point for fitting statistical models with simulation-based inference.

**Signature:**
```r
zelig2(formula, model, data, weights = NULL, survey_design = NULL,
       ids = NULL, strata = NULL, fpc = NULL, nest = FALSE,
       fixef = NULL, vcov_type = "default", cluster = NULL,
       bootstrap_n = 500, num = 1000, ...)
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `formula` | formula | Model formula. For fixed effects, use `y ~ x1 + x2 \| fe_var` syntax |
| `model` | character | Model type: "ls", "logit", "probit", "poisson", "negbin", "gamma", "tobit", "quantile" |
| `data` | data.frame | Input dataset. Character columns automatically converted to factors |
| `weights` | numeric vector, formula, or character | Survey or analytic weights. Can be vector, one-sided formula (e.g., `~weight_var`), or column name string |
| `survey_design` | survey.design | Pre-constructed survey design object from `survey` package |
| `ids` | formula or character | PSU/cluster identifiers for survey design (e.g., `~psu`) |
| `strata` | formula or character | Stratification variable for survey design (e.g., `~stratum`) |
| `fpc` | formula or character | Finite population correction for survey design |
| `nest` | logical | Whether PSUs are nested within strata (default FALSE). Set `TRUE` when cluster IDs are only unique *within* a stratum, as in most public-use survey files |
| `fixef` | formula or character vector | Alternative fixed effects specification. One-sided formula (e.g., `~state`) or character vector of variable names |
| `vcov_type` | character | Variance-covariance type: "default", "robust"/"HC1", "HC0", "HC2", "HC3", "HC4", "cluster", "bootstrap" |
| `cluster` | formula or character | Cluster variable for cluster-robust standard errors |
| `bootstrap_n` | integer | Number of bootstrap replicates when `vcov_type = "bootstrap"` (default 500) |
| `num` | integer | Number of simulation draws for quantities of interest (default 1000) |
| `...` | additional arguments | Model-specific parameters passed to underlying fitting function |

**Returns:** A `zelig2` object containing the fitted model, model specifications, and methods for simulation and visualization.

**Examples:**

```r
# Basic OLS
z1 <- zelig2(income ~ education + age, model = "ls", data = mydata)

# Logit with robust standard errors
z2 <- zelig2(employed ~ education, model = "logit", data = mydata,
             vcov_type = "robust")

# Fixed effects model
z3 <- zelig2(outcome ~ treatment + controls | state,
             model = "ls", data = panel_data)

# Survey-weighted model
z4 <- zelig2(satisfaction ~ service_quality, model = "ls", data = survey_data,
             weights = ~survey_weight, strata = ~region, ids = ~cluster_id)

# Quantile regression at 75th percentile
z5 <- zelig2(wage ~ education + experience, model = "quantile",
             data = mydata, tau = 0.75)
```

**Specifying a survey design:** there are two routes, and exactly one should be used. Either pass the components (`weights`/`ids`/`strata`/`fpc`) and let `zelig2()` build the `svydesign` for you, or pass a pre-built `survey_design`. Supplying both is an **error**: a pre-built design already carries its own weights and clustering, so the components could not be applied, and it is ambiguous which the caller meant. The two routes give identical coefficients; the pre-built design is preferable for anything beyond a simple design because it is explicit, inspectable, and reusable.

---

### setx()

Set covariate values to define a scenario for simulation of quantities of interest.

**Signature:**
```r
setx(object, ..., fn = NULL, factor_default = c("mode", "mean"))
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `object` | zelig2 | Fitted zelig2 model object |
| `...` | named arguments | Covariate values. One variable can be a vector to create range scenarios |
| `fn` | function or character | Default function applied to unspecified numeric covariates (e.g., "mean", "median", or custom function) |
| `factor_default` | character | How unspecified *factor* covariates are set: `"mode"` (default) uses the modal level; `"mean"` uses the column means of the variable's dummy columns |

**Returns:** The `zelig2` object with the scenario attached (any previous simulation results are cleared), so calls chain: `z <- setx(z, ...)`.

**Examples:**

```r
# Set all covariates to their means
z <- setx(z, fn = "mean")

# Set specific values
z <- setx(z, education = 16, age = 30)

# Range scenario: vary education while holding others at mean
z <- setx(z, education = 12:20, fn = "mean")

# Vary a factor across its levels
z <- setx(z, region = c("North", "South", "East", "West"), fn = "median")

# Population-average counterfactual: mean-of-dummies for unspecified factors
z <- setx(z, education = 16, fn = "mean", factor_default = "mean")
```

**Notes:**
- For factors, must specify valid factor levels
- Unspecified covariates use `fn` if numeric, or the sample median/mode otherwise
- Only one variable should vary in a single `setx()` call for range plots

**Modal vs. population-average scenarios.** `factor_default = "mode"` puts the counterfactual observation in a single, most-common category of every unspecified factor. `factor_default = "mean"` instead replaces each unspecified factor's dummy columns with their column means, so the scenario represents the average composition across categories --- the convention used by manual implementations of King, Tomz, and Wittenberg (2000), and the right choice when the target is a population-average effect rather than an effect at the modal covariate profile.

**Weighted fits.** When the fit carries non-trivial observation weights (e.g., a survey-weighted `svyglm` fit), `fn = "mean"` computes *weighted* means for unspecified numeric covariates, and `factor_default = "mean"` computes *weighted* dummy-column means. The scenario therefore describes the population the survey is designed to estimate, not the unweighted sample. Unweighted fits, and any `fn` other than `"mean"`, are unaffected.

---

### setx1()

Set a contrast scenario for computing first differences (treatment effects).

**Signature:**
```r
setx1(object, ..., fn = NULL, factor_default = c("mode", "mean"))
```

**Parameters:**

Same as `setx()`. Used in conjunction with `setx()` to define counterfactual scenarios.

**Returns:** The `zelig2` object with the contrast scenario attached.

**Examples:**

```r
# First difference: treatment effect
z <- setx(z, treatment = 0, fn = "mean")
z <- setx1(z, treatment = 1, fn = "mean")

# Before/after comparison
z <- setx(z, policy = 0, year = 2010, fn = "mean")
z <- setx1(z, policy = 1, year = 2015, fn = "mean")
```

**Usage:** Attach both scenarios, then call `sim(z)`. The first difference is `EV(setx1) - EV(setx)`.

---

### sim()

Simulate quantities of interest from a fitted model and specified covariate scenarios.

**Signature:**
```r
sim(object, num = NULL, ...)
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `object` | zelig2 | Fitted model with attached `setx()` and optionally `setx1()` scenarios |
| `num` | integer | Number of simulation draws. Overrides default from `zelig2()` call |
| `...` | additional arguments | Additional parameters for simulation |

**Returns:** The `zelig2` object with simulated quantities attached in `z$sim_out`:
- `ev`: Expected values (predictions)
- `pv`: Predicted values (with fundamental uncertainty)
- `fd`: First differences (if `setx1()` was specified)

**Examples:**

```r
# Basic prediction
z <- zelig2(income ~ education, model = "ls", data = mydata)
z <- setx(z, education = 16)
z <- sim(z)

# First differences
z <- zelig2(employed ~ training, model = "logit", data = mydata)
z <- setx(z, training = 0, fn = "mean")
z <- setx1(z, training = 1, fn = "mean")
z <- sim(z)

# Override number of simulations
z <- sim(z, num = 5000)

# The whole workflow chains
z <- zelig2(income ~ education, model = "ls", data = mydata) |>
  setx(education = 16) |>
  sim()
```

---

### plot()

Visualize simulation results. Automatically selects appropriate plot type based on scenario.

**Signature:**
```r
plot(x, ..., ci = 0.95)
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `x` | zelig2 | A simulated `zelig2` object (i.e., one that has been through `sim()`) |
| `...` | additional arguments | Graphical parameters |
| `ci` | numeric | Confidence interval level (default 0.95 for 95% CI) |

**Returns:** A ggplot object.

**Plot types:**
- Single scenario: density plot of expected values
- Range scenario: line plot with confidence bands
- First differences: density plot of treatment effect

**Examples:**

```r
# Density plot for single scenario
plot(z)

# Range plot with 90% CI
plot(z, ci = 0.90)

# Customize with ggplot2
plot(z) + labs(title = "Treatment Effect Distribution") + theme_minimal()
```

---

### summary()

Print model summary including coefficient table and simulation results.

**Signature:**
```r
summary(object, ...)
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `object` | zelig2 or sim object | Fitted model or simulation results |
| `...` | additional arguments | Passed to underlying summary methods |

**Returns:** Prints formatted summary to console. For sim objects, includes quantiles of simulated quantities.

**Examples:**

```r
# Model summary
summary(z)

# Simulation summary
summary(s)
```

---

### coef()

Extract model coefficients.

**Signature:**
```r
coef(object, ...)
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `object` | zelig2 | Fitted model object |
| `...` | additional arguments | Passed to underlying coef methods |

**Returns:** Named numeric vector of coefficients.

**Examples:**

```r
beta <- coef(z)
beta["education"]
```

---

### vcov()

Extract variance-covariance matrix of coefficients.

**Signature:**
```r
vcov(object, ...)
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `object` | zelig2 | Fitted model object |
| `...` | additional arguments | Passed to underlying vcov methods |

**Returns:** Variance-covariance matrix reflecting the specified `vcov_type`.

**Examples:**

```r
# Standard vcov
V <- vcov(z)

# For robust SEs, vcov reflects HC1 adjustment
z_robust <- zelig2(y ~ x, model = "ls", data = mydata, vcov_type = "robust")
V_robust <- vcov(z_robust)
```

---

## Utility Functions

### from_zelig2_model()

Extract the underlying fitted model object from a zelig2 wrapper.

**Signature:**
```r
from_zelig2_model(object)
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `object` | zelig2 | Fitted zelig2 model object |

**Returns:** The original fitted model object (e.g., `lm`, `glm`, `fixest::feols`, etc.).

**Examples:**

```r
z <- zelig2(y ~ x, model = "ls", data = mydata)
lm_fit <- from_zelig2_model(z)

# Use with stargazer or other tools
stargazer::stargazer(lm_fit)
```

---

### to_zelig2()

Wrap an existing fitted model in a zelig2 object for use with `setx()` and `sim()`.

**Signature:**
```r
to_zelig2(fit, model_name, ...)
```

**Parameters:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `fit` | model object | Fitted model (lm, glm, etc.) |
| `model_name` | character | Corresponding zelig2 model name ("ls", "logit", etc.) |
| `...` | additional arguments | Additional metadata or parameters |

**Returns:** A `zelig2` object wrapping the fitted model.

**Examples:**

```r
# Fit model manually
fit <- lm(income ~ education + age, data = mydata)

# Convert to zelig2 for simulation
z <- to_zelig2(fit, model_name = "ls")
z <- setx(z, education = 16, age = 30)
s <- sim(z)
```

**Note:** Useful for integrating zelig2 simulation workflow with existing model fitting code.

---

### list_models()

List all registered model names and descriptions.

**Signature:**
```r
list_models()
```

**Parameters:** None

**Returns:** Data frame with columns:
- `model`: Model name string
- `description`: Short description of the model
- `family`: Statistical family
- `supports_fixef`: Whether fixed effects are supported
- `supports_survey`: Whether survey weights are supported

**Examples:**

```r
list_models()

# Filter for models supporting fixed effects
models <- list_models()
models[models$supports_fixef, ]
```

---

## Object Classes

### zelig2 object

Returned by `zelig2()`. Contains:
- `fit`: Underlying fitted model
- `model`: Model name
- `formula`: Model formula
- `data`: Original data (subset to complete cases)
- `vcov_type`: Variance-covariance type
- `num`: Number of simulation draws
- Additional metadata for survey design, fixed effects, etc.

**Methods:** `summary()`, `coef()`, `vcov()`, `print()`

---

### setx object

Returned by `setx()`. Contains covariate scenario specifications.

**Methods:** `print()`

---

### sim object

Returned by `sim()`. Contains simulated quantities of interest.

**Slots:**
- `ev`: Expected values
- `pv`: Predicted values
- `fd`: First differences (if contrast scenario specified)
- `qi`: Quantile information

**Methods:** `summary()`, `plot()`, `print()`

---

## Advanced Usage

### Custom Variance-Covariance Specifications

```r
# Cluster-robust SEs
z <- zelig2(y ~ x, model = "ls", data = mydata,
            vcov_type = "cluster", cluster = ~state)

# Bootstrap SEs with 1000 replicates
z <- zelig2(y ~ x, model = "logit", data = mydata,
            vcov_type = "bootstrap", bootstrap_n = 1000)

# HC3 heteroskedasticity-robust
z <- zelig2(y ~ x, model = "ls", data = mydata, vcov_type = "HC3")
```

### Complex Survey Designs

```r
# Two-stage cluster sample -- components, design built for you
z <- zelig2(outcome ~ predictor, model = "ls", data = survey_data,
            ids = ~cluster_id, strata = ~stratum,
            weights = ~sampling_weight, nest = TRUE)

# Using pre-built survey design (`data` is still required)
library(survey)
des <- svydesign(ids = ~psu, strata = ~region, weights = ~wt,
                 data = mydata, nest = TRUE)
z <- zelig2(outcome ~ predictor, model = "ls", data = mydata,
            survey_design = des)

# Population-average scenario on a weighted fit: fn = "mean" gives weighted
# means for numeric covariates, factor_default = "mean" for factor dummies
z <- setx(z, predictor = 1, fn = "mean", factor_default = "mean")
z <- sim(z)
```

!!! warning "One route or the other"
    Passing `survey_design` *and* any of `weights` / `ids` / `strata` / `fpc` is an error. The design already carries its own weights and clustering, so the components cannot be applied --- earlier versions dropped them silently, which meant a caller could get results from the design's weights while believing their own had been used.

### Fixed Effects Models

```r
# Two-way fixed effects using formula syntax
z <- zelig2(outcome ~ treatment + controls | firm + year,
            model = "ls", data = panel_data)

# Alternative: fixef parameter
z <- zelig2(outcome ~ treatment + controls, model = "ls",
            data = panel_data, fixef = ~firm + year)
```

### Multiple Scenarios

```r
# Compare multiple treatment levels
z <- zelig2(outcome ~ treatment, model = "ls", data = mydata)

# Scenario 1: Control
z <- setx(z, treatment = 0)
s1 <- sim(z)

# Scenario 2: Low treatment
z <- setx(z, treatment = 1)
s2 <- sim(z)

# Scenario 3: High treatment
z <- setx(z, treatment = 2)
s3 <- sim(z)
```

---

## See Also

- [Supported Models](models.md) for detailed model descriptions
- Package vignettes for workflow tutorials
- `?zelig2` for R help documentation

## References

- **King, G., Tomz, M., and Wittenberg, J.** (2000). "Making the Most of Statistical Analyses: Improving Interpretation and Presentation." *American Journal of Political Science*, 44(2), 347--361.
- **Imai, K., King, G., and Lau, O.** (2007). "Zelig: Everyone's Statistical Software." [https://github.com/IQSS/Zelig](https://github.com/IQSS/Zelig)
- **Imai, K., King, G., and Lau, O.** (2008). "Toward a Common Framework for Statistical Analysis and Development." *Journal of Computational and Graphical Statistics*, 17(4), 892--913.
