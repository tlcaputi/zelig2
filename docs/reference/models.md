# Supported Models

The `zelig2` package supports 8 statistical models. The original [Zelig](https://github.com/IQSS/Zelig) package (Imai, King, and Lau 2007, 2008) supported many of the same model families; `zelig2` retains the core set while adding fixed effects (via `fixest`, Bergé 2018), robust/clustered/bootstrap standard errors (via `sandwich`, Zeileis 2004), and seamless survey weight integration (via `survey`, Lumley 2004). All models support standard errors via robust, cluster, or bootstrap methods.

## Model Summary

| Model | String | Family | Link | Outcome Type | Fixed Effects | Survey Weights |
|-------|--------|--------|------|--------------|---------------|----------------|
| Least Squares | `ls` | gaussian | identity | Continuous | Yes | Yes |
| Logistic Regression | `logit` | binomial | logit | Binary (0/1) | Yes | Yes |
| Probit Regression | `probit` | binomial | probit | Binary (0/1) | Yes | Yes |
| Poisson Regression | `poisson` | poisson | log | Count | Yes | Yes (quasipoisson) |
| Negative Binomial | `negbin` | custom | log | Overdispersed count | Yes | Yes (quasipoisson approx) |
| Gamma Regression | `gamma` | Gamma | inverse | Positive continuous | Yes | Yes |
| Tobit | `tobit` | custom | identity | Left-censored at 0 | No | No |
| Quantile Regression | `quantile` | custom | identity | Conditional quantiles | No | No |

## Detailed Model Descriptions

### Least Squares (OLS)

**Model string:** `"ls"`

**Statistical specification:** Gaussian family with identity link function

**Use case:** Linear regression for continuous, unbounded outcome variables.

**Example formula:**
```r
zelig2(income ~ education + age, model = "ls", data = mydata)
```

**Fixed effects:** Supported via `|` syntax or `fixef` parameter

**Survey weights:** Supported via `weights` or full survey design specification

---

### Logistic Regression

**Model string:** `"logit"`

**Statistical specification:** Binomial family with logit link function

**Use case:** Binary outcomes coded as 0/1. Estimates log-odds ratios.

**Example formula:**
```r
zelig2(employed ~ education + age, model = "logit", data = mydata)
```

**Fixed effects:** Supported. Uses conditional fixed effects logit for panel data.

**Survey weights:** Supported via survey design or weights parameter

**Note:** Outcome must be binary (0/1). Use `factor()` for labeled categories if needed.

---

### Probit Regression

**Model string:** `"probit"`

**Statistical specification:** Binomial family with probit (inverse normal) link function

**Use case:** Binary outcomes coded as 0/1. Assumes normally distributed latent variable.

**Example formula:**
```r
zelig2(voted ~ income + education, model = "probit", data = mydata)
```

**Fixed effects:** Supported via within-transformation

**Survey weights:** Supported via survey design or weights parameter

**Note:** Similar to logit but uses normal CDF instead of logistic. Coefficients not directly comparable to logit.

---

### Poisson Regression

**Model string:** `"poisson"`

**Statistical specification:** Poisson family with log link function

**Use case:** Count outcomes (non-negative integers). Assumes mean equals variance.

**Example formula:**
```r
zelig2(num_arrests ~ age + income, model = "poisson", data = mydata)
```

**Fixed effects:** Supported via conditional maximum likelihood

**Survey weights:** Supported. Uses quasipoisson for survey designs to handle design effects.

**Note:** If outcome shows overdispersion (variance > mean), consider negative binomial model.

---

### Negative Binomial

**Model string:** `"negbin"`

**Statistical specification:** Negative binomial distribution with log link

**Use case:** Overdispersed count data where variance exceeds the mean. More flexible than Poisson.

**Example formula:**
```r
zelig2(hospital_visits ~ age + chronic_illness, model = "negbin", data = mydata)
```

**Fixed effects:** Supported via within-transformation

**Survey weights:** Supported. Uses quasipoisson approximation for survey designs.

**Note:** Includes dispersion parameter to model extra-Poisson variation.

---

### Gamma Regression

**Model string:** `"gamma"`

**Statistical specification:** Gamma family with inverse link function

**Use case:** Positive continuous outcomes, often right-skewed (e.g., spending, duration, income).

**Example formula:**
```r
zelig2(medical_spending ~ insurance + age, model = "gamma", data = mydata)
```

**Fixed effects:** Supported via within-transformation

**Survey weights:** Supported via survey design or weights parameter

**Note:** Outcome must be strictly positive (> 0). Appropriate for skewed positive data.

---

### Tobit (Censored Regression)

**Model string:** `"tobit"`

**Statistical specification:** Normal distribution with left-censoring at 0

**Use case:** Continuous outcomes with a substantial proportion censored at zero (e.g., expenditures, time allocation).

**Example formula:**
```r
zelig2(charity_donation ~ income + education, model = "tobit", data = mydata)
```

**Fixed effects:** NOT supported. Tobit models do not support fixed effects estimation.

**Survey weights:** NOT supported in current implementation

**Implementation:** Uses `AER::tobit()` for maximum likelihood estimation.

**Note:** Assumes latent variable is normally distributed. Zeros should represent true censoring, not structural zeros.

---

### Quantile Regression

**Model string:** `"quantile"`

**Statistical specification:** Conditional quantile estimation via linear programming

**Use case:** Estimating effects at different points of the outcome distribution, not just the mean. Robust to outliers.

**Example formula:**
```r
zelig2(income ~ education + age, model = "quantile", data = mydata)
```

**Parameters:** Specify `tau` for desired quantile (default 0.5 for median):
```r
zelig2(income ~ education, model = "quantile", data = mydata, tau = 0.75)
```

**Fixed effects:** NOT supported. Quantile regression with fixed effects requires specialized methods.

**Survey weights:** NOT supported in current implementation

**Implementation:** Uses `quantreg::rq()` for estimation.

**Note:** Standard errors via bootstrap recommended. Useful when mean effects may be misleading due to heterogeneous treatment effects.

---

## Choosing the Right Model

**Continuous outcomes:**
- Unbounded, approximately normal: `ls`
- Positive, right-skewed: `gamma`
- Censored at zero: `tobit`
- Quantile effects: `quantile`

**Binary outcomes (0/1):**
- General use: `logit`
- Latent variable interpretation: `probit`

**Count outcomes:**
- No overdispersion: `poisson`
- Overdispersion present: `negbin`

**Need fixed effects?**
- Supported: `ls`, `logit`, `probit`, `poisson`, `negbin`, `gamma`
- Not supported: `tobit`, `quantile`

**Working with survey data?**
- Full support: `ls`, `logit`, `probit`, `poisson`, `gamma`
- Approximation: `negbin` (uses quasipoisson)
- Not supported: `tobit`, `quantile`
