# zelig2 0.1.0

Development notes for the (as yet unreleased) 0.1.0.

## New features

* **`setx()` / `setx1()` gain a `factor_default` argument.** `"mode"` (the
  default, and the previous behaviour) sets each unspecified factor covariate to
  its modal level, so the counterfactual observation sits in a single category.
  `"mean"` instead replaces the variable's dummy columns with their column means
  in the fitted model matrix, giving a population-average covariate profile —
  the convention used by manual implementations of King, Tomz, and Wittenberg
  (2000), and the right choice when the target is a population-average effect.

* **`fn = "mean"` now uses weighted means on weighted fits.** When the
  underlying fit carries non-trivial observation weights (e.g., a survey-weighted
  `svyglm` fit), defaults for unspecified *numeric* covariates are computed as
  weighted means, matching what `factor_default = "mean"` already did for factor
  dummies. Without this the X-row mixed a sample mean for numeric covariates
  with a population mean for factors. Unweighted fits, and any `fn` other than
  `"mean"`, are unaffected.

## Bug fixes

* **Fixed: range plots over a factor covariate were empty.** A range scenario
  may vary a factor (e.g. `setx(z, region = c("North", "South"))`); `sim()`
  handled it, but `plot()` coerced the levels with `as.numeric()`, producing all-
  NA x values, an empty ribbon, and a "NAs introduced by coercion" warning.
  Non-numeric ranges are now drawn on a discrete axis with point ranges.

* **Fixed: survey designs built from a numeric `weights` vector failed.** Passing
  `weights` as a numeric vector *together with* `ids`/`strata`/`fpc` errored with
  `object '.zelig2_weights' not found`. `resolve_weights()` attached its synthetic
  `.zelig2_weights` column to its own local copy of `data`; because R is
  copy-on-modify, that column did not exist in the data frame subsequently handed
  to `survey::svydesign()`, which was then asked to evaluate `~.zelig2_weights`
  against it.

  Only this one combination was affected. `weights` alone worked (that path passes
  the vector straight to `svyglm` rather than building a design), and `weights` as
  a **formula** or column-name string worked (no synthetic column is created).
  Every existing test used one of those forms, which is why the bug survived.

## Breaking changes

* **Supplying both `survey_design` and any of `weights`/`ids`/`strata`/`fpc` is now
  an error.** Previously the components were discarded **silently** and the
  pre-built design's own weights and clustering were used — so a caller who passed
  both got results from the design while believing their own weights had been
  applied.

  This errors rather than warns deliberately. There is no coherent reason to supply
  a complete design and separate components simultaneously; it is always an
  ambiguity about which is in force. In a statistical package the cost of guessing
  wrong is a wrong published number, and a warning emitted partway through a long
  script is easily missed.

  Pass **either** a pre-built `survey_design` **or** the components, not both.

## Documentation

* Documented the two mutually exclusive routes to a survey design (components
  vs. a pre-built `svydesign`) and when to set `nest = TRUE`, in both the R help
  and the site's API reference.
* Corrected the API reference, which described `setx()`, `setx1()`, and `sim()`
  as returning standalone `setx`/`sim` objects. All three return the `zelig2`
  object with the scenario or simulation attached, so calls chain
  (`z <- setx(z, ...)`). The vignettes were already correct.
* Fixed a survey example in the API reference that omitted the required `data`
  argument.

## Tests

* Added tests for weighted vs. unweighted `fn = "mean"` defaults.
* Added regression tests covering the numeric-weights + `ids`/`strata` path,
  including an assertion that it produces coefficients identical to the equivalent
  pre-built `survey_design` (to 1e-10).
* Added a test asserting the new both-supplied error, and that a design on its own
  still works.
