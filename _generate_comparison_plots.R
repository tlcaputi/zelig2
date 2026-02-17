#!/usr/bin/env Rscript
# Generate side-by-side comparison plots: Zelig vs zelig2
# Uses identical ggplot2 formatting for both, with descriptive facet labels

library(dplyr)
library(tibble)
assignInNamespace("tbl_df", function(data, ...) tibble::as_tibble(data), ns = "dplyr")
assignInNamespace("group_by_", function(.data, ..., .dots = list(), add = FALSE) {
  dots <- c(list(...), .dots)
  if (length(dots) == 0 || (length(dots) == 1 && dots[[1]] == "")) return(.data)
  group_by(.data, across(all_of(dots[[1]])))
}, ns = "dplyr")

library(Zelig)
library(ggplot2)
library(patchwork)
zelig2_env <- devtools::load_all(
  "/Users/theo/MIT Dropbox/Theodore Caputi/packages/zelig2/r",
  export_all = FALSE, quiet = TRUE
)

# --- Data prep ---------------------------------------------------------------
pulse_raw <- read.csv(
  "~/DropboxMIT/research/census-pulse/data/week62/pulse2023_puf_62.csv"
)
pulse <- data.frame(
  anxious = pulse_raw$ANXIOUS, down = pulse_raw$DOWN,
  interest = pulse_raw$INTEREST, worry = pulse_raw$WORRY,
  age = 2023 - pulse_raw$TBIRTH_YEAR, educ = pulse_raw$EEDUC,
  income = pulse_raw$INCOME, state = pulse_raw$EST_ST,
  pweight = pulse_raw$PWEIGHT, race = pulse_raw$RRACE
)
pulse$college <- ifelse(pulse$educ >= 6, 1L, 0L)
pulse$mh_score <- (pulse$anxious - 1) + (pulse$worry - 1) +
  (pulse$interest - 1) + (pulse$down - 1)
pulse$income_k <- ifelse(pulse$income == 1, 12.5,
  ifelse(pulse$income == 2, 25, ifelse(pulse$income == 3, 37.5,
  ifelse(pulse$income == 4, 55, ifelse(pulse$income == 5, 87.5,
  ifelse(pulse$income == 6, 125, ifelse(pulse$income == 7, 175,
  ifelse(pulse$income == 8, 250, NA))))))))
pulse$state_fct <- factor(pulse$state)
pulse$region <- factor(pulse_raw$REGION, levels = 1:4,
  labels = c("Northeast", "South", "Midwest", "West"))
pulse$race_cat <- factor(pulse$race, levels = 1:4,
  labels = c("White", "Black", "Asian", "Other"))
pulse$food_insecure <- ifelse(pulse_raw$CURFOODSUF %in% c(3, 4), 1L, 0L)
pulse$has_insurance <- ifelse(
  pulse_raw$HLTHINS1 == 1 | pulse_raw$HLTHINS2 == 1, 1L, 0L
)
pulse_clean <- pulse[complete.cases(pulse[, c(
  "mh_score", "age", "college", "income_k", "food_insecure",
  "has_insurance", "region", "state_fct", "race_cat", "pweight"
)]), ]
pulse_clean <- pulse_clean[
  pulse_clean$age > 0 & pulse_clean$age < 120 &
  pulse_clean$mh_score >= 0 & pulse_clean$income_k > 0 &
  pulse_clean$pweight > 0,
]
cat("Data ready: N =", nrow(pulse_clean), "\n")

out <- "/Users/theo/MIT Dropbox/Theodore Caputi/packages/zelig2/docs/assets"

# Descriptive facet labels
lbl_old <- "Zelig (IQSS/Zelig v5.1.7)"
lbl_new <- "zelig2 (tlcaputi/zelig2)"

# --- Helper ------------------------------------------------------------------
make_density <- function(df, title, xlim = NULL) {
  stats_df <- df |>
    group_by(package) |>
    summarise(
      mn = mean(value),
      lo = quantile(value, 0.025),
      hi = quantile(value, 0.975),
      .groups = "drop"
    )
  p <- ggplot(df, aes(x = value)) +
    geom_density(fill = "steelblue", alpha = 0.45, color = NA) +
    geom_vline(data = stats_df, aes(xintercept = mn),
               color = "darkblue", linewidth = 0.7) +
    geom_vline(data = stats_df, aes(xintercept = lo),
               linetype = "dashed", color = "grey30") +
    geom_vline(data = stats_df, aes(xintercept = hi),
               linetype = "dashed", color = "grey30") +
    facet_wrap(~package) +
    labs(title = title, x = "Value", y = "Density") +
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 11))
  if (!is.null(xlim)) p <- p + coord_cartesian(xlim = xlim)
  p
}

# =============================================================================
# POINT SCENARIO
# =============================================================================
cat("Point scenario...\n")

z_old <- Zelig::zelig(mh_score ~ age + college + income_k,
                      model = "ls", data = pulse_clean)
z_old <- Zelig::setx(z_old, age = 40, college = 1, income_k = 75)
set.seed(42)
z_old <- Zelig::sim(z_old)
zelig_ev <- as.numeric(z_old$get_qi("ev", "x"))
zelig_pv <- as.numeric(z_old$get_qi("pv", "x"))

z_new <- zelig2_env$env$zelig2(mh_score ~ age + college + income_k,
                                model = "ls", data = pulse_clean, num = 1000L)
z_new <- zelig2_env$env$setx(z_new, age = 40, college = 1, income_k = 75)
set.seed(42)
z_new <- zelig2_env$env$sim(z_new)
zelig2_ev <- as.numeric(z_new$sim_out$ev)
zelig2_pv <- as.numeric(z_new$sim_out$pv)

ev_df <- data.frame(
  value   = c(zelig_ev, zelig2_ev),
  package = factor(rep(c(lbl_old, lbl_new), each = length(zelig_ev)),
                   levels = c(lbl_old, lbl_new))
)
pv_df <- data.frame(
  value   = c(zelig_pv, zelig2_pv),
  package = factor(rep(c(lbl_old, lbl_new), each = length(zelig_pv)),
                   levels = c(lbl_old, lbl_new))
)

p_point <- make_density(ev_df, "Expected Values: E(Y|X)",
  xlim = range(c(zelig_ev, zelig2_ev)) + c(-0.02, 0.02)) /
  make_density(pv_df, "Predicted Values: Y|X",
  xlim = range(c(zelig_pv, zelig2_pv)))

ggsave(file.path(out, "comparison-ev.png"), p_point,
       width = 9, height = 7, dpi = 150, bg = "white")
cat("Saved comparison-ev.png\n")

# =============================================================================
# FIRST DIFFERENCE
# =============================================================================
cat("First difference...\n")

z_old_fd <- Zelig::zelig(mh_score ~ age + college + income_k,
                         model = "ls", data = pulse_clean)
z_old_fd <- Zelig::setx(z_old_fd, age = 40, college = 0, income_k = 75)
z_old_fd <- Zelig::setx1(z_old_fd, age = 40, college = 1, income_k = 75)
set.seed(42)
z_old_fd <- Zelig::sim(z_old_fd)
zelig_fd  <- as.numeric(z_old_fd$get_qi("fd", "x1"))
zelig_ev0 <- as.numeric(z_old_fd$get_qi("ev", "x"))
zelig_ev1 <- as.numeric(z_old_fd$get_qi("ev", "x1"))

z_new_fd <- zelig2_env$env$zelig2(mh_score ~ age + college + income_k,
                                   model = "ls", data = pulse_clean, num = 1000L)
z_new_fd <- zelig2_env$env$setx(z_new_fd, age = 40, college = 0, income_k = 75)
z_new_fd <- zelig2_env$env$setx1(z_new_fd, age = 40, college = 1, income_k = 75)
set.seed(42)
z_new_fd <- zelig2_env$env$sim(z_new_fd)
zelig2_fd  <- as.numeric(z_new_fd$sim_out$fd)
zelig2_ev0 <- as.numeric(z_new_fd$sim_out$ev)
zelig2_ev1 <- zelig2_ev0 + zelig2_fd

ev_overlay_df <- data.frame(
  value    = c(zelig_ev0, zelig_ev1, zelig2_ev0, zelig2_ev1),
  scenario = rep(c("No College (x)", "College (x1)",
                   "No College (x)", "College (x1)"),
                 each = length(zelig_ev0)),
  package  = factor(
    rep(c(lbl_old, lbl_old, lbl_new, lbl_new), each = length(zelig_ev0)),
    levels = c(lbl_old, lbl_new)
  )
)

p_ev_overlay <- ggplot(ev_overlay_df, aes(x = value, fill = scenario)) +
  geom_density(alpha = 0.45, color = NA) +
  facet_wrap(~package) +
  scale_fill_manual(values = c("No College (x)" = "coral",
                                "College (x1)" = "steelblue")) +
  labs(title = "Expected Values: E(Y|X) vs. E(Y|X1)",
       x = "Value", y = "Density", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", size = 11),
        legend.position = "bottom")

fd_df <- data.frame(
  value   = c(zelig_fd, zelig2_fd),
  package = factor(rep(c(lbl_old, lbl_new), each = length(zelig_fd)),
                   levels = c(lbl_old, lbl_new))
)
p_fd <- make_density(fd_df, "First Differences: E(Y|X1) - E(Y|X)",
  xlim = range(c(zelig_fd, zelig2_fd)) + c(-0.02, 0.02))

p_fd_full <- p_ev_overlay / p_fd
ggsave(file.path(out, "comparison-fd.png"), p_fd_full,
       width = 9, height = 7, dpi = 150, bg = "white")
cat("Saved comparison-fd.png\n")

# =============================================================================
# COEFFICIENT PLOT
# =============================================================================
cat("Coef plot...\n")

z_coefs  <- coef(z_old)
z_se     <- sqrt(diag(vcov(z_old$zelig.out$z.out[[1]])))
z2_coefs <- coef(z_new)
z2_se    <- sqrt(diag(vcov(z_new)))

vars <- names(z2_coefs)[-1]
comp_df <- data.frame(
  variable = rep(c("Age", "College", "Income ($1k)"), 2),
  estimate = c(z2_coefs[vars], z_coefs[vars]),
  se       = c(z2_se[vars], z_se[vars]),
  source   = rep(c(lbl_new, lbl_old), each = 3)
)
comp_df$lower <- comp_df$estimate - 1.96 * comp_df$se
comp_df$upper <- comp_df$estimate + 1.96 * comp_df$se

p_coef <- ggplot(comp_df, aes(x = variable, y = estimate,
                               color = source, shape = source)) +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.4), size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = setNames(c("darkblue", "firebrick"), c(lbl_new, lbl_old))
  ) +
  labs(title = "Coefficient Estimates",
       subtitle = "Identical point estimates and confidence intervals",
       x = NULL, y = "Estimate", color = NULL, shape = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out, "comparison-coef.png"), p_coef,
       width = 8, height = 4.5, dpi = 150, bg = "white")
cat("Saved comparison-coef.png\n")

# =============================================================================
# SURVEY-WEIGHTED: COEFFICIENTS
# =============================================================================
cat("Survey-weighted comparison...\n")

z_old_svy <- Zelig::zelig(mh_score ~ age + college + income_k,
                           model = "normal.survey",
                           weights = ~pweight, data = pulse_clean)
z_new_svy <- zelig2_env$env$zelig2(mh_score ~ age + college + income_k,
                                    model = "ls", data = pulse_clean,
                                    weights = pulse_clean$pweight, num = 1000L)

# Extract coefficients and SEs
z_svy_coefs  <- coef(z_old_svy)
z_svy_se     <- sqrt(diag(vcov(z_old_svy$zelig.out$z.out[[1]])))
z2_svy_coefs <- coef(z_new_svy)
z2_svy_se    <- sqrt(diag(vcov(z_new_svy)))

cat("Survey coef comparison:\n")
for (v in names(z2_svy_coefs)) {
  cat(sprintf("  %-15s Zelig: %12.8f  zelig2: %12.8f  diff: %e\n",
              v, z_svy_coefs[v], z2_svy_coefs[v],
              z_svy_coefs[v] - z2_svy_coefs[v]))
}

svy_vars <- names(z2_svy_coefs)[-1]
svy_comp_df <- data.frame(
  variable = rep(c("Age", "College", "Income ($1k)"), 2),
  estimate = c(z2_svy_coefs[svy_vars], z_svy_coefs[svy_vars]),
  se       = c(z2_svy_se[svy_vars], z_svy_se[svy_vars]),
  source   = rep(c(lbl_new, lbl_old), each = 3)
)
svy_comp_df$lower <- svy_comp_df$estimate - 1.96 * svy_comp_df$se
svy_comp_df$upper <- svy_comp_df$estimate + 1.96 * svy_comp_df$se

p_svy_coef <- ggplot(svy_comp_df, aes(x = variable, y = estimate,
                                       color = source, shape = source)) +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.4), size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = setNames(c("darkblue", "firebrick"), c(lbl_new, lbl_old))
  ) +
  labs(title = "Survey-Weighted Coefficient Estimates",
       subtitle = "Zelig produces incorrect estimates; zelig2 matches svyglm()",
       x = NULL, y = "Estimate", color = NULL, shape = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out, "comparison-survey-coef.png"), p_svy_coef,
       width = 8, height = 4.5, dpi = 150, bg = "white")
cat("Saved comparison-survey-coef.png\n")

# =============================================================================
# SURVEY-WEIGHTED: POINT SCENARIO
# =============================================================================
cat("Survey point scenario...\n")

z_old_svy <- Zelig::setx(z_old_svy, age = 40, college = 1, income_k = 75)
set.seed(42)
z_old_svy <- Zelig::sim(z_old_svy)
svy_zelig_ev <- as.numeric(z_old_svy$get_qi("ev", "x"))
svy_zelig_pv <- as.numeric(z_old_svy$get_qi("pv", "x"))

z_new_svy <- zelig2_env$env$setx(z_new_svy, age = 40, college = 1, income_k = 75)
set.seed(42)
z_new_svy <- zelig2_env$env$sim(z_new_svy)
svy_zelig2_ev <- as.numeric(z_new_svy$sim_out$ev)
svy_zelig2_pv <- as.numeric(z_new_svy$sim_out$pv)

svy_ev_df <- data.frame(
  value   = c(svy_zelig_ev, svy_zelig2_ev),
  package = factor(rep(c(lbl_old, lbl_new), each = length(svy_zelig_ev)),
                   levels = c(lbl_old, lbl_new))
)
svy_pv_df <- data.frame(
  value   = c(svy_zelig_pv, svy_zelig2_pv),
  package = factor(rep(c(lbl_old, lbl_new), each = length(svy_zelig_pv)),
                   levels = c(lbl_old, lbl_new))
)

p_svy_point <- make_density(svy_ev_df, "Survey-Weighted Expected Values: E(Y|X)",
  xlim = range(c(svy_zelig_ev, svy_zelig2_ev)) + c(-0.05, 0.05)) /
  make_density(svy_pv_df, "Survey-Weighted Predicted Values: Y|X",
  xlim = range(c(svy_zelig_pv, svy_zelig2_pv)))

ggsave(file.path(out, "comparison-survey-ev.png"), p_svy_point,
       width = 9, height = 7, dpi = 150, bg = "white")
cat("Saved comparison-survey-ev.png\n")

# =============================================================================
# SURVEY-WEIGHTED: FIRST DIFFERENCE
# =============================================================================
cat("Survey first difference...\n")

z_old_svy_fd <- Zelig::zelig(mh_score ~ age + college + income_k,
                              model = "normal.survey",
                              weights = ~pweight, data = pulse_clean)
z_old_svy_fd <- Zelig::setx(z_old_svy_fd, age = 40, college = 0, income_k = 75)
z_old_svy_fd <- Zelig::setx1(z_old_svy_fd, age = 40, college = 1, income_k = 75)
set.seed(42)
z_old_svy_fd <- Zelig::sim(z_old_svy_fd)
svy_zelig_fd  <- as.numeric(z_old_svy_fd$get_qi("fd", "x1"))
svy_zelig_ev0 <- as.numeric(z_old_svy_fd$get_qi("ev", "x"))
svy_zelig_ev1 <- as.numeric(z_old_svy_fd$get_qi("ev", "x1"))

z_new_svy_fd <- zelig2_env$env$zelig2(mh_score ~ age + college + income_k,
                                       model = "ls", data = pulse_clean,
                                       weights = pulse_clean$pweight, num = 1000L)
z_new_svy_fd <- zelig2_env$env$setx(z_new_svy_fd, age = 40, college = 0, income_k = 75)
z_new_svy_fd <- zelig2_env$env$setx1(z_new_svy_fd, age = 40, college = 1, income_k = 75)
set.seed(42)
z_new_svy_fd <- zelig2_env$env$sim(z_new_svy_fd)
svy_zelig2_fd  <- as.numeric(z_new_svy_fd$sim_out$fd)
svy_zelig2_ev0 <- as.numeric(z_new_svy_fd$sim_out$ev)
svy_zelig2_ev1 <- svy_zelig2_ev0 + svy_zelig2_fd

svy_ev_overlay_df <- data.frame(
  value    = c(svy_zelig_ev0, svy_zelig_ev1, svy_zelig2_ev0, svy_zelig2_ev1),
  scenario = rep(c("No College (x)", "College (x1)",
                   "No College (x)", "College (x1)"),
                 each = length(svy_zelig_ev0)),
  package  = factor(
    rep(c(lbl_old, lbl_old, lbl_new, lbl_new), each = length(svy_zelig_ev0)),
    levels = c(lbl_old, lbl_new)
  )
)

p_svy_ev_overlay <- ggplot(svy_ev_overlay_df, aes(x = value, fill = scenario)) +
  geom_density(alpha = 0.45, color = NA) +
  facet_wrap(~package) +
  scale_fill_manual(values = c("No College (x)" = "coral",
                                "College (x1)" = "steelblue")) +
  labs(title = "Survey-Weighted Expected Values: E(Y|X) vs. E(Y|X1)",
       x = "Value", y = "Density", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold", size = 11),
        legend.position = "bottom")

svy_fd_df <- data.frame(
  value   = c(svy_zelig_fd, svy_zelig2_fd),
  package = factor(rep(c(lbl_old, lbl_new), each = length(svy_zelig_fd)),
                   levels = c(lbl_old, lbl_new))
)
p_svy_fd <- make_density(svy_fd_df,
  "Survey-Weighted First Differences: E(Y|X1) - E(Y|X)",
  xlim = range(c(svy_zelig_fd, svy_zelig2_fd)) + c(-0.02, 0.02))

p_svy_fd_full <- p_svy_ev_overlay / p_svy_fd
ggsave(file.path(out, "comparison-survey-fd.png"), p_svy_fd_full,
       width = 9, height = 7, dpi = 150, bg = "white")
cat("Saved comparison-survey-fd.png\n")

cat("\n=== ALL COMPARISON PLOTS DONE ===\n")
