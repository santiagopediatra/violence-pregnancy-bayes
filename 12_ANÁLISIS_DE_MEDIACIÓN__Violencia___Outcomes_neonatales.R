# =============================================================================
# SCRIPT 02 CORRECTED: GLM vs FIRTH vs BAYES COMPARISON
# Table 3: 12 combinations (4 outcomes × 3 trimesters)
# Starting from: BASE_HASTA_2024 (already loaded in session)
# =============================================================================

library(tidyverse)
library(logistf)
library(brms)
library(posterior)
library(bayesplot)

set.seed(2024)
options(mc.cores = parallel::detectCores())

# =============================================================================
# 1. USE EXISTING DATASET
# =============================================================================
# Assumes BASE_HASTA_2024 is already in the R environment

df <- BASE_HASTA_2024

cat("Dataset loaded from environment: BASE_HASTA_2024\n")
cat("  Dimensions:", nrow(df), "rows ×", ncol(df), "columns\n")
cat("  Variables:", paste(names(df)[1:10], collapse = ", "), "...\n\n")

# =============================================================================
# 2. DEFINE PRIORS (Identical across all Bayesian models)
# =============================================================================

priors_default <- c(
  prior(normal(0, 2.5),       class = "b"),
  prior(student_t(3, 0, 2.5), class = "Intercept")
)

cat("PRIORS (applied to all Bayesian models):\n")
cat("  Coefficients: Normal(μ=0, σ=2.5)\n")
cat("  Intercept: Student-t(ν=3, μ=0, σ=2.5)\n\n")

# =============================================================================
# 3. DEFINE STUDY VARIABLES
# =============================================================================

outcomes_list <- list(
  PREMATURO.CODIGO = "Preterm birth",
  RCIU = "SGA/IUGR",
  CODIGO.Apgar.1.minuto..7 = "Apgar <7 at 1 min",
  LACTANCIA_EXCLUSIVA_SI_NO = "Exclusive breastfeeding"
)

exposure_map <- c(
  "Violencia.1er." = "T1", 
  "Violencia.2do." = "T2", 
  "Violencia.3er." = "T3"
)

# Confounders: demographic characteristics only
confounders <- c(
  "Edad.materna", 
  "ESCOLARIDAD.BAJA", 
  "PAREJA.ESTABLE.CODIGO",
  "ETNIA.MINORITARIA", 
  "Gestas.previas.CODIGO", 
  "Número.Consultas.prenatales"
)

# Mediators (included in the model)
mediators <- c("Alcohol.CODIGO", "DROGAS", "TABACO.PASIVO")

# All 9 variables
all_covars <- c(confounders, mediators)
covars_str <- paste(all_covars, collapse = " + ")

cat("OUTCOMES:\n")
for (code in names(outcomes_list)) {
  cat(sprintf("  %s: %s\n", code, outcomes_list[[code]]))
}

cat("\nEXPOSURES (Violence by trimester):\n")
for (var in names(exposure_map)) {
  cat(sprintf("  %s: %s\n", var, exposure_map[[var]]))
}

cat("\nADJUSTED VARIABLES (9 total):\n")
cat("  Confounders (6): age, education, partner stability, ethnicity, parity, prenatal visits\n")
cat("  Mediators (3): alcohol, drugs, tobacco\n\n")

# =============================================================================
# 4. CREATE ANALYSIS GRID (12 combinations)
# =============================================================================

combinaciones <- expand_grid(
  outcome_code = names(outcomes_list),
  exposure_var = names(exposure_map)
) |>
  mutate(
    outcome_label = map_chr(outcome_code, ~outcomes_list[[.]]),
    trimester = exposure_map[exposure_var]
  )

cat("ANALYSIS GRID (", nrow(combinaciones), "combinations):\n")
cat(strrep("-", 80), "\n")
print(as.data.frame(combinaciones))
cat(strrep("-", 80), "\n\n")

# =============================================================================
# 5. RUN COMPARISON: GLM vs FIRTH vs BAYESIAN
# =============================================================================

cat("Running 12 analyses (GLM → Firth → Bayesian for each)...\n")
cat("Estimated time: 2-3 hours\n")
cat(strrep("=", 80), "\n\n")

resultados <- list()
iteration <- 1

for (i in seq_len(nrow(combinaciones))) {
  
  outcome_code <- combinaciones$outcome_code[i]
  outcome_label <- combinaciones$outcome_label[i]
  exposure_var <- combinaciones$exposure_var[i]
  trimester <- combinaciones$trimester[i]
  
  # Build formula
  formula_str <- paste(outcome_code, "~", exposure_var, "+", covars_str)
  formula_obj <- as.formula(formula_str)
  
  cat(sprintf("[%2d/12 | %s] %s at %s\n", 
              iteration, format(Sys.time(), "%H:%M:%S"), 
              outcome_label, trimester))
  
  # =========================================================================
  # Check for separation (affects GLM reliability)
  # =========================================================================
  sep_check <- tryCatch({
    m_test <- glm(formula_obj, data = df, family = binomial())
    any(is.na(coef(m_test)))
  }, error = function(e) NA)
  
  sep_flag <- isTRUE(sep_check)
  
  if (sep_flag) {
    cat("  ⚠ WARNING: Separation detected (GLM unreliable)\n")
  }
  
  # =========================================================================
  # METHOD 1: GLM (Classical maximum likelihood)
  # =========================================================================
  cat("  • GLM...", file = stderr())
  m_glm <- tryCatch(
    glm(formula_obj, data = df, family = binomial()),
    error = function(e) NULL
  )
  
  if (!is.null(m_glm) && !sep_flag) {
    coef_summary <- coef(summary(m_glm))[exposure_var, ]
    glm_row <- tibble(
      method = "GLM",
      OR = exp(coef_summary[1]),
      ci_lower = exp(coef_summary[1] - 1.96 * coef_summary[2]),
      ci_upper = exp(coef_summary[1] + 1.96 * coef_summary[2]),
      p_value = coef_summary[4],
      p_OR_gt1 = NA_real_,
      rhat = NA_real_,
      ess_bulk = NA_real_,
      convergence = "OK"
    )
    cat(" ✓\n", file = stderr())
  } else {
    glm_row <- tibble(
      method = "GLM",
      OR = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
      p_value = NA_real_, p_OR_gt1 = NA_real_, rhat = NA_real_,
      ess_bulk = NA_real_,
      convergence = if (sep_flag) "Separation" else "Error"
    )
    cat(" ✗\n", file = stderr())
  }
  
  # =========================================================================
  # METHOD 2: Firth's Logistic Regression (bias-reduced)
  # =========================================================================
  cat("  • Firth...", file = stderr())
  m_firth <- tryCatch(
    logistf(formula_obj, data = df, firth = TRUE, pl = TRUE),
    error = function(e) NULL
  )
  
  if (!is.null(m_firth)) {
    firth_row <- tibble(
      method = "Firth",
      OR = exp(m_firth$coefficients[exposure_var]),
      ci_lower = exp(m_firth$ci.lower[exposure_var]),
      ci_upper = exp(m_firth$ci.upper[exposure_var]),
      p_value = m_firth$prob[exposure_var],
      p_OR_gt1 = NA_real_,
      rhat = NA_real_,
      ess_bulk = NA_real_,
      convergence = "OK"
    )
    cat(" ✓\n", file = stderr())
  } else {
    firth_row <- tibble(
      method = "Firth",
      OR = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
      p_value = NA_real_, p_OR_gt1 = NA_real_, rhat = NA_real_,
      ess_bulk = NA_real_, convergence = "Error"
    )
    cat(" ✗\n", file = stderr())
  }
  
  # =========================================================================
  # METHOD 3: Bayesian (brms with default priors)
  # =========================================================================
  cat("  • Bayesian...", file = stderr())
  m_bayes <- tryCatch(
    brm(
      formula_obj,
      data = df,
      family = bernoulli(link = "logit"),
      prior = priors_default,
      chains = 4,
      iter = 6000,
      warmup = 2000,
      cores = 4,
      seed = 2024,
      control = list(adapt_delta = 0.97, max_treedepth = 15),
      refresh = 0,
      silent = 2
    ),
    error = function(e) NULL
  )
  
  if (!is.null(m_bayes)) {
    posterior_draws <- as_draws_df(m_bayes)[[paste0("b_", exposure_var)]]
    or_median <- exp(median(posterior_draws))
    
    # Extract Rhat correctly
    bayes_summary <- summary(m_bayes)
    rhat_exposure <- bayes_summary$fixed[exposure_var, "Rhat"]
    rhat_max <- if (is.na(rhat_exposure)) NA_real_ else rhat_exposure
    
    # Extract ESS
    ess_exposure <- bayes_summary$fixed[exposure_var, "Bulk_ESS"]
    ess_min <- if (is.na(ess_exposure)) NA_real_ else ess_exposure
    
    bayes_row <- tibble(
      method = "Bayesian",
      OR = or_median,
      ci_lower = exp(quantile(posterior_draws, 0.025)),
      ci_upper = exp(quantile(posterior_draws, 0.975)),
      p_value = NA_real_,
      p_OR_gt1 = mean(posterior_draws > 0),
      rhat = rhat_max,
      ess_bulk = ess_min,
      convergence = if (is.na(rhat_max)) "NA" else if (rhat_max < 1.01) "Converged" else "Check Rhat"
    )
    cat(" ✓\n", file = stderr())
  } else {
    bayes_row <- tibble(
      method = "Bayesian",
      OR = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
      p_value = NA_real_, p_OR_gt1 = NA_real_, rhat = NA_real_,
      ess_bulk = NA_real_, convergence = "Error"
    )
    cat(" ✗\n", file = stderr())
  }
  
  # =========================================================================
  # Combine results
  # =========================================================================
  combo_result <- bind_rows(glm_row, firth_row, bayes_row) |>
    mutate(
      outcome = outcome_code,
      outcome_label = outcome_label,
      trimester = trimester,
      exposure = exposure_var,
      .before = method
    )
  
  resultados[[paste0(outcome_code, "_", trimester)]] <- combo_result
  
  iteration <- iteration + 1
}

# =============================================================================
# 6. COMPILE FINAL COMPARISON TABLE
# =============================================================================

tabla_final <- bind_rows(resultados) |>
  select(outcome, outcome_label, trimester, method, 
         OR, ci_lower, ci_upper, p_value, p_OR_gt1, rhat, ess_bulk, convergence)

# =============================================================================
# 7. EXPORT RESULTS
# =============================================================================

write_csv(tabla_final, "tabla_comparativa_metodos_table3.csv")

cat("\n", strrep("=", 90), "\n")
cat("✓✓✓ TABLE 3: COMPARISON ANALYSIS COMPLETE ✓✓✓\n")
cat(strrep("=", 90), "\n\n")

cat("Results exported to: tabla_comparativa_metodos_table3.csv\n\n")

# =============================================================================
# 8. DISPLAY SUMMARY
# =============================================================================

cat("SUMMARY TABLE: Method Comparison (n=12 analyses)\n")
cat(strrep("-", 90), "\n")

tabla_display <- tabla_final |>
  mutate(
    OR_ci = sprintf("%.2f (%.2f–%.2f)", OR, ci_lower, ci_upper)
  ) |>
  select(outcome_label, trimester, method, OR_ci, p_value, convergence) |>
  pivot_wider(
    names_from = method,
    values_from = OR_ci,
    id_cols = c(outcome_label, trimester)
  )

print(tabla_display)

cat("\n")
cat("BAYESIAN CONVERGENCE DIAGNOSTICS (Rhat and ESS):\n")
cat(strrep("-", 90), "\n")

bayes_only <- tabla_final |>
  filter(method == "Bayesian") |>
  select(outcome_label, trimester, rhat, ess_bulk, convergence)

print(bayes_only)

cat("\nInterpretation:\n")
cat("  • Rhat < 1.01: Chains converged (good)\n")
cat("  • ESS > 400: Sufficient effective sample size (good)\n")
cat("  • Convergence = 'Converged': Model is reliable\n\n")

cat(strrep("=", 90), "\n")
cat("Session info:\n")
cat("  Dataset:", nrow(df), "records\n")
cat("  Analyses:", nrow(tabla_final), "rows (3 methods × 12 combinations)\n")
cat("  Completion time:", format(Sys.time()), "\n")
cat(strrep("=", 90), "\n")