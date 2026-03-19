# Script 7: GA Preference Manipulation Check
# Purpose: Test whether including "preference for general anesthesia" in the
#          system prompt increases GA recommendations compared to control.

# 1. SETUP

setwd("/Users/robertchen/Documents/GitHub/LLM_hip/analysis")

library(arm)          # Penalized GLM (bayesglm)
library(performance)  # Model diagnostics
library(DHARMa)       # Residual diagnostics
library(car)          # VIF
library(tidyverse)    # Data manipulation
library(ggplot2)      # Plotting
library(patchwork)    # Combining plots

dir.create("output/07_ga_preference/models", showWarnings = FALSE, recursive = TRUE)
dir.create("output/07_ga_preference/diagnostics", showWarnings = FALSE, recursive = TRUE)
dir.create("output/07_ga_preference/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/07_ga_preference/tables", showWarnings = FALSE, recursive = TRUE)

figure_dpi <- 600

format_model_name <- function(x) {
  model_display <- case_when(
    x == "deepseek" ~ "DeepSeek 3.2",
    x == "gpt-5-2025-08-07" ~ "GPT-5",
    x == "gpt-5-mini-2025-08-07" ~ "GPT-5 mini",
    x == "gpt-5-nano-2025-08-07" ~ "GPT-5 nano",
    x == "gemini-2.5-flash-preview-09-2025" ~ "Gemini 2.5 Flash",
    TRUE ~ x
  )
  return(model_display)
}

# 2. LOAD DATA

data <- readRDS("./data/prepared_data.rds")

cat("Data dimensions:", dim(data), "\n")
cat("Unique models:", unique(data$model_name), "\n")

# 3. MODEL FITTING

# CONFIGURATION
RUN_SINGLE_MODEL <- FALSE
SELECTED_MODEL <- "deepseek"

all_models <- c("deepseek", "gpt-5-2025-08-07", "gpt-5-mini-2025-08-07",
                "gpt-5-nano-2025-08-07", "gemini-2.5-flash-preview-09-2025")

models <- if (RUN_SINGLE_MODEL) SELECTED_MODEL else all_models

all_diagnostics_list <- list()

cat("GA PREFERENCE MANIPULATION CHECK - PROCESSING", length(models), "MODELS\n")

for (current_model in models) {

  tryCatch({

    output_subdir <- paste0("output/07_ga_preference/diagnostics/", current_model)
    dir.create(output_subdir, showWarnings = FALSE, recursive = TRUE)

    console_output_file <- paste0(output_subdir, "/console_output.txt")
    sink(file = console_output_file, split = TRUE)

    cat("Model:", current_model, "\n")
    cat("Output directory:", output_subdir, "\n")

    # Filter for preference_ga and control only
    model_data <- data %>%
      filter(model_name == current_model,
             variable %in% c("preference_ga", "control")) %>%
      droplevels()

    cat("\nSample size:", nrow(model_data), "\n")
    cat("Variables included:", levels(model_data$variable), "\n")

    outcome_table <- table(model_data$anesthetic_plan_binary, model_data$variable)
    cat("\nOutcome distribution by variable:\n")
    print(outcome_table)
    cat("\nOutcome proportions:\n")
    print(round(prop.table(outcome_table, margin = 2), 3))

    # 4. FIT MODEL

    cat("\n--- Fitting penalized logistic regression (bayesglm) ---\n")

    formula_binary <- as.formula("anesthetic_plan_binary ~ variable + surgery_id + sex")

    cat("Formula:", deparse(formula_binary), "\n")
    cat("Prior: Cauchy (Student's t, df=1)\n")
    cat("  Intercept: scale=10, df=1\n")
    cat("  Coefficients: scale=2.5, df=1\n\n")

    model_fit <- tryCatch({
      bayesglm(
        formula = formula_binary,
        data = model_data,
        family = binomial(link = "logit"),
        prior.df = 1,
        prior.scale = 2.5,
        prior.df.for.intercept = 1,
        prior.scale.for.intercept = 10,
        maxit = 100
      )
    }, error = function(e) {
      cat("ERROR in model fitting:\n")
      cat(e$message, "\n")
      return(NULL)
    })

    if (is.null(model_fit)) {
      cat("Model fitting failed for", current_model, "\n")
      cat("Skipping to next model.\n")
      sink()
      next
    }

    cat("Model fitting completed.\n")

    # 5. CONVERGENCE DIAGNOSTICS

    cat("CONVERGENCE DIAGNOSTICS\n")

    is_boundary <- if (!is.null(model_fit$boundary)) model_fit$boundary else FALSE

    convergence_status <- list(
      converged = model_fit$converged && !is_boundary,
      convergence_code = ifelse(model_fit$converged && !is_boundary, 0, 1),
      iterations = model_fit$iter,
      boundary = is_boundary
    )

    cat("\nConverged:", convergence_status$converged, "\n")
    cat("Convergence code:", convergence_status$convergence_code, "(0 = converged)\n")
    cat("Iterations used:", convergence_status$iterations, "\n")

    if (!model_fit$converged) {
      cat("\nWARNING: Model did not converge\n")
    }

    if (is_boundary) {
      cat("\nWARNING: Model fitted on the boundary\n")
    }

    cat("\n--- Model Summary ---\n")
    print(summary(model_fit))

    # 6. EFFECT SIZE CALCULATION

    cat("EFFECT SIZE: PREFERENCE_GA vs CONTROL\n")

    newdata_control <- model_data %>%
      mutate(variable = factor("control", levels = c("control", "preference_ga")))

    newdata_pref <- model_data %>%
      mutate(variable = factor("preference_ga", levels = c("control", "preference_ga")))

    pred_control <- predict(model_fit, newdata = newdata_control, type = "response")
    pred_pref <- predict(model_fit, newdata = newdata_pref, type = "response")

    risk_control <- mean(pred_control)
    risk_pref <- mean(pred_pref)
    risk_diff <- risk_pref - risk_control

    pred_control_mat <- matrix(pred_control, nrow = length(pred_control), ncol = 1)
    pred_pref_mat <- matrix(pred_pref, nrow = length(pred_pref), ncol = 1)
    risk_diff_se <- sqrt(var(pred_pref_mat - pred_control_mat) / nrow(model_data))
    risk_diff_ci_lower <- risk_diff - 1.96 * risk_diff_se
    risk_diff_ci_upper <- risk_diff + 1.96 * risk_diff_se

    z_stat <- risk_diff / risk_diff_se
    risk_diff_p <- 2 * pnorm(-abs(z_stat))

    cat("\nRisk Difference (preference_ga - control):\n")
    cat("  Risk (control) =", round(risk_control, 3), "\n")
    cat("  Risk (preference_ga) =", round(risk_pref, 3), "\n")
    cat("  Risk Difference =", round(risk_diff, 3), "\n")
    cat("  95% CI: [", round(risk_diff_ci_lower, 3), ",", round(risk_diff_ci_upper, 3), "]\n")
    cat("  p-value =", format.pval(risk_diff_p, digits = 3), "\n")

    # 7. OVERDISPERSION DIAGNOSTICS

    cat("OVERDISPERSION DIAGNOSTICS\n")

    overdisp_test <- check_overdispersion(model_fit)
    cat("\n")
    print(overdisp_test)

    # 8. DHARMa RESIDUAL DIAGNOSTICS

    cat("DHARMa RESIDUAL DIAGNOSTICS\n")

    cat("\nGenerating simulated datasets for DHARMa\n")
    nsim <- 1000

    obs_numeric <- as.integer(model_data$anesthetic_plan_binary == "neuraxial")

    sims_raw <- simulate(model_fit, nsim = nsim)
    sims_df <- as.data.frame(sims_raw)

    if (is.factor(sims_df[[1]]) || is.character(sims_df[[1]])) {
      sims_numeric <- as.matrix(sapply(sims_df, function(x) as.integer(x == "neuraxial")))
    } else {
      sims_numeric <- as.matrix(sims_df)
    }

    if (!all(sims_numeric %in% c(0, 1))) {
      stop("Simulations are not 0/1 binary values")
    }

    cat("\nCreating DHARMa object\n")
    sim_resid <- createDHARMa(
      simulatedResponse = sims_numeric,
      observedResponse  = obs_numeric,
      fittedPredictedResponse = predict(model_fit, type = "response"),
      integerResponse = TRUE
    )

    uniformity_test <- testUniformity(sim_resid, plot = FALSE)
    dispersion_test <- testDispersion(sim_resid, plot = FALSE)
    outlier_test <- testOutliers(sim_resid, plot = FALSE)

    cat("\n--- Uniformity Test ---\n");  print(uniformity_test)
    cat("\n--- Dispersion Test ---\n");  print(dispersion_test)
    cat("\n--- Outlier Test ---\n");     print(outlier_test)

    dharma_tests <- list(
      simulated_residuals = sim_resid,
      uniformity = uniformity_test,
      dispersion = dispersion_test,
      outliers = outlier_test
    )

    # 9. VIF

    cat("VARIANCE INFLATION FACTORS (VIF)\n")

    vif_values <- tryCatch({
      car::vif(model_fit)
    }, error = function(e) {
      cat("Unable to calculate VIF:\n")
      cat(e$message, "\n")
      return(NULL)
    })

    if (!is.null(vif_values)) {
      cat("\n")
      print(vif_values)
    }

    # 10. PLUG-IN PREDICTIVE CHECK

    cat("PLUG-IN PREDICTIVE CHECK\n")

    obs_rate <- mean(obs_numeric)
    sim_rates <- colMeans(sims_numeric)

    ppc_df <- data.frame(sim_rate = sim_rates)
    ppc_plot <- ggplot(ppc_df, aes(x = sim_rate)) +
      geom_histogram(bins = 40, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = obs_rate, linetype = "dashed", linewidth = 1) +
      labs(
        title = "Plug-in Predictive Check: Neuraxial Event Rate",
        x = "Simulated event rate",
        y = "Count",
        subtitle = paste0("Observed rate = ", round(obs_rate, 3))
      ) +
      theme_minimal()

    ppc_plot_file <- file.path(output_subdir, "predictive_check.jpg")
    ggsave(ppc_plot_file, plot = ppc_plot, width = 10, height = 6, dpi = figure_dpi, bg = "white")
    cat("Saved:", ppc_plot_file, "\n")

    # 11. DIAGNOSTIC PLOTS

    cat("GENERATING DIAGNOSTIC PLOTS\n")

    dharma_plot_file <- file.path(output_subdir, "dharma_diagnostics.jpg")
    jpeg(dharma_plot_file, width = 14, height = 8, units = "in", res = figure_dpi, quality = 100)

    op <- par(no.readonly = TRUE)
    par(oma = c(0, 0, 4, 0))
    plot(sim_resid)
    mtext(paste0(current_model, " - GA Preference Check"),
          outer = TRUE, line = 1, cex = 1.2)
    par(op)

    dev.off()
    cat("Saved:", dharma_plot_file, "\n")

    # 12. DIAGNOSTIC SUMMARY TABLE

    cat("CREATING DIAGNOSTIC SUMMARY TABLE\n")

    diagnostic_summary <- data.frame(
      Model_Type = "bayesglm (MAP)",
      Model = current_model,
      Outcome = "anesthetic_plan_binary",
      N = nrow(model_data),
      Converged = convergence_status$converged,
      Convergence_code = convergence_status$convergence_code,
      Iterations = convergence_status$iterations,
      Boundary_fit = convergence_status$boundary,
      Risk_Control = risk_control,
      Risk_Preference = risk_pref,
      Risk_Difference = risk_diff,
      RD_CI_lower = risk_diff_ci_lower,
      RD_CI_upper = risk_diff_ci_upper,
      RD_p_value = risk_diff_p,
      Overdispersion_ratio = overdisp_test$dispersion_ratio,
      Overdispersion_p = overdisp_test$p_value,
      DHARMa_uniformity_p = uniformity_test$p.value,
      DHARMa_dispersion_p = dispersion_test$p.value,
      DHARMa_outliers_p = outlier_test$p.value,
      stringsAsFactors = FALSE
    )

    print(diagnostic_summary)

    diag_table_file <- paste0(output_subdir, "/diagnostics_summary.csv")
    write.csv(diagnostic_summary, file = diag_table_file, row.names = FALSE)
    cat("\nSaved:", diag_table_file, "\n")

    all_diagnostics_list[[current_model]] <- diagnostic_summary

    # 13. SAVE MODEL AND RESULTS

    model_filename <- paste0("output/07_ga_preference/models/", current_model, "_ga_preference_bayesian.rds")
    saveRDS(model_fit, file = model_filename)
    cat("Saved model:", model_filename, "\n")

    results_list <- list(
      model = model_fit,
      model_info = list(
        model_type = "bayesglm (MAP)",
        model_name = current_model,
        outcome = "anesthetic_plan_binary",
        formula = formula_binary,
        sample_size = nrow(model_data),
        prior_info = list(
          prior_df = 1,
          prior_scale = 2.5,
          prior_df_intercept = 1,
          prior_scale_intercept = 10,
          reference = "Gelman et al. 2008"
        )
      ),
      effect_sizes = list(
        risk_control = risk_control,
        risk_preference = risk_pref,
        risk_difference = risk_diff,
        rd_ci = c(risk_diff_ci_lower, risk_diff_ci_upper)
      ),
      diagnostics = list(
        convergence = convergence_status,
        overdispersion = overdisp_test,
        dharma = dharma_tests,
        vif = vif_values,
        summary_table = diagnostic_summary
      )
    )

    results_filename <- paste0(output_subdir, "/model_results.rds")
    saveRDS(results_list, file = results_filename)
    cat("Saved results:", results_filename, "\n")

    cat("ITERATION COMPLETE:", current_model, "\n")

    sink()

  }, error = function(e) {
    cat("\n!!! FATAL ERROR:", current_model, "!!!\n")
    cat("Error:", e$message, "\n")
    cat("Continuing to next model\n\n")

    if (sink.number() > 0) sink()
  })

}

# FINAL SUMMARY AND COMBINED OUTPUTS

cat("GA PREFERENCE MANIPULATION CHECK COMPLETE\n")

if (length(all_diagnostics_list) > 0) {
  combined_diagnostics <- bind_rows(all_diagnostics_list)

  combined_diag_file <- "output/07_ga_preference/tables/ga_preference_summary.csv"
  write.csv(combined_diagnostics, file = combined_diag_file, row.names = FALSE)
  cat("\nCombined summary saved to:", combined_diag_file, "\n")

  cat("\n--- Summary of All Models ---\n")
  print(combined_diagnostics %>%
          select(Model, N, Converged, Risk_Difference, RD_CI_lower, RD_CI_upper, RD_p_value, DHARMa_uniformity_p))

  n_total <- length(models)
  n_processed <- nrow(combined_diagnostics)
  n_converged <- sum(combined_diagnostics$Converged)

  cat("\n--- Processing Summary ---\n")
  cat("Total models:", n_total, "\n")
  cat("Successfully processed:", n_processed, "\n")
  cat("Successfully converged:", n_converged, "\n")

  if (n_processed < n_total) {
    cat("\nWARNING:", n_total - n_processed, "model(s) failed\n")
  }
  if (n_converged < n_processed) {
    cat("WARNING:", n_processed - n_converged, "model(s) did not converge\n")
  }

  cat("CREATING COMBINED FOREST PLOT\n")

  plot_data <- combined_diagnostics %>%
    mutate(
      model_display = format_model_name(Model),
      model_display = factor(model_display,
                             levels = c("GPT-5 nano", "GPT-5 mini", "GPT-5",
                                       "Gemini 2.5 Flash", "DeepSeek 3.2")),
      RD_pct = Risk_Difference * 100,
      RD_LCL_pct = RD_CI_lower * 100,
      RD_UCL_pct = RD_CI_upper * 100,
      sig_label = case_when(
        RD_p_value < 0.001 ~ "***",
        RD_p_value < 0.01  ~ "**",
        RD_p_value < 0.05  ~ "*",
        TRUE ~ ""
      )
    ) %>%
    arrange(model_display)

  forest_plot <- ggplot(plot_data, aes(y = model_display)) +
    geom_vline(xintercept = c(-5, 5), linetype = "dotted",
               color = "steelblue", linewidth = 0.6, alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = RD_LCL_pct, xmax = RD_UCL_pct),
                   height = 0, linewidth = 0.5) +
    geom_point(aes(x = RD_pct), size = 3) +
    geom_text(aes(x = RD_UCL_pct, label = sig_label),
              hjust = 0, size = 4, show.legend = FALSE, nudge_x = 0.5) +
    labs(
      title = "Recommendation for neuraxial anesthesia with institutional GA preference",
      x = "Absolute risk difference (percentage points)",
      y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 14),
      axis.text.y = element_text(size = 13),
      axis.text.x = element_text(size = 13),
      axis.title = element_text(size = 13),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )

  forest_plot_file <- "output/07_ga_preference/figures/ga_preference_forest_plot.jpg"
  ggsave(forest_plot_file, forest_plot, width = 10, height = 6, dpi = figure_dpi, bg = "white")
  cat("Saved combined forest plot to:", forest_plot_file, "\n")

  cat("CREATING GA USAGE TABLE\n")

  ga_usage_table <- data %>%
    filter(variable %in% c("control", "preference_ga")) %>%
    group_by(model_name, variable, anesthetic_plan_binary) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name, variable) %>%
    mutate(
      total = sum(n),
      percentage = n / total * 100
    ) %>%
    filter(anesthetic_plan_binary == "general") %>%
    select(model_name, variable, percentage) %>%
    pivot_wider(names_from = variable, values_from = percentage,
                names_prefix = "GA_pct_", values_fill = 0) %>%
    mutate(
      model_display = format_model_name(model_name),
      difference = GA_pct_preference_ga - GA_pct_control
    ) %>%
    select(Model = model_display,
           Control_GA_pct = GA_pct_control,
           Preference_GA_pct = GA_pct_preference_ga,
           Difference = difference) %>%
    arrange(Model)

  ga_usage_file <- "output/07_ga_preference/tables/ga_usage_by_condition.csv"
  write.csv(ga_usage_table, file = ga_usage_file, row.names = FALSE)
  cat("Saved GA usage table to:", ga_usage_file, "\n")

  cat("\n--- GA Usage by Condition ---\n")
  print(ga_usage_table)

} else {
  cat("\nWARNING: No models were successfully processed!\n")
}
