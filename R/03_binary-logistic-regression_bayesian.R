# Script 3: Penalized MLE Binary Logistic Regression (bayesglm with MAP estimation)
# Purpose: Fit binary logistic regression models using bayesglm (MAP estimation
#          with Cauchy prior) for LLM recommendation outcomes.

# 1. SETUP 

# Load required packages
library(arm)          # Penalized GLM (bayesglm = MAP estimation with Cauchy prior)
library(performance)   # Model diagnostics
library(DHARMa)       # Residual diagnostics
library(car)          # VIF and other diagnostics
library(tidyverse)    # Data manipulation
library(data.table)   # Efficient data handling
library(ggplot2)      # Plotting
library(patchwork)    # Combining plots

# Create output directories if they don't exist
dir.create("models", showWarnings = FALSE)
dir.create("output/diagnostics", showWarnings = FALSE, recursive = TRUE)

# 2. LOAD DATA

data <- readRDS("./data/prepared_data.rds")

data <- data %>%
  filter(variable != "preference_ga") %>%
  droplevels()

cat("Data dimensions:", dim(data), "\n")
cat("Unique models:", unique(data$model_name), "\n")
cat("\nOutcome distribution for anesthetic_plan_binary:\n")
print(table(data$anesthetic_plan_binary, useNA = "ifany"))

# 3. MODEL FITTING

# CONFIGURATION
RUN_SINGLE_MODEL <- FALSE  # Set TRUE to run single model only
SELECTED_MODEL <- "deepseek"  # Model to run if RUN_SINGLE_MODEL = TRUE

all_models <- c("deepseek", "gpt-5-2025-08-07", "gpt-5-mini-2025-08-07",
                "gpt-5-nano-2025-08-07", "gemini-2.5-flash-preview-09-2025")
outcomes <- c("nerve_block_binary", "anesthetic_plan_binary", "arterial_line_binary")

models <- if (RUN_SINGLE_MODEL) SELECTED_MODEL else all_models

# Event levels for each outcome
event_levels <- list(
  nerve_block_binary = "recommend",
  anesthetic_plan_binary = "neuraxial",
  arterial_line_binary = "recommend"
)

all_diagnostics_list <- list()

cat("\n==\n")
cat("PROCESSING", length(models), "MODELS ×", length(outcomes), "OUTCOMES =",
    length(models) * length(outcomes), "TOTAL COMBINATIONS\n")
cat("==\n\n")

# Loop through all model-outcome combinations
for (current_model in models) {
  for (current_outcome in outcomes) {

    # Wrap entire iteration in tryCatch to ensure script continues if one fails
    tryCatch({

      # Get the event level for this outcome
      event_level <- event_levels[[current_outcome]]

      # Create output subdirectory for this analysis
      output_subdir <- paste0("output/diagnostics/", current_model, "_",
                              current_outcome, "_bayesian")
      dir.create(output_subdir, showWarnings = FALSE, recursive = TRUE)

      # Start capturing console output
      console_output_file <- paste0(output_subdir, "/console_output.txt")
      sink(file = console_output_file, split = TRUE)

      cat("\n==\n")
      cat("Fitting model for:", current_model, "-", current_outcome, "\n")
      cat("Event level:", event_level, "\n")
      cat("Output directory:", output_subdir, "\n")
      cat("==\n")

      # Filter data for current model
      model_data <- data %>%
        filter(model_name == current_model) %>%
        droplevels()

      cat("\nSample size:", nrow(model_data), "\n")

      outcome_table <- table(model_data[[current_outcome]])
      cat("\nOutcome distribution:\n")
      print(outcome_table)
      cat("\nOutcome proportions:\n")
      print(round(prop.table(outcome_table), 3))

      # 4. FIT MODEL

      cat("\n--- Fitting penalized logistic regression (bayesglm) ---\n")

      # Model formula (no random effects - replicates are independent samples)
      formula_binary <- as.formula(paste0(
        current_outcome,
        " ~ variable + surgery_id + sex"
      ))

      cat("Formula:", deparse(formula_binary), "\n")
      cat("Prior: Cauchy (Student's t, df=1) - Gelman et al. 2008\n")
      cat("  Intercept: scale=10, df=1\n")
      cat("  Coefficients: scale=2.5, df=1\n\n")

      # Fit model with error handling
      model_fit <- tryCatch({
        bayesglm(
          formula = formula_binary,
          data = model_data,
          family = binomial(link = "logit"),
          prior.df = 1,                      # Student t df for coefficients
          prior.scale = 2.5,                 # Scale for coefficients
          prior.df.for.intercept = 1,        # Student t df for intercept
          prior.scale.for.intercept = 10,    # Scale for intercept
          maxit = 100                         # Max iterations
        )
      }, error = function(e) {
        cat("ERROR in model fitting:\n")
        cat(e$message, "\n")
        return(NULL)
      })

      # Check if model fitting was successful
      if (is.null(model_fit)) {
        cat("Model fitting failed for", current_model, "-", current_outcome, "\n")
        cat("Skipping to next model-outcome combination.\n")
        sink()  # Close sink before skipping
        next
      }

      cat("Model fitting completed.\n")

      # 5. CONVERGENCE DIAGNOSTICS 

      cat("\n==\n")
      cat("CONVERGENCE DIAGNOSTICS\n")
      cat("==\n")

      # Check convergence (bayesglm returns standard glm object)
      # GLM objects have: $converged, $boundary, $iter
      is_boundary <- if (!is.null(model_fit$boundary)) model_fit$boundary else FALSE

      convergence_status <- list(
        converged = model_fit$converged && !is_boundary,
        convergence_code = ifelse(model_fit$converged && !is_boundary, 0, 1),
        iterations = model_fit$iter,
        boundary = is_boundary,
        messages = NULL,
        warnings = NULL
      )

      cat("\nConverged:", convergence_status$converged, "\n")
      cat("Convergence code:", convergence_status$convergence_code,
          "(0 = converged)\n")
      cat("Iterations used:", convergence_status$iterations, "\n")

      # Check for boundary fits or other warnings
      if (!model_fit$converged) {
        cat("\nWARNING: Model did not converge after", model_fit$iter, "iterations\n")
      }

      if (is_boundary) {
        cat("\nWARNING: Model fitted on the boundary (possible separation or collinearity)\n")
      }

      # Model summary
      cat("\n--- Model Summary ---\n")
      print(summary(model_fit))

      # 6. OVERDISPERSION DIAGNOSTICS 

      cat("\n==\n")
      cat("OVERDISPERSION DIAGNOSTICS\n")
      cat("==\n")

      overdisp_test <- check_overdispersion(model_fit)
      cat("\n")
      print(overdisp_test)

      # 7. DHARMa RESIDUAL DIAGNOSTICS

      cat("\n==\n")
      cat("DHARMa RESIDUAL DIAGNOSTICS\n")
      cat("==\n")

      # Note: DHARMa checks assess the plug-in predictive distribution conditional
      # on the MAP estimate, not a full posterior predictive distribution
      cat("\nGenerating simulated datasets for DHARMa\n")
      cat("Using event level:", event_level, "for outcome:", current_outcome, "\n")
      nsim <- 1000

      # Convert observed response to numeric 0/1
      obs_numeric <- as.integer(model_data[[current_outcome]] == event_level)

      # Generate simulations
      sims_raw <- simulate(model_fit, nsim = nsim)

      # Convert to matrix - simulate.glm can return factors or numeric
      sims_df <- as.data.frame(sims_raw)
      if (is.factor(sims_df[[1]]) || is.character(sims_df[[1]])) {
        # Convert factors to 0/1 based on event_level
        sims_numeric <- as.matrix(sapply(sims_df, function(x) as.integer(x == event_level)))
      } else {
        # Already numeric - just convert to matrix
        sims_numeric <- as.matrix(sims_df)
      }

      # Validate that final matrix is 0/1
      if (!all(sims_numeric %in% c(0, 1))) {
        stop("Simulations are not 0/1 binary values")
      }

      # DHARMa creation
      cat("\nCreating DHARMa object from numeric simulations\n")
      sim_resid <- createDHARMa(
        simulatedResponse = sims_numeric,
        observedResponse  = obs_numeric,
        fittedPredictedResponse = predict(model_fit, type = "response"),
        integerResponse = TRUE
      )

      # Diagnostic tests
      uniformity_test <- testUniformity(sim_resid, plot = FALSE)
      dispersion_test <- testDispersion(sim_resid, plot = FALSE)
      outlier_test <- testOutliers(sim_resid, plot = FALSE)

      cat("\n--- Uniformity Test ---\n");  print(uniformity_test)
      cat("\n--- Dispersion Test ---\n");  print(dispersion_test)
      cat("\n--- Outlier Test ---\n");     print(outlier_test)

      # Store all DHARMa tests
      dharma_tests <- list(
        simulated_residuals = sim_resid,
        uniformity = uniformity_test,
        dispersion = dispersion_test,
        outliers = outlier_test
      )

      # 8. VARIANCE INFLATION FACTORS (VIF) 

      cat("\n==\n")
      cat("VARIANCE INFLATION FACTORS (VIF)\n")
      cat("==\n")

      # Calculate VIF
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
        cat("\nInterpretation:\n")
        cat("  - For numeric predictors: VIF > 10 suggests multicollinearity\n")
        cat("  - For factors: Examine GVIF^(1/(2*Df)) (rightmost column)\n")
        cat("    Values > 2-3 suggest multicollinearity\n")
      }

      # 9. PLUG-IN PREDICTIVE CHECK

      cat("\n==\n")
      cat("PLUG-IN PREDICTIVE CHECK\n")
      cat("==\n")

      cat("\nPerforming plug-in predictive check (event rate)\n")

      # Compare observed vs simulated event rates
      obs_rate <- mean(obs_numeric)
      sim_rates <- colMeans(sims_numeric)

      ppc_df <- data.frame(sim_rate = sim_rates)
      ppc_plot <- ggplot(ppc_df, aes(x = sim_rate)) +
        geom_histogram(bins = 40) +
        geom_vline(xintercept = obs_rate, linetype = "dashed") +
        labs(
          title = "Plug-in Predictive Check: Event Rate",
          x = "Simulated event rate",
          y = "Count",
          subtitle = paste0("Observed event rate = ", round(obs_rate, 3))
        ) +
        theme_minimal()

      ppc_plot_file <- file.path(output_subdir, "predictive_check.png")
      ggsave(ppc_plot_file, plot = ppc_plot, width = 10, height = 6, dpi = 300)
      cat("Saved:", ppc_plot_file, "\n")

      # 10. DIAGNOSTIC PLOTS 

      cat("\n==\n")
      cat("GENERATING DIAGNOSTIC PLOTS\n")
      cat("==\n")

      # DHARMa diagnostic plots
      dharma_plot_file <- file.path(output_subdir, "dharma_diagnostics.png")
      png(dharma_plot_file, width = 14, height = 8, units = "in", res = 300)

      op <- par(no.readonly = TRUE)
      par(oma = c(0, 0, 4, 0))
      plot(sim_resid)
      mtext(paste0(current_model, " - ", current_outcome, " (bayesglm)"),
            outer = TRUE, line = 1, cex = 1.2)
      par(op)

      dev.off()
      cat("Saved:", dharma_plot_file, "\n")

      # 11. CREATE DIAGNOSTIC SUMMARY TABLE

      cat("\n==\n")
      cat("CREATING DIAGNOSTIC SUMMARY TABLE\n")
      cat("==\n")

      diagnostic_summary <- data.frame(
        Model_Type = "bayesglm (MAP)",
        Model = current_model,
        Outcome = current_outcome,
        Event_Level = event_level,
        N = nrow(model_data),
        Converged = convergence_status$converged,
        Convergence_code = convergence_status$convergence_code,
        Iterations = convergence_status$iterations,
        Boundary_fit = convergence_status$boundary,
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

      # Store for combined summary
      combination_name <- paste0(current_model, "_", current_outcome)
      all_diagnostics_list[[combination_name]] <- diagnostic_summary

      # 12. SAVE MODEL AND RESULTS 

      cat("\n==\n")
      cat("SAVING MODEL AND RESULTS\n")
      cat("==\n")

      # Save model object
      model_filename <- paste0("models/", current_model, "_", current_outcome, "_binary_bayesian_glm.rds")
      saveRDS(model_fit, file = model_filename)
      cat("Saved model:", model_filename, "\n")

      # Save comprehensive results
      results_list <- list(
        model = model_fit,
        model_info = list(
          model_type = "bayesglm (MAP)",
          model_name = current_model,
          outcome = current_outcome,
          event_level = event_level,
          formula = formula_binary,
          sample_size = nrow(model_data),
          prior_info = list(
            prior_df = 1,  # Cauchy (Student's t, df=1)
            prior_scale = 2.5,
            prior_df_intercept = 1,
            prior_scale_intercept = 10,
            reference = "Gelman et al. 2008"
          )
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

      cat("\n==\n")
      cat("ITERATION COMPLETE:", current_model, "-", current_outcome, "\n")
      cat("==\n")

      sink()

    }, error = function(e) {
      # Error handler
      cat("\n!!! FATAL ERROR:", current_model, "-", current_outcome, "!!!\n")
      cat("Error:", e$message, "\n")
      cat("Continuing to next combination\n\n")

      if (sink.number() > 0) sink()
    })

  } # End outcome loop
} # End model loop

# FINAL SUMMARY

cat("\n\n==\n")
cat("SCRIPT 3 COMPLETE - ALL MODELS AND OUTCOMES PROCESSED\n")
cat("==\n")

if (length(all_diagnostics_list) > 0) {
  combined_diagnostics <- bind_rows(all_diagnostics_list)

  combined_diag_file <- "output/diagnostics/all_models_diagnostics_summary_bayesian.csv"
  write.csv(combined_diagnostics, file = combined_diag_file, row.names = FALSE)
  cat("\nCombined diagnostics saved to:", combined_diag_file, "\n")

  cat("\n--- Summary of All Model-Outcome Combinations ---\n")
  summary_cols <- combined_diagnostics %>%
    dplyr::select(Model_Type, Model, Outcome, Event_Level, N, Converged,
                  Convergence_code, Iterations, Boundary_fit,
                  Overdispersion_ratio, DHARMa_uniformity_p)
  print(summary_cols)

  n_total <- length(models) * length(outcomes)
  n_processed <- nrow(combined_diagnostics)
  n_converged <- sum(combined_diagnostics$Converged)

  cat("\n--- Processing Summary ---\n")
  cat("Total combinations:", n_total, "\n")
  cat("Successfully processed:", n_processed, "\n")
  cat("Successfully converged:", n_converged, "\n")

  if (n_processed < n_total) {
    cat("\nWARNING:", n_total - n_processed, "combination(s) failed\n")
  }
  if (n_converged < n_processed) {
    cat("WARNING:", n_processed - n_converged, "model(s) did not converge\n")
  }

} else {
  cat("\nWARNING: No models were successfully processed!\n")
}