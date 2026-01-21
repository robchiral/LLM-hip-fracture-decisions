# SCRIPT 6: Conditional Outcomes Analysis
# Purpose: Analyze outcomes conditional on primary choices

# Load required packages
library(tidyverse)
library(broom)
library(knitr)
library(kableExtra)
library(ggplot2)
library(patchwork)

# Label formatting functions
format_label <- function(x) {
  label <- str_replace_all(x, "_", " ")
  label <- case_when(
    tolower(label) == "copd" ~ "COPD",
    tolower(label) == "ivdu" ~ "Drug use",
    tolower(label) == "native american" ~ "American Indian",
    tolower(label) == "middle eastern" ~ "Middle Eastern",
    tolower(label) == "pacific islander" ~ "Pacific Islander",
    tolower(label) == "ga airway" ~ "GA airway",
    tolower(label) == "ga technique" ~ "GA technique",
    tolower(label) == "lma" ~ "Laryngeal mask airway",
    tolower(label) == "ett" ~ "Endotracheal tube",
    tolower(label) == "tiva" ~ "Total intravenous",
    tolower(label) == "ficb" ~ "FICB",
    tolower(label) == "peng" ~ "PENG block",
    tolower(label) == "femoral" ~ "Femoral block",
    tolower(label) == "suprainguinal ficb" ~ "Suprainguinal FICB",
    tolower(label) == "infrainguinal ficb" ~ "Infrainguinal FICB",
    tolower(label) == "anesthetic plan" ~ "Anesthesia type",
    TRUE ~ label
  )
  label <- if_else(
    label %in% c("COPD", "Drug use", "American Indian", "Middle Eastern", "Pacific Islander",
                 "GA airway", "GA technique", "Laryngeal mask airway", "Endotracheal tube", "Total intravenous",
                 "FICB", "PENG block", "Femoral block", "Suprainguinal FICB", "Infrainguinal FICB", "Anesthesia type"),
    label,
    str_to_sentence(label)
  )
  return(label)
}

format_model_name <- function(x) {
  model_display <- case_when(
    x == "deepseek" ~ "DeepSeek 3.2",
    x == "gpt-5-2025-08-07" ~ "GPT-5",
    x == "gpt-5-mini-2025-08-07" ~ "GPT-5 mini",
    x == "gpt-5-nano-2025-08-07" ~ "GPT-5 nano",
    x == "gemini-2.5-flash-preview-09-2025" ~ "Gemini 2.5 Flash",
    x == "deepseek (Bayesian)" ~ "DeepSeek 3.2 (Bayesian)",
    x == "gpt-5-2025-08-07 (Bayesian)" ~ "GPT-5 (Bayesian)",
    x == "gpt-5-mini-2025-08-07 (Bayesian)" ~ "GPT-5 mini (Bayesian)",
    x == "gpt-5-nano-2025-08-07 (Bayesian)" ~ "GPT-5 nano (Bayesian)",
    x == "gemini-2.5-flash-preview-09-2025 (Bayesian)" ~ "Gemini 2.5 Flash (Bayesian)",
    TRUE ~ x
  )
  return(model_display)
}

# Create output directories if they don't exist
dir.create("./output/06_conditional/data", showWarnings = FALSE, recursive = TRUE)
dir.create("./output/06_conditional/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("./output/06_conditional/figures", showWarnings = FALSE, recursive = TRUE)

# 1. LOAD DATA

cat("Loading prepared data\n")
data <- readRDS("./data/prepared_data.rds")

data <- data %>%
  filter(variable != "preference_ga") %>%
  droplevels()

cat("Data loaded successfully!\n")
cat("Dimensions:", nrow(data), "rows x", ncol(data), "columns\n\n")

# 2. DEFINE CONDITIONAL OUTCOMES AND FILTERS

cat("Defining conditional outcomes and their filters\n\n")

# Define conditional outcomes with their filtering conditions
conditional_outcomes <- list(
  nerve_block_type = list(
    outcome = "nerve_block_type",
    filter_var = "nerve_block",
    filter_values = c("recommend_placement", "strongly_recommend_placement"),
    description = "Nerve block type",
    title = "D. Nerve block type"
  ),

  ga_airway = list(
    outcome = "ga_airway",
    filter_var = "anesthetic_plan",
    filter_values = c("strongly_recommend_general", "recommend_general"),
    description = "General anesthesia airway",
    title = "A. General anesthesia airway"
  ),

  ga_technique = list(
    outcome = "ga_technique",
    filter_var = "anesthetic_plan",
    filter_values = c("strongly_recommend_general", "recommend_general"),
    description = "General anesthesia technique",
    title = "B. General anesthesia technique"
  ),

  neuraxial_type = list(
    outcome = "neuraxial_type",
    filter_var = "anesthetic_plan",
    filter_values = c("recommend_neuraxial", "strongly_recommend_neuraxial"),
    description = "Neuraxial anesthesia type",
    title = "C. Neuraxial anesthesia type"
  )
)

# 3. OVERALL DISTRIBUTIONS BY MODEL

cat("=" , rep("=", 78), "\n", sep = "")
cat("CALCULATING OVERALL DISTRIBUTIONS\n")
cat("=" , rep("=", 78), "\n\n", sep = "")

# Initialize list to store results
overall_distributions <- list()

for (outcome_name in names(conditional_outcomes)) {
  outcome_info <- conditional_outcomes[[outcome_name]]

  cat("\n--- ", outcome_info$description, " ---\n", sep = "")

  # Filter data based on condition
  filtered_data <- data %>%
    filter(!!sym(outcome_info$filter_var) %in% outcome_info$filter_values) %>%
    filter(!!sym(outcome_info$outcome) != "none")  # Exclude "none" category

  cat("Filtered N =", nrow(filtered_data), "\n")

  # Calculate overall distribution by model
  dist <- filtered_data %>%
    group_by(model_name, !!sym(outcome_info$outcome)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name) %>%
    mutate(
      total = sum(n),
      proportion = n / total,
      percentage = round(proportion * 100, 1)
    ) %>%
    arrange(model_name, !!sym(outcome_info$outcome))

  print(dist)

  # Store results
  overall_distributions[[outcome_name]] <- dist %>%
    mutate(outcome = outcome_name) %>%
    rename(category = !!sym(outcome_info$outcome)) %>%
    select(outcome, model_name, category, n, total, proportion, percentage)
}

# Combine all distributions into one table
overall_distributions_combined <- bind_rows(overall_distributions)

# Save to file
write.csv(overall_distributions_combined,
          "./output/06_conditional/tables/overall_distributions.csv",
          row.names = FALSE)

cat("\nOverall distributions saved to ./output/06_conditional/tables/overall_distributions.csv\n")

# 4. HETEROGENEITY TESTS
# Uses hybrid approach:
#   - Chi-square test when N >= 100 and all expected cell counts >= 5
#   - Fisher's exact test (simulated) when N < 100 or sparse cells present

cat("\n")
cat("=" , rep("=", 78), "\n", sep = "")
cat("TESTING FOR HETEROGENEITY\n")
cat("=" , rep("=", 78), "\n\n", sep = "")

# Initialize list to store test results
heterogeneity_tests <- list()

for (outcome_name in names(conditional_outcomes)) {
  outcome_info <- conditional_outcomes[[outcome_name]]

  cat("\n--- ", outcome_info$description, " ---\n\n", sep = "")

  # Filter data based on condition
  filtered_data <- data %>%
    filter(!!sym(outcome_info$filter_var) %in% outcome_info$filter_values) %>%
    filter(!!sym(outcome_info$outcome) != "none")  # Exclude "none" category

  # Loop through each model
  for (model in unique(filtered_data$model_name)) {
    cat("Model:", model, "\n")

    model_data <- filtered_data %>% filter(model_name == model)

    # Test 1: Heterogeneity by variable
    cat("  Testing heterogeneity by variable ")
    tryCatch({
      # Filter variables for ga_technique based on model
      test_data <- model_data
      if (outcome_name == "ga_technique") {
        if (model %in% c("gpt-5-2025-08-07", "gpt-5-mini-2025-08-07")) {
          test_data <- model_data %>%
            filter(variable %in% c("stent", "aortic_stenosis", "lumbar_fusion", "thrombocytopenia"))
        } else if (model == "gpt-5-nano-2025-08-07") {
          test_data <- model_data %>%
            filter(variable %in% c("stent", "thrombocytopenia"))
        }
      }

      # Create contingency table
      cont_table <- table(test_data[[outcome_info$outcome]],
                          test_data$variable)

      # Skip if insufficient data
      if (nrow(test_data) < 30) {
        cat("Insufficient data (N=", nrow(test_data), ", need ≥30)\n", sep = "")
        heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
          outcome = outcome_name,
          model = model,
          factor = "variable",
          p_value = NA,
          n = nrow(test_data),
          method = "insufficient_data"
        )))
      } else {
        # Hybrid approach: choose test based on sample size and expected cell counts
        # Check expected cell counts
        chi_result <- chisq.test(cont_table)
        min_expected <- min(chi_result$expected)

        # Use chi-square if N >= 100 and all expected counts >= 5
        # Otherwise use Fisher's exact test with simulation
        if (nrow(test_data) >= 100 && min_expected >= 5) {
          cat("p =", format.pval(chi_result$p.value, digits = 3),
              " (Chi-square, N=", nrow(test_data), ")\n", sep = "")

          heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
            outcome = outcome_name,
            model = model,
            factor = "variable",
            p_value = chi_result$p.value,
            n = nrow(test_data),
            method = "chi_square"
          )))
        } else {
          # Use Fisher's exact test with simulation for small samples or sparse cells
          fisher_var <- fisher.test(cont_table, simulate.p.value = TRUE, B = 10000, workspace = 2e8)

          cat("p =", format.pval(fisher_var$p.value, digits = 3),
              " (Fisher's exact, simulated, N=", nrow(test_data),
              ", min expected=", round(min_expected, 1), ")\n", sep = "")

          heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
            outcome = outcome_name,
            model = model,
            factor = "variable",
            p_value = fisher_var$p.value,
            n = nrow(test_data),
            method = "fisher_simulated"
          )))
        }
      }
    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
      heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
        outcome = outcome_name,
        model = model,
        factor = "variable",
        p_value = NA,
        n = nrow(test_data),
        method = "error"
      )))
    })

    # Test 2: Heterogeneity by surgery_id
    cat("  Testing heterogeneity by surgery ")
    tryCatch({
      cont_table <- table(model_data[[outcome_info$outcome]],
                          model_data$surgery_id)

      # Skip if insufficient data
      if (nrow(model_data) < 30) {
        cat("Insufficient data (N=", nrow(model_data), ", need ≥30)\n", sep = "")
        heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
          outcome = outcome_name,
          model = model,
          factor = "surgery_id",
          p_value = NA,
          n = nrow(model_data),
          method = "insufficient_data"
        )))
      } else {
        # Hybrid approach: choose test based on sample size and expected cell counts
        chi_result <- chisq.test(cont_table)
        min_expected <- min(chi_result$expected)

        # Use chi-square if N >= 100 and all expected counts >= 5
        if (nrow(model_data) >= 100 && min_expected >= 5) {
          cat("p =", format.pval(chi_result$p.value, digits = 3),
              " (Chi-square, N=", nrow(model_data), ")\n", sep = "")

          heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
            outcome = outcome_name,
            model = model,
            factor = "surgery_id",
            p_value = chi_result$p.value,
            n = nrow(model_data),
            method = "chi_square"
          )))
        } else {
          # Use Fisher's exact test with simulation
          fisher_surg <- fisher.test(cont_table, simulate.p.value = TRUE, B = 10000, workspace = 2e8)

          cat("p =", format.pval(fisher_surg$p.value, digits = 3),
              " (Fisher's exact, simulated, N=", nrow(model_data),
              ", min expected=", round(min_expected, 1), ")\n", sep = "")

          heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
            outcome = outcome_name,
            model = model,
            factor = "surgery_id",
            p_value = fisher_surg$p.value,
            n = nrow(model_data),
            method = "fisher_simulated"
          )))
        }
      }
    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
      heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
        outcome = outcome_name,
        model = model,
        factor = "surgery_id",
        p_value = NA,
        n = nrow(model_data),
        method = "error"
      )))
    })

    # Test 3: Heterogeneity by sex
    cat("  Testing heterogeneity by sex ")
    tryCatch({
      cont_table <- table(model_data[[outcome_info$outcome]],
                          model_data$sex)

      # Skip if insufficient data
      if (nrow(model_data) < 30) {
        cat("Insufficient data (N=", nrow(model_data), ", need ≥30)\n", sep = "")
        heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
          outcome = outcome_name,
          model = model,
          factor = "sex",
          p_value = NA,
          n = nrow(model_data),
          method = "insufficient_data"
        )))
      } else {
        # Hybrid approach: choose test based on sample size and expected cell counts
        chi_result <- chisq.test(cont_table)
        min_expected <- min(chi_result$expected)

        # Use chi-square if N >= 100 and all expected counts >= 5
        if (nrow(model_data) >= 100 && min_expected >= 5) {
          cat("p =", format.pval(chi_result$p.value, digits = 3),
              " (Chi-square, N=", nrow(model_data), ")\n", sep = "")

          heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
            outcome = outcome_name,
            model = model,
            factor = "sex",
            p_value = chi_result$p.value,
            n = nrow(model_data),
            method = "chi_square"
          )))
        } else {
          # Use Fisher's exact test with simulation
          fisher_sex <- fisher.test(cont_table, simulate.p.value = TRUE, B = 10000, workspace = 2e8)

          cat("p =", format.pval(fisher_sex$p.value, digits = 3),
              " (Fisher's exact, simulated, N=", nrow(model_data),
              ", min expected=", round(min_expected, 1), ")\n", sep = "")

          heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
            outcome = outcome_name,
            model = model,
            factor = "sex",
            p_value = fisher_sex$p.value,
            n = nrow(model_data),
            method = "fisher_simulated"
          )))
        }
      }
    }, error = function(e) {
      cat("ERROR:", e$message, "\n")
      heterogeneity_tests <- append(heterogeneity_tests, list(data.frame(
        outcome = outcome_name,
        model = model,
        factor = "sex",
        p_value = NA,
        n = nrow(model_data),
        method = "error"
      )))
    })

    cat("\n")
  }
}

# Combine all test results
heterogeneity_tests_combined <- bind_rows(heterogeneity_tests)

# Add significance flags
heterogeneity_tests_combined <- heterogeneity_tests_combined %>%
  mutate(
    significant = ifelse(p_value < 0.05, "Yes", "No"),
    sig_level = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Save to file
write.csv(heterogeneity_tests_combined,
          "./output/06_conditional/tables/heterogeneity_tests.csv",
          row.names = FALSE)

cat("\nHeterogeneity test results saved to ./output/06_conditional/tables/heterogeneity_tests.csv\n")

# Print summary of significant results
cat("\n")
cat("=" , rep("=", 78), "\n", sep = "")
cat("SUMMARY OF SIGNIFICANT HETEROGENEITY (p < 0.05)\n")
cat("=" , rep("=", 78), "\n\n", sep = "")

sig_results <- heterogeneity_tests_combined %>%
  filter(p_value < 0.05) %>%
  arrange(p_value)

if (nrow(sig_results) > 0) {
  print(sig_results %>%
          select(outcome, model, factor, p_value, sig_level, method, n))
} else {
  cat("No significant heterogeneity detected (as expected).\n")
}

# 5. VISUALIZATIONS

cat("\n")
cat("=" , rep("=", 78), "\n", sep = "")
cat("CREATING VISUALIZATIONS\n")
cat("=" , rep("=", 78), "\n\n", sep = "")

# Function to create bar plot for overall distribution
create_distribution_plot <- function(outcome_name, outcome_info, data) {
  # Filter data
  filtered_data <- data %>%
    filter(!!sym(outcome_info$filter_var) %in% outcome_info$filter_values) %>%
    filter(!!sym(outcome_info$outcome) != "none")

  # Calculate percentages and format labels
  # First, get all possible outcome levels (excluding "none")
  all_outcome_levels <- levels(filtered_data[[outcome_info$outcome]])
  all_outcome_levels <- all_outcome_levels[all_outcome_levels != "none"]

  # Get all unique models
  all_models <- unique(filtered_data$model_name)

  # Create complete grid and calculate percentages
  plot_data <- filtered_data %>%
    group_by(model_name, !!sym(outcome_info$outcome)) %>%
    summarise(n = n(), .groups = "drop") %>%
    # Complete the grid to include all combinations
    complete(model_name = all_models,
             !!sym(outcome_info$outcome) := all_outcome_levels,
             fill = list(n = 0)) %>%
    group_by(model_name) %>%
    mutate(
      percentage = n / sum(n) * 100,
      # Format model names for legend
      model_display = format_model_name(as.character(model_name)),
      # Format outcome values for x-axis, preserving factor order
      outcome_display = factor(
        format_label(as.character(!!sym(outcome_info$outcome))),
        levels = format_label(all_outcome_levels)
      )
    )

  # Create plot
  p <- ggplot(plot_data,
              aes(x = outcome_display, y = percentage, fill = model_display)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = sprintf("%.0f%%", round(percentage))),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 3.5) +
    labs(
      title = outcome_info$title,  # Use title from outcome_info
      x = NULL,
      y = "Percent of responses",
      fill = "Model"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(hjust = 0.5),
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "pt")
    )

  return(p)
}

# Create plots for each outcome
plots <- list()

for (outcome_name in names(conditional_outcomes)) {
  outcome_info <- conditional_outcomes[[outcome_name]]
  cat("Creating plot for", outcome_name, "\n")

  plots[[outcome_name]] <- create_distribution_plot(outcome_name, outcome_info, data)
}

# Reorder plots: GA airway, GA technique, Neuraxial type, Nerve block type
# This creates a counterclockwise rotation in the 2x2 grid
# Arrange as: top row (A, B), spacer, bottom row (C, D)
combined_plot <- (plots[["ga_airway"]] + plots[["ga_technique"]]) /
                 plot_spacer() /
                 (plots[["neuraxial_type"]] + plots[["nerve_block_type"]]) +
  plot_layout(heights = c(1, 0.05, 1), guides = "collect") &
  theme(legend.position = "bottom")

# Save combined plot
ggsave("./output/06_conditional/figures/conditional_distributions.png",
       combined_plot,
       width = 14, height = 10, dpi = 300)

cat("\nPlot saved to ./output/06_conditional/figures/conditional_distributions.png\n")

# 6. DETAILED CROSSTABS (for reference)

cat("\n")
cat("=" , rep("=", 78), "\n", sep = "")
cat("CREATING DETAILED CROSSTABS\n")
cat("=" , rep("=", 78), "\n\n", sep = "")

for (outcome_name in names(conditional_outcomes)) {
  outcome_info <- conditional_outcomes[[outcome_name]]

  cat("Creating crosstabs for", outcome_name, "\n")

  # Filter data
  filtered_data <- data %>%
    filter(!!sym(outcome_info$filter_var) %in% outcome_info$filter_values) %>%
    filter(!!sym(outcome_info$outcome) != "none")

  # Create detailed crosstabs by variable
  crosstab_var <- filtered_data %>%
    group_by(model_name, variable, !!sym(outcome_info$outcome)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name, variable) %>%
    mutate(
      total = sum(n),
      percentage = round(n / total * 100, 1)
    )

  # Save to file
  filename <- paste0("./output/06_conditional/tables/", outcome_name, "_by_variable.csv")
  write.csv(crosstab_var, filename, row.names = FALSE)

  # Create crosstabs by surgery
  crosstab_surg <- filtered_data %>%
    group_by(model_name, surgery_id, !!sym(outcome_info$outcome)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name, surgery_id) %>%
    mutate(
      total = sum(n),
      percentage = round(n / total * 100, 1)
    )

  # Save to file
  filename <- paste0("./output/06_conditional/tables/", outcome_name, "_by_surgery.csv")
  write.csv(crosstab_surg, filename, row.names = FALSE)

  # Create crosstabs by sex
  crosstab_sex <- filtered_data %>%
    group_by(model_name, sex, !!sym(outcome_info$outcome)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name, sex) %>%
    mutate(
      total = sum(n),
      percentage = round(n / total * 100, 1)
    )

  # Save to file
  filename <- paste0("./output/06_conditional/tables/", outcome_name, "_by_sex.csv")
  write.csv(crosstab_sex, filename, row.names = FALSE)
}

cat("\nDetailed crosstabs saved to ./output/06_conditional/tables/\n")

# 7. SESSION INFO

cat("\n")
cat("=" , rep("=", 78), "\n", sep = "")
cat("ANALYSIS COMPLETE\n")
cat("=" , rep("=", 78), "\n\n", sep = "")

cat("Summary:\n")
cat("- Analyzed", length(conditional_outcomes), "conditional outcomes\n")
cat("- Conducted", nrow(heterogeneity_tests_combined), "heterogeneity tests (hybrid chi-square/Fisher's exact)\n")
cat("- Created 1 combined visualization\n")
cat("- Saved all results to ./output/06_conditional/\n\n")

cat("Output files:\n")
cat("  Tables:\n")
cat("    - overall_distributions.csv\n")
cat("    - heterogeneity_tests.csv\n")
cat("    - [outcome]_by_variable.csv (x4)\n")
cat("    - [outcome]_by_surgery.csv (x4)\n")
cat("    - [outcome]_by_sex.csv (x4)\n")
cat("  Figures:\n")
cat("    - conditional_distributions.png\n\n")

cat("=== Session Information ===\n")
sessionInfo()
