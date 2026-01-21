# Script 8: Model Comparisons
# Purpose: Compare different LLM models on hip fracture treatment recommendations

# Load required libraries
library(tidyverse)
library(lme4)
library(brms)
library(emmeans)
library(irr)        # for kappa statistics
library(metafor)    # for meta-analytic tests
library(patchwork)  # for combining plots

# Label formatting functions
format_label <- function(x) {
  label <- str_replace_all(x, "_", " ")
  label <- case_when(
    tolower(label) == "copd" ~ "COPD",
    tolower(label) == "ivdu" ~ "Drug use",
    tolower(label) == "native american" ~ "American Indian",
    tolower(label) == "middle eastern" ~ "Middle Eastern",
    tolower(label) == "pacific islander" ~ "Pacific Islander",
    tolower(label) == "anesthetic plan" ~ "Anesthesia type",
    TRUE ~ label
  )
  label <- if_else(
    label %in% c("COPD", "Drug use", "American Indian", "Middle Eastern", "Pacific Islander", "Anesthesia type"),
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

# Set up paths
output_dir <- "./output/08_model_comparisons"
dir.create(file.path(output_dir, "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(output_dir, "data"), showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# DATA LOADING
# ==============================================================================

cat("Loading data\n")

prepared_data <- readRDS("./data/prepared_data.rds")

prepared_data <- prepared_data %>%
  filter(variable != "preference_ga") %>%
  droplevels()

model_prefs <- read_csv("./R/model_preferences.csv", show_col_types = FALSE)

cat("Data loaded successfully.\n")
cat("Total observations:", nrow(prepared_data), "\n")
cat("Models in preferences:", length(unique(model_prefs$model)), "\n")

# ==============================================================================
# PREPARE OUTCOME INFO
# ==============================================================================

# Prepare data for each outcome
outcomes_info <- tribble(
  ~outcome_var, ~outcome_name,
  "nerve_block", "Nerve block",
  "anesthetic_plan", "Anesthesia type",
  "arterial_line", "Arterial line"
)

# ==============================================================================
# ANALYSIS 4: MODEL HETEROGENEITY ACROSS REPLICATES
# ==============================================================================

cat("\n=== Analysis 4: Replicate Heterogeneity ===\n")

# Calculate variability across replicates for each model/surgery/sex/variable
replicate_variability <- list()

for (i in 1:nrow(outcomes_info)) {
  outcome_var <- paste0(outcomes_info$outcome_var[i], "_binary")
  outcome_name <- outcomes_info$outcome_name[i]

  cat("Calculating variability for", outcome_name, "\n")

  var_data <- prepared_data %>%
    group_by(model_name, surgery_id, sex, variable) %>%
    summarise(
      n_replicates = n(),
      # For binary outcomes, calculate percent agreement
      # (percentage of replicates agreeing with the most common outcome)
      n_positive = sum(as.numeric(!!sym(outcome_var) == levels(!!sym(outcome_var))[2])),
      n_negative = n() - sum(as.numeric(!!sym(outcome_var) == levels(!!sym(outcome_var))[2])),
      n_mode = max(n_positive, n_negative),
      pct_agreement = (n_mode / n_replicates) * 100,
      .groups = "drop"
    ) %>%
    mutate(outcome = outcome_name)

  replicate_variability[[outcome_name]] <- var_data
}

replicate_var_df <- bind_rows(replicate_variability)

# Save detailed variability data
write_csv(replicate_var_df,
          file.path(output_dir, "data", "replicate_variability_detailed.csv"))

# Create violin plot of percent agreement distributions
p_variability <- replicate_var_df %>%
  mutate(model_name = format_model_name(model_name)) %>%
  ggplot(aes(x = model_name, y = pct_agreement, fill = model_name)) +
  geom_violin(scale = "count", trim = TRUE) +
  geom_boxplot(width = 0.05, outlier.shape = NA, fill = "white", alpha = 0.7) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray50", alpha = 0.5) +
  facet_wrap(~ outcome) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, 10)) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 11),
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 1), "pt")) +
  labs(x = NULL, y = "Percent agreement",
       title = "A. Model sampling variability")

ggsave(file.path(output_dir, "figures", "replicate_variability_violinplot.png"),
       p_variability, width = 10, height = 6, dpi = 300)

cat("Variability violin plot saved.\n")

# ==============================================================================
# TABLE 1: OUTCOME-VARIABLE COMBINATIONS WITH LOWEST AGREEMENT
# ==============================================================================

cat("\n=== Creating Table 1: Lowest Agreement Combinations ===\n")

# Calculate average percent agreement across models for each variable-outcome combination
# Lower percent agreement indicates higher variability/lower reproducibility
lowest_agreement <- replicate_var_df %>%
  group_by(variable, outcome) %>%
  summarise(
    mean_agreement = mean(pct_agreement, na.rm = TRUE),
    sd_agreement = sd(pct_agreement, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(variable_label = format_label(variable)) %>%
  arrange(mean_agreement) %>%
  select(outcome, variable, variable_label, mean_agreement, sd_agreement)

write_csv(lowest_agreement,
          file.path(output_dir, "tables", "lowest_agreement_combinations.csv"))

cat("Table 1: Lowest agreement combinations saved.\n")

# ==============================================================================
# TABLE 2: CHI-SQUARE TESTS FOR STRONG RECOMMENDATION USAGE
# ==============================================================================

cat("\n=== Creating Table 2: Chi-square Tests ===\n")

# Analyze use of "strongly" vs regular recommendations across models
# For plotting purposes, collect data by model
strength_by_model <- list()
# For Table 2, collect chi-square test results
chi_square_results <- list()

for (i in 1:nrow(outcomes_info)) {
  outcome_var <- outcomes_info$outcome_var[i]
  outcome_name <- outcomes_info$outcome_name[i]

  cat("Analyzing recommendation strength for", outcome_name, "\n")

  # Calculate percent strong by model (for plotting)
  strength_data <- prepared_data %>%
    mutate(
      is_strong = str_detect(as.character(!!sym(outcome_var)), "^strongly_")
    ) %>%
    group_by(model_name) %>%
    summarise(
      n_total = n(),
      n_strong = sum(is_strong),
      pct_strong = n_strong / n_total * 100,
      .groups = "drop"
    ) %>%
    mutate(outcome = outcome_name)

  strength_by_model[[outcome_name]] <- strength_data

  # Chi-square test for differences in strength usage across models
  contingency <- prepared_data %>%
    mutate(
      is_strong = str_detect(as.character(!!sym(outcome_var)), "^strongly_")
    ) %>%
    count(model_name, is_strong) %>%
    pivot_wider(names_from = is_strong, values_from = n) %>%
    select(-model_name) %>%
    as.matrix()

  chi_test <- chisq.test(contingency)

  # Store chi-square results
  chi_square_results[[outcome_name]] <- tibble(
    outcome = outcome_name,
    chi_square_statistic = as.numeric(chi_test$statistic),
    degrees_of_freedom = chi_test$parameter,
    p_value = chi_test$p.value
  )
}

# Combine strength data for plotting
strength_df <- bind_rows(strength_by_model)

# Combine chi-square results for Table 2
chi_square_df <- bind_rows(chi_square_results)

write_csv(chi_square_df,
          file.path(output_dir, "tables", "chi_square_strength_tests.csv"))

cat("Table 2: Chi-square strength tests saved.\n")

# Create barplot
p_strength <- strength_df %>%
  mutate(model_name = format_model_name(model_name)) %>%
  ggplot(aes(x = model_name, y = pct_strong, fill = model_name)) +
  geom_col(width = 1) +
  geom_text(aes(label = sprintf("%.0f%%", round(pct_strong))),
            vjust = -0.3, size = 3.5) +
  facet_wrap(~ outcome) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 1), "pt")) +
  labs(x = NULL, y = "Percent strong recommendations",
       title = "B. Model usage of 'strong' recommendations")

ggsave(file.path(output_dir, "figures", "recommendation_strength_barplot.png"),
       p_strength, width = 10, height = 6, dpi = 300)

cat("Recommendation strength barplot saved.\n")

# Create combined plot with violinplot on top and barplot on bottom
# Add spacing between top and bottom panels
p_combined <- p_variability / plot_spacer() / p_strength +
  plot_layout(heights = c(1, 0.05, 1))

ggsave(file.path(output_dir, "figures", "model_comparison_combined.png"),
       p_combined, width = 10, height = 12, dpi = 300)

cat("Combined model comparison plot saved.\n")