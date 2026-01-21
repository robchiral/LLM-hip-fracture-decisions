# SCRIPT 4: Combined Variable, Surgery, and Sex Effects Analysis
# Purpose: Combine analyses from Scripts 04a and 05a into unified forest plots
#          showing all three effect types on the same plot

# 1. SETUP 

# Load required packages
library(lme4)          # Mixed effects models
library(emmeans)       # Estimated marginal means and contrasts
library(tidyverse)     # Data manipulation and plotting
library(data.table)    # Efficient data handling
library(kableExtra)    # Table formatting
library(patchwork)     # Combining plots

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
    x == "deepseek (Bayesian)" ~ "DeepSeek 3.2",
    x == "gpt-5-2025-08-07 (Bayesian)" ~ "GPT-5",
    x == "gpt-5-mini-2025-08-07 (Bayesian)" ~ "GPT-5 mini",
    x == "gpt-5-nano-2025-08-07 (Bayesian)" ~ "GPT-5 nano",
    x == "gemini-2.5-flash-preview-09-2025 (Bayesian)" ~ "Gemini 2.5 Flash",
    TRUE ~ x
  )
  return(model_display)
}

# Create output directories if they don't exist
dir.create("./output/04_combined/data", showWarnings = FALSE, recursive = TRUE)
dir.create("./output/04_combined/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("./output/04_combined/figures", showWarnings = FALSE, recursive = TRUE)

cat("\n=\n")
cat("SCRIPT 4: Combined Variable, Surgery, and Sex Effects Analysis\n")
cat("=\n\n")

# 2. LOAD DATA AND MODELS 

cat("Loading data and models\n")

data <- readRDS("./data/prepared_data.rds")

data <- data %>%
  filter(variable != "preference_ga") %>%
  droplevels()

# Load model preferences from CSV
model_preferences <- read_csv("./R/model_preferences.csv", show_col_types = FALSE)

# Create model_info dataframe with full file paths
model_info <- model_preferences %>%
  mutate(
    file = file.path("./models", filename),
    is_bayesian = (type == "bayesian"),
    model_display = if_else(is_bayesian,
                            paste0(model, " (Bayesian)"),
                            model)
  ) %>%
  rename(model_name = model) %>%
  select(file, model_name, outcome, is_bayesian, model_display)

cat("Loaded", nrow(model_info), "model-outcome combinations from model_preferences.csv\n")
cat("  Frequentist:", sum(!model_info$is_bayesian), "\n")
cat("  Bayesian:", sum(model_info$is_bayesian), "\n")
cat("Models:", unique(model_info$model_name), "\n")
cat("Outcomes:", unique(model_info$outcome), "\n\n")

cat("NOTE: Models are GLMs (not GLMMs) - no random effects included.\n")
cat("      Replicates are independent samples; effects are population-average.\n")
cat("      emmeans() calculates marginal means at the population level.\n\n")

# Define orderings (from Scripts 01 and 05a)
variable_order <- c("thrombocytopenia", "stent", "obesity", "lumbar_fusion", "delirium", "copd", "aortic_stenosis",
                   "unhoused", "undocumented", "ivdu",
                   "white", "pacific_islander", "native_american", "middle_eastern", "hispanic", "black", "asian",
                   "control")

# Surgery order from Script 05a (line 94)
surgery_order <- c("percutaneous_screws", "sliding_hip_screw", "short_cephalomedullary",
                   "long_cephalomedullary", "hemiarthroplasty", "total_hip")

# 3. HELPER FUNCTIONS

# FUNCTION: Extract variable contrasts with risk differences (from Script 04a)
extract_variable_contrasts_with_rd <- function(model_obj, outcome_name, model_name) {

  cat("  Extracting variable contrasts with risk differences for:", outcome_name, "\n")

  # Get marginal means on logit scale
  emm_logit <- emmeans(model_obj, ~ variable)

  # Get probabilities for reporting
  emm_prob <- emmeans(model_obj, ~ variable, type = "response")
  prob_summary <- as.data.frame(summary(emm_prob))

  # Get risk differences
  emm_prob_regrid <- regrid(emm_prob)
  risk_diff <- contrast(emm_prob_regrid, method = "trt.vs.ctrl", ref = "control", adjust = "none")
  risk_diff_summary <- as.data.frame(summary(risk_diff, infer = TRUE))

  # Extract variable names from contrast labels
  risk_diff_summary <- risk_diff_summary %>%
    mutate(
      variable = str_remove(contrast, " - control"),
      variable = str_trim(variable)
    )

  # Get control group baseline probability
  control_prob <- prob_summary %>%
    filter(variable == "control") %>%
    pull(prob)

  # Combine all metrics
  results <- risk_diff_summary %>%
    rename(
      rd_estimate = estimate,
      rd_SE = SE,
      rd_LCL = asymp.LCL,
      rd_UCL = asymp.UCL,
      rd_z = z.ratio,
      rd_p = p.value
    ) %>%
    select(variable, rd_estimate, rd_SE, rd_LCL, rd_UCL, rd_z, rd_p) %>%
    # Add predicted probabilities
    left_join(
      prob_summary %>%
        select(variable, prob_variable = prob, prob_variable_SE = SE) %>%
        mutate(control_prob = control_prob),
      by = "variable"
    ) %>%
    # Add metadata
    mutate(
      outcome = outcome_name,
      model = model_name,
      effect_type = "Variable",
      comparison_label = variable,
      variable_type = case_when(
        variable == "control" ~ "control",
        variable %in% c("native_american", "asian", "black", "hispanic",
                       "middle_eastern", "pacific_islander", "white",
                       "unhoused", "undocumented", "ivdu") ~ "sociodemographic",
        variable %in% c("delirium", "thrombocytopenia", "lumbar_fusion",
                       "aortic_stenosis", "copd", "obesity", "stent") ~ "medical",
        TRUE ~ "other"
      )
    )

  return(results)
}

# FUNCTION: Extract surgery contrasts with risk differences (from Script 05a)
extract_surgery_contrasts_with_rd <- function(model_obj, outcome_name, model_name) {

  cat("  Extracting surgery contrasts with risk differences for:", outcome_name, "\n")

  # Get marginal means on logit scale
  emm_logit <- emmeans(model_obj, ~ surgery_id)

  # Get probabilities for reporting
  emm_prob <- emmeans(model_obj, ~ surgery_id, type = "response")
  prob_summary <- as.data.frame(summary(emm_prob))

  # Get risk differences
  emm_prob_regrid <- regrid(emm_prob)
  risk_diff <- contrast(emm_prob_regrid, method = "trt.vs.ctrl", ref = "short_cephalomedullary", adjust = "none")
  risk_diff_summary <- as.data.frame(summary(risk_diff, infer = TRUE))

  # Extract surgery name from contrast
  risk_diff_summary <- risk_diff_summary %>%
    mutate(
      surgery = str_remove(contrast, " - short_cephalomedullary"),
      surgery = str_trim(surgery)
    )

  # Get reference probability
  ref_prob <- prob_summary %>%
    filter(surgery_id == "short_cephalomedullary") %>%
    pull(prob)

  # Combine all results
  results <- risk_diff_summary %>%
    rename(
      rd_estimate = estimate,
      rd_SE = SE,
      rd_LCL = asymp.LCL,
      rd_UCL = asymp.UCL,
      rd_z = z.ratio,
      rd_p = p.value
    ) %>%
    select(surgery, rd_estimate, rd_SE, rd_LCL, rd_UCL, rd_z, rd_p) %>%
    # Add predicted probabilities
    left_join(
      prob_summary %>%
        select(surgery = surgery_id, prob_surgery = prob, prob_surgery_SE = SE) %>%
        mutate(ref_prob = ref_prob),
      by = "surgery"
    ) %>%
    # Add metadata
    mutate(
      outcome = outcome_name,
      model = model_name,
      effect_type = "Surgery",
      comparison_label = surgery
    )

  return(results)
}

# FUNCTION: Extract sex contrasts with risk differences (from Script 05a)
extract_sex_contrasts_with_rd <- function(model_obj, outcome_name, model_name) {

  cat("  Extracting sex contrasts with risk differences for:", outcome_name, "\n")

  # Get marginal means on logit scale
  emm_logit <- emmeans(model_obj, ~ sex)

  # Get probabilities for reporting
  emm_prob <- emmeans(model_obj, ~ sex, type = "response")
  prob_summary <- as.data.frame(summary(emm_prob))

  # Get risk differences
  emm_prob_regrid <- regrid(emm_prob)
  risk_diff <- contrast(emm_prob_regrid, method = "pairwise", adjust = "none")
  risk_diff_summary <- as.data.frame(summary(risk_diff, infer = TRUE))

  # Get reference (female) probability
  ref_prob <- prob_summary %>%
    filter(sex == "female") %>%
    pull(prob)

  # Combine all results
  results <- risk_diff_summary %>%
    rename(
      rd_estimate = estimate,
      rd_SE = SE,
      rd_LCL = asymp.LCL,
      rd_UCL = asymp.UCL,
      rd_z = z.ratio,
      rd_p = p.value
    ) %>%
    select(contrast, rd_estimate, rd_SE, rd_LCL, rd_UCL, rd_z, rd_p) %>%
    # Add predicted probabilities
    cbind(
      prob_male = prob_summary %>% filter(sex == "male") %>% pull(prob),
      prob_male_SE = prob_summary %>% filter(sex == "male") %>% pull(SE),
      prob_female = ref_prob,
      prob_female_SE = prob_summary %>% filter(sex == "female") %>% pull(SE)
    ) %>%
    # Add metadata
    mutate(
      outcome = outcome_name,
      model = model_name,
      effect_type = "Sex",
      comparison_label = "male"  # Male vs Female comparison
    )

  return(results)
}

# 4. MAIN ANALYSIS

cat("\n=\n")
cat("EXTRACTING ALL CONTRASTS (VARIABLE, SURGERY, SEX)\n")
cat("=\n\n")

# Initialize lists to store results
variable_results_list <- list()
surgery_results_list <- list()
sex_results_list <- list()

# Loop through all model-outcome combinations
for(i in 1:nrow(model_info)) {

  current_file <- model_info$file[i]
  current_model <- model_info$model_display[i]
  current_outcome <- model_info$outcome[i]

  cat("\n--- Processing:", current_model, "-", current_outcome, "---\n")

  # Load model
  tryCatch({
    model_obj <- readRDS(current_file)

    # Extract variable contrasts
    variable_contrasts <- extract_variable_contrasts_with_rd(
      model_obj,
      outcome_name = current_outcome,
      model_name = current_model
    )
    variable_results_list[[paste0(current_model, "_", current_outcome)]] <- variable_contrasts

    # Extract surgery contrasts
    surgery_contrasts <- extract_surgery_contrasts_with_rd(
      model_obj,
      outcome_name = current_outcome,
      model_name = current_model
    )
    surgery_results_list[[paste0(current_model, "_", current_outcome)]] <- surgery_contrasts

    # Extract sex contrasts
    sex_contrasts <- extract_sex_contrasts_with_rd(
      model_obj,
      outcome_name = current_outcome,
      model_name = current_model
    )
    sex_results_list[[paste0(current_model, "_", current_outcome)]] <- sex_contrasts

    cat("  Successfully extracted all contrasts\n")

  }, error = function(e) {
    cat("  ERROR:", e$message, "\n")
  })
}

# Combine results
cat("\nCombining results\n")
variable_results_all <- bind_rows(variable_results_list)
surgery_results_all <- bind_rows(surgery_results_list)
sex_results_all <- bind_rows(sex_results_list)

# Apply FDR corrections
cat("Applying FDR corrections\n")

# Variable effects: Family-wise FDR by variable_type (from Script 04a)
cat("  Variables: Family-wise correction by variable type\n")
variable_results_all <- variable_results_all %>%
  filter(variable != "control") %>%  # Exclude control from contrasts
  group_by(variable_type) %>%
  mutate(
    rd_p_fdr = p.adjust(rd_p, method = "fdr"),
    rd_significant_fdr = rd_p_fdr < 0.05
  ) %>%
  ungroup()

# Surgery effects: Within-model FDR (from Script 05a)
cat("  Surgery: Within-model correction\n")
surgery_results_all <- surgery_results_all %>%
  group_by(outcome, model) %>%
  mutate(
    rd_p_fdr = p.adjust(rd_p, method = "fdr"),
    rd_significant_fdr = rd_p_fdr < 0.05
  ) %>%
  ungroup()

# Sex effects: No FDR correction (single comparison, from Script 05a)
cat("  Sex: No correction (single comparison)\n")
sex_results_all <- sex_results_all %>%
  mutate(
    rd_p_fdr = rd_p,  # Use raw p-value
    rd_significant_fdr = rd_p < 0.05
  )

# Combine all three effect types into single dataframe
cat("Merging all effect types\n")
combined_results_all <- bind_rows(
  variable_results_all %>% select(effect_type, comparison_label, outcome, model,
                                  rd_estimate, rd_SE, rd_LCL, rd_UCL, rd_z, rd_p,
                                  rd_p_fdr, rd_significant_fdr),
  surgery_results_all %>% select(effect_type, comparison_label, outcome, model,
                                rd_estimate, rd_SE, rd_LCL, rd_UCL, rd_z, rd_p,
                                rd_p_fdr, rd_significant_fdr),
  sex_results_all %>% select(effect_type, comparison_label, outcome, model,
                            rd_estimate, rd_SE, rd_LCL, rd_UCL, rd_z, rd_p,
                            rd_p_fdr, rd_significant_fdr)
)

# Save combined results
cat("Saving combined results\n")
saveRDS(combined_results_all, "./output/04_combined/data/combined_effects_risk_diff_all.rds")
write_csv(combined_results_all, "./output/04_combined/data/combined_effects_risk_diff_all.csv")

cat("Results saved to ./output/04_combined/data/\n")

# 5. CREATE SUMMARY TABLES

cat("\n=\n")
cat("CREATING SUMMARY TABLES\n")
cat("=\n\n")


# Significant AND clinically meaningful effects (>5 percentage points)
cat("Creating table of significant and clinically meaningful effects\n")
significant_and_meaningful <- combined_results_all %>%
  filter(
    rd_significant_fdr == TRUE,  # Statistically significant
    abs(rd_estimate) > 0.05       # Clinically meaningful (>5 percentage points)
  ) %>%
  mutate(
    # Convert to percentage points for easier interpretation
    rd_estimate_pct = rd_estimate * 100,
    rd_LCL_pct = rd_LCL * 100,
    rd_UCL_pct = rd_UCL * 100,
    # Add a flag for whether entire CI exceeds 5pp threshold
    ci_entirely_meaningful = (abs(rd_LCL_pct) > 5 & abs(rd_UCL_pct) > 5),
    # Apply formatting to labels
    comparison_label = format_label(comparison_label),
    outcome = case_when(
      outcome == "anesthetic_plan_binary" ~ "Neuraxial anesthesia",
      outcome == "nerve_block_binary" ~ "Nerve block",
      outcome == "arterial_line_binary" ~ "Arterial line",
      TRUE ~ str_to_sentence(str_replace_all(str_remove(outcome, "_binary"), "_", " "))
    ),
    model = format_model_name(model),
    # Create combined RD column with mean and CI
    RD = sprintf("%.2f (%.2f-%.2f)", rd_estimate_pct, rd_LCL_pct, rd_UCL_pct)
  ) %>%
  arrange(effect_type, outcome, model, desc(abs(rd_estimate_pct))) %>%
  select(
    effect_type,
    comparison_label,
    outcome,
    model,
    RD,
    rd_p,
    rd_p_fdr,
    ci_entirely_meaningful
  )

write_csv(significant_and_meaningful, "./output/04_combined/tables/significant_and_clinically_meaningful.csv")
cat("  Saved: significant_and_clinically_meaningful.csv\n")
cat("  Found", nrow(significant_and_meaningful), "effects that are both significant and clinically meaningful (>5pp)\n")


# 6. CREATE COMPACT THREE-PANEL FIGURE

cat("\n=\n")
cat("CREATING COMPACT THREE-PANEL FIGURE\n")
cat("=\n\n")

# FUNCTION: Create forest plot for three-panel layout
create_panel_forest_plot <- function(combined_df, outcome_name, show_y_labels = TRUE, show_legend = TRUE) {

  # Prepare data
  df <- combined_df %>%
    filter(outcome == outcome_name) %>%
    mutate(
      model_display = format_model_name(model),
      display_label = format_label(comparison_label),
      # Convert to percentage points for display
      RD_pct = rd_estimate * 100,
      RD_LCL_pct = rd_LCL * 100,
      RD_UCL_pct = rd_UCL * 100,
      sig_label = case_when(
        rd_p_fdr < 0.001 ~ "***",
        rd_p_fdr < 0.01  ~ "**",
        rd_p_fdr < 0.05  ~ "*",
        TRUE ~ ""
      )
    )

  if (nrow(df) == 0) {
    warning(sprintf("No data for outcome: %s", outcome_name))
    return(NULL)
  }

  # Define ordering using Script 1 variable order
  variable_display_order <- format_label(setdiff(variable_order, "control"))
  surgery_display_order <- rev(format_label(setdiff(surgery_order, "short_cephalomedullary")))
  sex_display_order <- "Male"

  # Combine all in order: Sex -> Surgery -> Variables (bottom to top in plot)
  all_levels <- c(sex_display_order, surgery_display_order, variable_display_order)

  df <- df %>%
    mutate(
      display_label = factor(display_label, levels = all_levels)
    )

  # Calculate effect type counts
  n_sex <- length(sex_display_order)
  n_surgeries <- length(surgery_display_order)
  n_variables <- length(variable_display_order)

  # Define gap sizes
  gap_size <- 0.5

  # Y positions with actual discontinuities
  var_levels <- levels(df$display_label)
  df$y_base <- match(df$display_label, var_levels)

  # Add gap offsets to create discontinuities
  df <- df %>%
    mutate(
      y_base_adjusted = case_when(
        y_base <= n_sex ~ y_base,
        y_base <= n_sex + n_surgeries ~ y_base + gap_size,
        TRUE ~ y_base + 2 * gap_size
      )
    )

  model_levels <- levels(factor(df$model_display))
  dodge_span <- 0.65
  offs <- if (length(model_levels) == 1) 0 else seq(dodge_span/2, -dodge_span/2, length.out = length(model_levels))
  df$y_pos <- df$y_base_adjusted + offs[match(df$model_display, model_levels)]

  # Calculate adjusted breaks and labels for y-axis
  y_breaks <- df %>%
    group_by(display_label) %>%
    summarise(y_pos = mean(y_base_adjusted), .groups = "drop") %>%
    arrange(y_pos) %>%
    pull(y_pos)

  y_labels <- df %>%
    group_by(display_label) %>%
    summarise(y_pos = mean(y_base_adjusted), .groups = "drop") %>%
    arrange(y_pos) %>%
    pull(display_label) %>%
    as.character()

  # Calculate gap boundaries for separator lines
  max_sex <- max(df$y_pos[df$y_base <= n_sex])
  min_surgery <- min(df$y_pos[df$y_base > n_sex & df$y_base <= n_sex + n_surgeries])
  max_surgery <- max(df$y_pos[df$y_base > n_sex & df$y_base <= n_sex + n_surgeries])
  min_variables <- min(df$y_pos[df$y_base > n_sex + n_surgeries])

  # Calculate separator positions (midpoint of gaps)
  separator1_y <- (max_sex + min_surgery) / 2
  separator2_y <- (max_surgery + min_variables) / 2

  # Create plot
  p <- ggplot(df, aes(y = y_pos, color = model_display, group = model_display)) +
    # Add vertical dotted lines at clinically meaningful thresholds (continuous)
    geom_vline(xintercept = c(-5, 5), linetype = "dotted",
               color = "steelblue", size = 0.6, alpha = 0.7) +

    # Add horizontal separators between effect types
    geom_hline(yintercept = separator1_y, linetype = "solid",
               color = "gray30", size = 0.5, alpha = 0.6) +
    geom_hline(yintercept = separator2_y, linetype = "solid",
               color = "gray30", size = 0.5, alpha = 0.6) +

    # Null hypothesis line
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +

    geom_errorbarh(aes(xmin = RD_LCL_pct, xmax = RD_UCL_pct),
                   height = 0, size = 0.5) +

    geom_point(aes(x = RD_pct, shape = model_display), size = 3) +

    geom_text(aes(x = RD_UCL_pct, label = sig_label),
              hjust = 0, size = 4, show.legend = FALSE, nudge_x = 0.5) +

    scale_color_discrete(name = "Model") +
    scale_shape_discrete(name = "Model") +

    scale_y_continuous(breaks = y_breaks,
                       labels = if(show_y_labels) y_labels else rep("", length(y_labels)),
                       expand = expansion(mult = c(0.01, 0.01))) +

    labs(
      title = case_when(
        grepl("anesthetic_plan", outcome_name, ignore.case = TRUE) ~ "Recommendation for neuraxial anesthesia",
        grepl("nerve_block", outcome_name, ignore.case = TRUE) ~ "Recommendation for peripheral nerve block placement",
        grepl("arterial", outcome_name, ignore.case = TRUE) ~ "Recommendation for arterial line placement",
        TRUE ~ str_to_sentence(str_replace_all(str_remove(outcome_name, "_binary"), "_", " "))
      ),
      x = "Absolute risk difference (percentage points)",
      y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = if(show_legend) "bottom" else "none",
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 13),
      axis.text = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      axis.title = element_text(size = 13),
      panel.grid.major.y = element_line(color = "gray90"),
      panel.grid.major.x = element_line(color = "gray95"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40"),
      plot.margin = margin(t = 1, r = 1, b = 1, l = 1),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    )

  return(p)
}

# Create three-panel combined figure

cat("Creating three-panel combined figure\n")

# Define the three outcomes in desired order
outcomes_ordered <- c("anesthetic_plan_binary", "nerve_block_binary", "arterial_line_binary")

# Create individual panels
cat("  Creating left panel (neuraxial anesthesia)\n")
p_left <- create_panel_forest_plot(
  combined_results_all,
  "anesthetic_plan_binary",
  show_y_labels = TRUE,
  show_legend = FALSE
)

cat("  Creating center panel (nerve block)\n")
p_center <- create_panel_forest_plot(
  combined_results_all,
  "nerve_block_binary",
  show_y_labels = FALSE,
  show_legend = FALSE
)

cat("  Creating right panel (arterial line)\n")
p_right <- create_panel_forest_plot(
  combined_results_all,
  "arterial_line_binary",
  show_y_labels = FALSE,
  show_legend = TRUE
)

# Extract legend from right panel to place at bottom center
cat("  Extracting shared legend\n")
library(cowplot)
legend <- get_legend(p_right)

# Remove legend from right panel
p_right <- p_right + theme(legend.position = "none")

# Combine three panels horizontally
cat("  Combining panels\n")
p_combined <- (p_left | p_center | p_right)

# Add legend at bottom center using cowplot
p_final <- plot_grid(
  p_combined,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.04)
)

# Save the combined figure
output_file <- "./output/04_combined/figures/combined_three_panel_figure.png"
cat("  Saving combined figure\n")

ggsave(
  output_file,
  p_final,
  width = 20,
  height = 18,
  dpi = 300,
  bg = "white"
)

cat("  Saved to:", output_file, "\n")