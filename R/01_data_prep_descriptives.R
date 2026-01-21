# SCRIPT 1: Data Preparation and Descriptive Statistics
# Purpose: Load data, create factors, generate descriptive statistics.

# Load required packages
library(tidyverse)
library(data.table)
library(knitr)
library(kableExtra)
library(ggplot2)
library(patchwork)

# Create output directories if they don't exist
dir.create("./data", showWarnings = FALSE, recursive = TRUE)
dir.create("./output/01_descriptives/tables", showWarnings = FALSE, recursive = TRUE)
dir.create("./output/01_descriptives/figures", showWarnings = FALSE, recursive = TRUE)

# LABEL FORMATTING FUNCTION
format_label <- function(x) {
  label <- str_replace_all(x, "_", " ")
  label <- case_when(
    tolower(label) == "copd" ~ "COPD",
    tolower(label) == "ivdu" ~ "Drug use",
    tolower(label) == "native american" ~ "American Indian",
    tolower(label) == "middle eastern" ~ "Middle Eastern",
    tolower(label) == "pacific islander" ~ "Pacific Islander",
    tolower(label) == "preference ga" ~ "Hospital preference",
    tolower(label) == "ga airway" ~ "GA airway",
    tolower(label) == "ga technique" ~ "GA technique",
    tolower(label) == "lma" ~ "LMA",
    tolower(label) == "ett" ~ "ETT",
    tolower(label) == "tiva" ~ "TIVA",
    tolower(label) == "ficb" ~ "FICB",
    tolower(label) == "peng" ~ "PENG",
    tolower(label) == "suprainguinal ficb" ~ "Suprainguinal FICB",
    tolower(label) == "infrainguinal ficb" ~ "Infrainguinal FICB",
    tolower(label) == "asa classification" ~ "ASA classification",
    tolower(label) == "anesthetic plan" ~ "Anesthesia type",
    tolower(label) == "i" ~ "I",
    tolower(label) == "ii" ~ "II",
    tolower(label) == "iii" ~ "III",
    tolower(label) == "iv" ~ "IV",
    tolower(label) == "v" ~ "V",
    tolower(label) == "vi" ~ "VI",
    TRUE ~ label
  )
  label <- if_else(
    label %in% c("COPD", "Drug use", "American Indian", "Middle Eastern", "Pacific Islander", "Hospital preference",
                 "GA airway", "GA technique", "LMA", "ETT", "TIVA", "FICB", "PENG", "Suprainguinal FICB",
                 "Infrainguinal FICB", "ASA classification", "Anesthesia type", "I", "II", "III", "IV", "V", "VI"),
    label,
    str_to_sentence(label)
  )
  label
}

# MODEL NAME FORMATTING FUNCTION
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
  model_display
}

# 1. LOAD DATA
cat("Loading data from ./data/openai.tsv\n")
data_openai <- read.delim("./data/openai.tsv", sep = "\t", header = TRUE,
                          stringsAsFactors = FALSE)
cat("OpenAI data loaded:", nrow(data_openai), "rows\n")

cat("Loading data from ./data/gemini.tsv\n")
data_gemini <- read.delim("./data/gemini.tsv", sep = "\t", header = TRUE,
                          stringsAsFactors = FALSE)
cat("Gemini data loaded:", nrow(data_gemini), "rows\n")

cat("Loading data from ./data/deepseek.tsv\n")
data_deepseek <- read.delim("./data/deepseek.tsv", sep = "\t", header = TRUE,
                            stringsAsFactors = FALSE)
cat("DeepSeek data loaded:", nrow(data_deepseek), "rows\n")

cat("Combining datasets\n")
data_raw <- rbind(data_openai, data_gemini, data_deepseek)
cat("Data combined successfully!\n")
cat("Total dimensions:", nrow(data_raw), "rows x", ncol(data_raw), "columns\n\n")
str(data_raw); cat("\n")

# 2. DATA CLEANING AND FACTOR CREATION
cat("Creating properly ordered factors\n")
data <- data_raw

# Ordinal outcomes (4-level)
data$nerve_block <- factor(data$nerve_block,
  levels = c("strongly_recommend_against_placement",
             "recommend_against_placement",
             "recommend_placement",
             "strongly_recommend_placement"),
  ordered = TRUE)

data$anesthetic_plan <- factor(data$anesthetic_plan,
  levels = c("strongly_recommend_general",
             "recommend_general",
             "recommend_neuraxial",
             "strongly_recommend_neuraxial"),
  ordered = TRUE)

data$arterial_line <- factor(data$arterial_line,
  levels = c("strongly_recommend_against_placement",
             "recommend_against_placement",
             "recommend_placement",
             "strongly_recommend_placement"),
  ordered = TRUE)

# ASA classification (ordered)
data$asa_classification <- factor(data$asa_classification,
  levels = c("I", "II", "III", "IV", "V", "VI"),
  ordered = TRUE)

# Categorical variables (unordered)
data$nerve_block_type <- factor(data$nerve_block_type,
  levels = c("PENG", "suprainguinal_FICB", "infrainguinal_FICB", "femoral", "none"))
data$ga_airway   <- factor(data$ga_airway,   levels = c("LMA", "ETT", "none"))
data$ga_technique<- factor(data$ga_technique,levels = c("TIVA", "sevoflurane", "none"))
data$neuraxial_type <- factor(data$neuraxial_type,
  levels = c("spinal_only", "epidural_only", "combined_spinal_epidural", "none"))

# Experimental design factors
# Reversed because ggplot2 y-axis displays factors from bottom to top
data$surgery_id <- factor(data$surgery_id,
  levels = c("total_hip", "hemiarthroplasty", "long_cephalomedullary",
             "short_cephalomedullary", "sliding_hip_screw", "percutaneous_screws"))

data$sex <- factor(data$sex, levels = c("male", "female"))

data$variable_type <- factor(data$variable_type,
  levels = c("control", "preference", "sociodemographic", "medical"))

data$variable <- factor(data$variable,
  levels = c("thrombocytopenia", "stent", "obesity", "lumbar_fusion", "delirium", "copd", "aortic_stenosis",
             "unhoused", "undocumented", "ivdu",
             "white", "pacific_islander", "native_american", "middle_eastern", "hispanic", "black", "asian",
             "preference_ga",
             "control"))

data$model_name <- factor(data$model_name,
  levels = c("deepseek", "gemini-2.5-flash-preview-09-2025", "gpt-5-2025-08-07", "gpt-5-mini-2025-08-07", "gpt-5-nano-2025-08-07"))

# 3. CREATE BINARY COLLAPSED VERSIONS
cat("Creating binary collapsed variables for robustness checks\n")
data$nerve_block_binary <- factor(
  ifelse(data$nerve_block %in% c("recommend_placement","strongly_recommend_placement"),
         "recommend","against"),
  levels = c("against","recommend"))

data$anesthetic_plan_binary <- factor(
  ifelse(data$anesthetic_plan %in% c("recommend_neuraxial","strongly_recommend_neuraxial"),
         "neuraxial","general"),
  levels = c("general","neuraxial"))

data$arterial_line_binary <- factor(
  ifelse(data$arterial_line %in% c("recommend_placement","strongly_recommend_placement"),
         "recommend","against"),
  levels = c("against","recommend"))

# 4. DESCRIPTIVE STATISTICS

cat("\nGenerating descriptive statistics\n")
sample_summary <- data %>%
  group_by(model_name, surgery_id, sex, variable_type, variable) %>%
  summarise(n_replicates = n(), .groups = "drop")

cat("\nSample size summary:\n")
print(sample_summary %>%
  group_by(model_name) %>%
  summarise(
    n_combinations = n(),
    total_observations = sum(n_replicates),
    .groups = "drop"
  ))

cat("\nMissing data check:\n")
missing_summary <- data %>%
  summarise(across(everything(), ~sum(is.na(.))))
if (any(colSums(missing_summary) > 0)) {
  print(t(missing_summary[, colSums(missing_summary) > 0]))
} else {
  cat("No missing values detected in any column.\n")
}

# 5–7. DESCRIPTIVE TABLES
# Function to create frequency tables
create_freq_table <- function(data, var_name) {
  data %>%
    group_by(model_name, !!sym(var_name)) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name) %>%
    mutate(
      proportion = n / sum(n),
      percentage = round(proportion * 100, 1)
    ) %>%
    arrange(model_name, !!sym(var_name))
}
nerve_block_freq     <- create_freq_table(data, "nerve_block")
anesthetic_plan_freq <- create_freq_table(data, "anesthetic_plan")
arterial_line_freq   <- create_freq_table(data, "arterial_line")
asa_freq             <- create_freq_table(data, "asa_classification")

cat("\n=== NERVE BLOCK Distribution ===\n");     print(nerve_block_freq)
cat("\n=== ANESTHESIA TYPE Distribution ===\n");print(anesthetic_plan_freq)
cat("\n=== ARTERIAL LINE Distribution ===\n");  print(arterial_line_freq)
cat("\n=== ASA Classification Distribution ===\n"); print(asa_freq)

# Condensed variable x outcome summaries
variable_outcome_summary <- data %>%
  group_by(model_name, variable, nerve_block) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(model_name, variable) %>%
  mutate(
    total = sum(n),
    percentage = round(n / total * 100, 1),
    model_display = format_model_name(as.character(model_name)),
    variable_display = format_label(as.character(variable))
  ) %>%
  select(model_display, variable_display, nerve_block, n, percentage)

variable_outcome_wide <- variable_outcome_summary %>%
  pivot_wider(
    names_from = nerve_block,
    values_from = c(n, percentage),
    names_glue = "{nerve_block}_{.value}"
  )
write.csv(variable_outcome_wide,
          "./output/01_descriptives/tables/variable_nerve_block_summary.csv",
          row.names = FALSE)

variable_anesthetic_summary <- data %>%
  group_by(model_name, variable, anesthetic_plan) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(model_name, variable) %>%
  mutate(
    total = sum(n),
    percentage = round(n / total * 100, 1),
    model_display = format_model_name(as.character(model_name)),
    variable_display = format_label(as.character(variable))
  ) %>%
  select(model_display, variable_display, anesthetic_plan, n, percentage)

variable_anesthetic_wide <- variable_anesthetic_summary %>%
  pivot_wider(
    names_from = anesthetic_plan,
    values_from = c(n, percentage),
    names_glue = "{anesthetic_plan}_{.value}"
  )
write.csv(variable_anesthetic_wide,
          "./output/01_descriptives/tables/variable_anesthetic_plan_summary.csv",
          row.names = FALSE)

variable_arterial_summary <- data %>%
  group_by(model_name, variable, arterial_line) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(model_name, variable) %>%
  mutate(
    total = sum(n),
    percentage = round(n / total * 100, 1),
    model_display = format_model_name(as.character(model_name)),
    variable_display = format_label(as.character(variable))
  ) %>%
  select(model_display, variable_display, arterial_line, n, percentage)

variable_arterial_wide <- variable_arterial_summary %>%
  pivot_wider(
    names_from = arterial_line,
    values_from = c(n, percentage),
    names_glue = "{arterial_line}_{.value}"
  )
write.csv(variable_arterial_wide,
          "./output/01_descriptives/tables/variable_arterial_line_summary.csv",
          row.names = FALSE)

variable_asa_summary <- data %>%
  group_by(model_name, variable, asa_classification) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(model_name, variable) %>%
  mutate(
    total = sum(n),
    percentage = round(n / total * 100, 1),
    model_display = format_model_name(as.character(model_name)),
    variable_display = format_label(as.character(variable))
  ) %>%
  select(model_display, variable_display, asa_classification, n, percentage)

variable_asa_wide <- variable_asa_summary %>%
  pivot_wider(
    names_from = asa_classification,
    values_from = c(n, percentage),
    names_glue = "ASA_{asa_classification}_{.value}"
  )
write.csv(variable_asa_wide,
          "./output/01_descriptives/tables/variable_asa_summary.csv",
          row.names = FALSE)

# Binary recommendation summary table
calc_recommendation_stats_binary <- function(data, outcome_var_name, outcome_label) {
  overall <- data %>%
    group_by(model_name, .data[[outcome_var_name]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name) %>%
    mutate(total = sum(n), percentage_overall = round(n/total*100, 1)) %>%
    select(model_name, level = .data[[outcome_var_name]], percentage_overall)

  control_only <- data %>%
    filter(variable == "control") %>%
    group_by(model_name, .data[[outcome_var_name]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name) %>%
    mutate(total = sum(n), percentage_control = round(n/total*100, 1)) %>%
    select(model_name, level = .data[[outcome_var_name]], percentage_control)

  overall %>%
    left_join(control_only, by = c("model_name","level")) %>%
    mutate(outcome = outcome_label, .before = 1, level = as.character(level))
}
rec_stats_nerve      <- calc_recommendation_stats_binary(data, "nerve_block_binary", "nerve_block")
rec_stats_anesthetic <- calc_recommendation_stats_binary(data, "anesthetic_plan_binary", "anesthetic_plan")
rec_stats_arterial   <- calc_recommendation_stats_binary(data, "arterial_line_binary", "arterial_line")

recommendation_statistics <- bind_rows(rec_stats_nerve, rec_stats_anesthetic, rec_stats_arterial) %>%
  mutate(
    model_display   = format_model_name(as.character(model_name)),
    outcome_display = format_label(as.character(outcome))
  ) %>%
  filter(
    (outcome == "nerve_block"      & level == "recommend") |
    (outcome == "anesthetic_plan"  & level == "neuraxial") |
    (outcome == "arterial_line"    & level == "recommend")
  ) %>%
  select(outcome_display, model_display, level, percentage_overall, percentage_control)
write.csv(recommendation_statistics,
          "./output/01_descriptives/tables/recommendation_statistics.csv",
          row.names = FALSE)

# 8. VISUALIZATIONS — COMPACT COMPOSITES
data_for_plots <- data %>%
  filter(variable != "preference_ga") %>%
  droplevels()

# Palettes
colors_4level <- c("#b2182b", "#ef8a62", "#67a9cf", "#2166ac")
colors_asa6   <- c("#edf8fb", "#b2e2e2", "#66c2a4", "#2ca25f", "#006d2c", "#00441b")

# Global bar widths for consistent thickness/spacing
BAR_WIDTH_SINGLE <- 0.90  # Plot 1
BAR_WIDTH_MULTI  <- 0.85  # Plots 2–5

# Compact theme
theme_compact <- theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text  = element_text(size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.box.spacing = unit(0.2, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.text.x  = element_text(size = 7),
    axis.text.y  = element_text(size = 7, hjust = 1, margin = margin(r = 1)),
    axis.title   = element_text(size = 7),
    plot.title   = element_text(size = 9),
    strip.text   = element_text(size = 7),
    panel.grid   = element_blank(),
    plot.margin  = margin(2, 2, 2, 1)
  )

mk_outcome_display <- function(df, var) {
  lvls <- levels(df[[var]])
  factor(format_label(as.character(df[[var]])), levels = format_label(lvls))
}

# ---------- Builders (string-based; no quosures) ----------
plot_overall_by_model <- function(df, outcome_var, title, palette, bar_width = BAR_WIDTH_SINGLE) {
  lvls <- levels(df[[outcome_var]])
  lvls_model <- levels(df[["model_name"]])

  ov <- df %>%
    group_by(model_name, .data[[outcome_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name) %>%
    mutate(
      percentage = n / sum(n) * 100,
      model_display = factor(
        format_model_name(as.character(model_name)),
        levels = format_model_name(lvls_model)
      )
    ) %>%
    ungroup() %>%
    mutate(outcome_display = factor(
      format_label(as.character(.data[[outcome_var]])),
      levels = format_label(lvls)
    ))

  ggplot(ov, aes(x = percentage, y = model_display, fill = outcome_display)) +
    geom_bar(stat = "identity", width = bar_width) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = palette) +
    labs(title = title, x = NULL, y = NULL, fill = format_label(outcome_var)) +
    theme_compact +
    guides(fill = guide_legend(ncol = 2)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02)))
}

plot_by_group <- function(df, outcome_var, group_var, title, palette,
                          xlab, bar_width = BAR_WIDTH_MULTI) {
  lvls_outcome <- levels(df[[outcome_var]])
  lvls_group <- levels(df[[group_var]])
  lvls_model <- levels(df[["model_name"]])

  dat <- df %>%
    group_by(model_name, .data[[group_var]], .data[[outcome_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name, .data[[group_var]]) %>%
    mutate(
      percentage = n / sum(n) * 100,
      model_display = factor(
        format_model_name(as.character(model_name)),
        levels = format_model_name(lvls_model)
      )
    ) %>%
    ungroup() %>%
    mutate(
      group_display = if (group_var == "sex")
        factor(str_to_title(as.character(.data[[group_var]])),
               levels = str_to_title(lvls_group))
      else
        factor(format_label(as.character(.data[[group_var]])),
               levels = format_label(lvls_group)),
      outcome_display = factor(
        format_label(as.character(.data[[outcome_var]])),
        levels = format_label(lvls_outcome)
      )
    )

  ggplot(dat, aes(x = percentage, y = group_display, fill = outcome_display)) +
    geom_bar(stat = "identity", width = bar_width) +                # uniform thickness
    facet_wrap(~ model_display, ncol = 1) +
    scale_fill_manual(values = palette) +
    labs(title = title, x = NULL, y = NULL, fill = format_label(outcome_var)) +
    theme_compact +
    theme(panel.spacing = unit(0.1, "lines")) +
    guides(fill = guide_legend(ncol = 2)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02)))
}

plot_variable_outcome <- function(df, outcome_var, title, palette) {
  plot_by_group(df, outcome_var, "variable",   title, palette, xlab = "Variable")
}
plot_by_surgery <- function(df, outcome_var, title, palette) {
  plot_by_group(df, outcome_var, "surgery_id", title, palette, xlab = "Surgery type")
}
plot_by_sex <- function(df, outcome_var, title, palette) {
  plot_by_group(df, outcome_var, "sex",        title, palette, xlab = "Sex")
}

# Counts for dynamic sizing
n_variables <- length(unique(data_for_plots$variable))
n_surgeries <- length(unique(data_for_plots$surgery_id))
n_sex       <- length(unique(data_for_plots$sex))
n_models    <- length(unique(data_for_plots$model_name))

h_overall_block <- (n_models * 0.35 + 0.9)
h_plot1 <- 3 * h_overall_block + 1.0

h_var    <- n_variables * 0.30 + n_models * 0.45 + 0.6
h_surg   <- n_surgeries * 0.30 + n_models * 0.45 + 0.6
h_sex    <- n_sex * 0.30 + n_models * 0.45 + 0.6
h_combo  <- max(h_var, (h_surg + h_sex + 0.7))
w_combo  <- 8.5

# ---------- PLOT 1: three separate legends, directly under each subplot ----------
p1_ap <- plot_overall_by_model(data_for_plots, "anesthetic_plan",
           "Anesthesia type by model", colors_4level,
           bar_width = BAR_WIDTH_SINGLE) + theme(legend.position = "bottom")
p1_nb <- plot_overall_by_model(data_for_plots, "nerve_block",
           "Nerve block by model", colors_4level,
           bar_width = BAR_WIDTH_SINGLE) + theme(legend.position = "bottom")
p1_al <- plot_overall_by_model(data_for_plots, "arterial_line",
           "Arterial line by model", colors_4level,
           bar_width = BAR_WIDTH_SINGLE) + theme(legend.position = "bottom")

plot1_overall <- (p1_ap / p1_nb / p1_al) + plot_layout(guides = "keep")
ggsave("./output/01_descriptives/figures/plot1_overall_preferences.png",
       plot1_overall, width = 7, height = h_plot1, dpi = 300)

# ---------- OVERALL ASA PLOT (separate from plot 1) ----------
p_asa_overall <- plot_overall_by_model(data_for_plots, "asa_classification",
           "ASA classification by model", colors_asa6,
           bar_width = BAR_WIDTH_SINGLE) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 3))

h_asa_overall <- (n_models * 0.35 + 0.9)
ggsave("./output/01_descriptives/figures/plot_asa_overall.png",
       p_asa_overall, width = 7, height = h_asa_overall, dpi = 300)

# ---------- Two-column composite helper (shared legend under right column) ----------
build_two_col_composite <- function(p_left, p_top_right, p_bottom_right,
                                    n_top, n_bottom, legend_height = 0.001) {
  leg <- patchwork::guide_area()
  right_col <- (p_top_right / p_bottom_right / leg) +
    plot_layout(heights = c(n_top, n_bottom, legend_height))  # proportional heights
  (p_left | right_col) +
    plot_layout(widths = c(1, 1.05), guides = "collect") &
    theme(legend.position = "bottom")
}

# ---------- PLOT 2: Nerve block ----------
p2_left <- plot_variable_outcome(data_for_plots, "nerve_block", "Nerve block by variable", colors_4level)
p2_tr   <- plot_by_surgery(data_for_plots,  "nerve_block", "Nerve block by surgery", colors_4level)
p2_br   <- plot_by_sex(data_for_plots,      "nerve_block", "Nerve block by sex",     colors_4level)
plot2_nb <- build_two_col_composite(p2_left, p2_tr, p2_br,
                                    n_surgeries, n_sex)
ggsave("./output/01_descriptives/figures/plot2_nerve_block_combo.png",
       plot2_nb, width = w_combo, height = h_combo, dpi = 300)

# ---------- PLOT 3: Anesthesia type ----------
p3_left <- plot_variable_outcome(data_for_plots, "anesthetic_plan", "Anesthesia type by variable", colors_4level)
p3_tr   <- plot_by_surgery(data_for_plots,  "anesthetic_plan", "Anesthesia type by surgery", colors_4level)
p3_br   <- plot_by_sex(data_for_plots,      "anesthetic_plan", "Anesthesia type by sex",     colors_4level)
plot3_ap <- build_two_col_composite(p3_left, p3_tr, p3_br,
                                    n_surgeries, n_sex)
ggsave("./output/01_descriptives/figures/plot3_anesthetic_plan_combo.png",
       plot3_ap, width = w_combo, height = h_combo, dpi = 300)

# ---------- PLOT 4: Arterial line ----------
p4_left <- plot_variable_outcome(data_for_plots, "arterial_line", "Arterial line by variable", colors_4level)
p4_tr   <- plot_by_surgery(data_for_plots,  "arterial_line", "Arterial line by surgery", colors_4level)
p4_br   <- plot_by_sex(data_for_plots,      "arterial_line", "Arterial line by sex",     colors_4level)
plot4_al <- build_two_col_composite(p4_left, p4_tr, p4_br,
                                    n_surgeries, n_sex)
ggsave("./output/01_descriptives/figures/plot4_arterial_line_combo.png",
       plot4_al, width = w_combo, height = h_combo, dpi = 300)

# ---------- PLOT 5: ASA ----------
plot_overall_asa_by_group <- function(df, group_var, title) {
  lvls <- levels(df[["asa_classification"]])
  lvls_group <- levels(df[[group_var]])
  lvls_model <- levels(df[["model_name"]])

  dat <- df %>%
    group_by(model_name, .data[[group_var]], .data[["asa_classification"]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(model_name, .data[[group_var]]) %>%
    mutate(
      percentage = n / sum(n) * 100,
      model_display = factor(
        format_model_name(as.character(model_name)),
        levels = format_model_name(lvls_model)
      )
    ) %>%
    ungroup() %>%
    mutate(
      group_display = if (group_var == "sex")
        factor(str_to_title(as.character(.data[[group_var]])),
               levels = str_to_title(lvls_group))
      else
        factor(format_label(as.character(.data[[group_var]])),
               levels = format_label(lvls_group)),
      outcome_display = factor(
        format_label(as.character(.data[["asa_classification"]])),
        levels = format_label(lvls)
      )
    )

  ggplot(dat, aes(x = percentage, y = group_display, fill = outcome_display)) +
    geom_bar(stat = "identity", width = BAR_WIDTH_MULTI) +
    facet_wrap(~ model_display, ncol = 1) +
    scale_fill_manual(values = colors_asa6) +
    labs(title = title,
         x = NULL,
         y = NULL,
         fill = "ASA classification") +
    theme_compact +
    theme(panel.spacing = unit(0.1, "lines")) +
    guides(fill = guide_legend(ncol = 3)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.02)))
}

p5_left <- plot_overall_asa_by_group(data_for_plots, "variable",   "ASA by variable")
p5_tr   <- plot_overall_asa_by_group(data_for_plots, "surgery_id", "ASA by surgery")
p5_br   <- plot_overall_asa_by_group(data_for_plots, "sex",        "ASA by sex")
plot5_asa <- build_two_col_composite(p5_left, p5_tr, p5_br,
                                     n_surgeries, n_sex)
ggsave("./output/01_descriptives/figures/plot5_asa_combo.png",
       plot5_asa, width = w_combo, height = h_combo, dpi = 300)


# 9. SAVE PREPARED DATASET
saveRDS(data, "./data/prepared_data.rds")
write.csv(data, "./data/prepared_data.csv", row.names = FALSE)