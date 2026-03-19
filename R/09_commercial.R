# Script 9: Commercial Model Sensitivity Analysis
# Purpose: Create a supplemental figure showing control-vignette preference
#          distributions for all models and conditional outcome distributions
#          for the two commercial medical-grade models.

# Set working directory
setwd("/Users/robertchen/Documents/GitHub/LLM_hip/analysis")

# Load required packages
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(patchwork)

# Create output directories
output_dir <- "./output/09_commercial"
figure_dir <- file.path(output_dir, "figures")
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)

figure_dpi <- 600

main_topic_file <- "./topics/control_regex_summary.csv"
commercial_topic_file <- "./topics/commercial_regex_summary.tsv"

if (!file.exists(main_topic_file) || !file.exists(commercial_topic_file)) {
  stop("Topic summaries not found in ./topics. Run analysis/topics/analysis.py first.")
}

format_label <- function(x) {
  label <- str_replace_all(x, "_", " ")
  label <- case_when(
    tolower(label) == "ga airway" ~ "GA airway",
    tolower(label) == "ga technique" ~ "GA technique",
    tolower(label) == "lma" ~ "LMA",
    tolower(label) == "ett" ~ "ETT",
    tolower(label) == "tiva" ~ "TIVA",
    tolower(label) == "ficb" ~ "FICB",
    tolower(label) == "peng" ~ "PENG",
    tolower(label) == "suprainguinal ficb" ~ "Suprainguinal FICB",
    tolower(label) == "infrainguinal ficb" ~ "Infrainguinal FICB",
    tolower(label) == "anesthetic plan" ~ "Anesthesia type",
    TRUE ~ label
  )
  label <- ifelse(
    label %in% c("GA airway", "GA technique", "LMA", "ETT", "TIVA",
                 "FICB", "PENG", "Suprainguinal FICB", "Infrainguinal FICB",
                 "Anesthesia type"),
    label,
    str_to_sentence(label)
  )
  label
}

format_model_name <- function(x) {
  case_when(
    x == "deepseek" ~ "DeepSeek 3.2",
    x == "gemini-2.5-flash-preview-09-2025" ~ "Gemini 2.5 Flash",
    x == "gpt-5-2025-08-07" ~ "GPT-5",
    x == "gpt-5-mini-2025-08-07" ~ "GPT-5 mini",
    x == "gpt-5-nano-2025-08-07" ~ "GPT-5 nano",
    x == "openevidence_031726" ~ "OpenEvidence",
    x == "doxgpt_031726" ~ "Doximity GPT",
    TRUE ~ x
  )
}

model_levels <- c(
  "deepseek",
  "gemini-2.5-flash-preview-09-2025",
  "gpt-5-2025-08-07",
  "gpt-5-mini-2025-08-07",
  "gpt-5-nano-2025-08-07",
  "openevidence_031726",
  "doxgpt_031726"
)

average_model_levels <- c(
  "deepseek",
  "gemini-2.5-flash-preview-09-2025",
  "gpt-5-2025-08-07"
)

conditional_display_levels <- c(
  "3-model average",
  "OpenEvidence",
  "Doximity GPT"
)

colors_4level <- c("#b2182b", "#ef8a62", "#67a9cf", "#2166ac")
conditional_palette <- c(
  "3-model average" = "#66c2a5",
  "OpenEvidence" = "#fc8d62",
  "Doximity GPT" = "#8da0cb"
)

theme_compact <- theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.05, "cm"),
    legend.box.spacing = unit(0.2, "cm"),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7, hjust = 1, margin = margin(r = 1)),
    axis.title = element_text(size = 7),
    plot.title = element_text(size = 9),
    strip.text = element_text(size = 7),
    panel.grid = element_blank(),
    plot.margin = margin(2, 2, 2, 1)
  )

cat("Loading datasets\n")

data_openai <- read_tsv("./data/openai.tsv", show_col_types = FALSE)
data_gemini <- read_tsv("./data/gemini.tsv", show_col_types = FALSE)
data_deepseek <- read_tsv("./data/deepseek.tsv", show_col_types = FALSE)
data_commercial <- read_tsv("./data/commercial.tsv", show_col_types = FALSE)

data_raw <- bind_rows(data_openai, data_gemini, data_deepseek, data_commercial)

data_control <- data_raw %>%
  filter(variable == "control") %>%
  mutate(
    model_name = factor(model_name, levels = model_levels),
    nerve_block = factor(
      nerve_block,
      levels = c(
        "strongly_recommend_against_placement",
        "recommend_against_placement",
        "recommend_placement",
        "strongly_recommend_placement"
      ),
      ordered = TRUE
    ),
    anesthetic_plan = factor(
      anesthetic_plan,
      levels = c(
        "strongly_recommend_general",
        "recommend_general",
        "recommend_neuraxial",
        "strongly_recommend_neuraxial"
      ),
      ordered = TRUE
    ),
    arterial_line = factor(
      arterial_line,
      levels = c(
        "strongly_recommend_against_placement",
        "recommend_against_placement",
        "recommend_placement",
        "strongly_recommend_placement"
      ),
      ordered = TRUE
    ),
    nerve_block_type = factor(
      nerve_block_type,
      levels = c("PENG", "suprainguinal_FICB", "infrainguinal_FICB", "femoral", "none")
    ),
    ga_airway = factor(ga_airway, levels = c("LMA", "ETT", "none")),
    ga_technique = factor(ga_technique, levels = c("TIVA", "sevoflurane", "none")),
    neuraxial_type = factor(
      neuraxial_type,
      levels = c("spinal_only", "epidural_only", "combined_spinal_epidural", "none")
    )
  )

plot_overall_by_model <- function(df, outcome_var, title, palette) {
  lvls <- levels(df[[outcome_var]])
  model_percentages <- df %>%
    group_by(model_name, .data[[outcome_var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    complete(
      model_name = model_levels,
      !!sym(outcome_var) := lvls,
      fill = list(n = 0)
    ) %>%
    group_by(model_name) %>%
    mutate(
      percentage = if (sum(n) > 0) n / sum(n) * 100 else 0
    ) %>%
    ungroup()

  averaged_general <- model_percentages %>%
    filter(model_name %in% average_model_levels) %>%
    group_by(.data[[outcome_var]]) %>%
    summarise(percentage = mean(percentage), .groups = "drop") %>%
    mutate(model_display = "3-model average")

  commercial_models <- model_percentages %>%
    filter(model_name %in% c("openevidence_031726", "doxgpt_031726")) %>%
    mutate(model_display = format_model_name(as.character(model_name))) %>%
    select(-model_name)

  plot_data <- bind_rows(averaged_general, commercial_models) %>%
    mutate(
      model_display = factor(model_display, levels = conditional_display_levels),
      outcome_display = factor(
        format_label(as.character(.data[[outcome_var]])),
        levels = format_label(lvls)
      )
    )

  ggplot(plot_data, aes(x = percentage, y = model_display, fill = outcome_display)) +
    geom_bar(stat = "identity", width = 0.9) +
    scale_y_discrete(limits = rev) +
    scale_fill_manual(values = palette) +
    labs(title = title, x = NULL, y = NULL, fill = format_label(outcome_var)) +
    theme_compact +
    guides(fill = guide_legend(ncol = 2)) +
    scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02)))
}

conditional_outcomes <- list(
  ga_airway = list(
    outcome = "ga_airway",
    filter_var = "anesthetic_plan",
    filter_values = c("strongly_recommend_general", "recommend_general"),
    title = "General anesthesia airway"
  ),
  ga_technique = list(
    outcome = "ga_technique",
    filter_var = "anesthetic_plan",
    filter_values = c("strongly_recommend_general", "recommend_general"),
    title = "General anesthesia technique"
  ),
  neuraxial_type = list(
    outcome = "neuraxial_type",
    filter_var = "anesthetic_plan",
    filter_values = c("recommend_neuraxial", "strongly_recommend_neuraxial"),
    title = "Neuraxial anesthesia type"
  ),
  nerve_block_type = list(
    outcome = "nerve_block_type",
    filter_var = "nerve_block",
    filter_values = c("recommend_placement", "strongly_recommend_placement"),
    title = "Nerve block type"
  )
)

create_conditional_plot <- function(outcome_info, data) {
  filtered_data <- data %>%
    filter(.data[[outcome_info$filter_var]] %in% outcome_info$filter_values) %>%
    filter(.data[[outcome_info$outcome]] != "none")

  all_outcome_levels <- levels(filtered_data[[outcome_info$outcome]])
  all_outcome_levels <- all_outcome_levels[all_outcome_levels != "none"]

  model_percentages <- filtered_data %>%
    group_by(model_name, .data[[outcome_info$outcome]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    complete(
      model_name = model_levels,
      !!sym(outcome_info$outcome) := all_outcome_levels,
      fill = list(n = 0)
    ) %>%
    group_by(model_name) %>%
    mutate(
      percentage = if (sum(n) > 0) n / sum(n) * 100 else 0
    ) %>%
    ungroup()

  averaged_general <- model_percentages %>%
    filter(model_name %in% average_model_levels) %>%
    group_by(.data[[outcome_info$outcome]]) %>%
    summarise(percentage = mean(percentage), .groups = "drop") %>%
    mutate(model_display = "3-model average")

  commercial_models <- model_percentages %>%
    filter(model_name %in% c("openevidence_031726", "doxgpt_031726")) %>%
    mutate(model_display = format_model_name(as.character(model_name))) %>%
    select(-model_name)

  plot_data <- bind_rows(averaged_general, commercial_models) %>%
    mutate(
      model_display = factor(model_display, levels = conditional_display_levels),
      outcome_display = factor(
        format_label(as.character(.data[[outcome_info$outcome]])),
        levels = format_label(all_outcome_levels)
      )
    )

  ggplot(plot_data, aes(x = outcome_display, y = percentage, fill = model_display)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(
      aes(label = sprintf("%.0f%%", round(percentage))),
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      size = 3.2
    ) +
    labs(
      title = outcome_info$title,
      x = NULL,
      y = "Percent of responses",
      fill = "Model"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
    scale_fill_manual(values = conditional_palette) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(hjust = 0.5, size = 7),
      axis.text.y = element_text(size = 7),
      axis.title = element_text(size = 8),
      plot.title = element_text(size = 9),
      legend.position = "bottom",
      legend.title = element_text(size = 7),
      legend.text = element_text(size = 7),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = unit(c(1, 1, 1, 1), "pt")
    ) +
    guides(fill = guide_legend(ncol = 3))
}

cat("Building plots\n")

p_overall_ap <- plot_overall_by_model(
  data_control, "anesthetic_plan", "Anesthesia type by model", colors_4level
) +
  labs(x = "Percent of model responses")

p_overall_nb <- plot_overall_by_model(
  data_control, "nerve_block", "Nerve block by model", colors_4level
) +
  labs(x = "Percent of model responses")

p_overall_al <- plot_overall_by_model(
  data_control, "arterial_line", "Arterial line by model", colors_4level
) +
  labs(x = "Percent of model responses")

overall_panel <- p_overall_ap / p_overall_nb / p_overall_al

conditional_plots <- list(
  create_conditional_plot(conditional_outcomes$ga_airway, data_control),
  create_conditional_plot(conditional_outcomes$ga_technique, data_control),
  create_conditional_plot(conditional_outcomes$neuraxial_type, data_control),
  create_conditional_plot(conditional_outcomes$nerve_block_type, data_control)
)

conditional_panel <- wrap_plots(plotlist = conditional_plots, ncol = 2)

make_section_label <- function(label) {
  wrap_elements(
    full = grid::textGrob(
      label,
      x = 0,
      hjust = 0,
      gp = grid::gpar(fontsize = 9)
    )
  )
}

overall_section <- wrap_plots(
  plotlist = list(make_section_label("A. Model recommendations for primary outcomes"), overall_panel),
  ncol = 1,
  heights = c(0.03, 1)
)

conditional_section <- wrap_plots(
  plotlist = list(make_section_label("B. Model recommendations for secondary outcomes"), conditional_panel),
  ncol = 1,
  heights = c(0.03, 1)
)

final_plot <- wrap_plots(
  plotlist = list(
    wrap_elements(full = overall_section),
    plot_spacer(),
    wrap_elements(full = conditional_section)
  ),
  ncol = 1,
  heights = c(1.2, 0.025, 1)
)

figure_file <- file.path(figure_dir, "commercial_model_sensitivity.jpg")
ggsave(figure_file, final_plot, width = 12, height = 13, dpi = figure_dpi, bg = "white")

cat("Saved figure to:", figure_file, "\n")

# Topic prevalence heatmap
cat("Creating commercial topic prevalence heatmap\n")

topic_data_main <- read_csv(main_topic_file, show_col_types = FALSE)
topic_data_commercial <- read_tsv(commercial_topic_file, show_col_types = FALSE)

topic_general_average <- topic_data_main %>%
  filter(model %in% average_model_levels) %>%
  group_by(plan, topic) %>%
  summarise(
    count = mean(count),
    total_applicable = mean(total_applicable),
    prevalence = mean(prevalence),
    prevalence_pct = mean(prevalence_pct),
    .groups = "drop"
  ) %>%
  mutate(model_display = "3-model average")

topic_commercial <- topic_data_commercial %>%
  filter(model %in% c("openevidence_031726", "doxgpt_031726")) %>%
  mutate(model_display = format_model_name(model))

topic_plot_data <- bind_rows(topic_general_average, topic_commercial) %>%
  filter(!(model_display == "Doximity GPT" & plan == "General")) %>%
  mutate(
    topic_display = format_label(topic),
    model_display = factor(model_display, levels = conditional_display_levels),
    plan = factor(plan, levels = c("General", "Neuraxial"))
  )

topic_order <- topic_plot_data %>%
  group_by(topic_display) %>%
  summarise(avg_prevalence = mean(prevalence_pct, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_prevalence)) %>%
  pull(topic_display)

topic_plot_data <- topic_plot_data %>%
  mutate(topic_display = factor(topic_display, levels = topic_order))

topic_heatmap <- ggplot(topic_plot_data, aes(x = model_display, y = topic_display, fill = prevalence_pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", prevalence_pct)), size = 3, color = "black") +
  facet_grid(. ~ plan, scales = "free_x", space = "free_x") +
  scale_x_discrete(drop = TRUE) +
  scale_fill_gradient2(
    low = "#f7fbff",
    mid = "#6baed6",
    high = "#08306b",
    midpoint = 25,
    limits = c(0, NA),
    name = "Prevalence (%)",
    labels = function(x) sprintf("%.0f%%", x)
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray90", color = NA),
    panel.grid = element_blank(),
    legend.position = "right",
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1)
  )

topic_heatmap_file <- file.path(figure_dir, "commercial_keyword_prevalence_heatmap.jpg")
ggsave(topic_heatmap_file, topic_heatmap, width = 8, height = 8, dpi = figure_dpi, bg = "white")

cat("Saved figure to:", topic_heatmap_file, "\n")
