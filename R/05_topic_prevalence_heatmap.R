# SCRIPT 5: Topic Prevalence Heatmap by Model and Plan
# Purpose: Visualize keyword prevalence across models and anesthesia plans

# 1. SETUP 

# Load required packages
library(tidyverse)     # Data manipulation and plotting
library(scales)        # For percent formatting

# Label formatting functions (matching existing scripts)
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
  label <- if_else(
    label %in% c("COPD", "Drug use", "American Indian", "Middle Eastern", "Pacific Islander",
                 "GA airway", "GA technique", "LMA", "ETT", "TIVA", "FICB", "PENG", "Suprainguinal FICB",
                 "Infrainguinal FICB", "Anesthesia type"),
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

# Create output directory if it doesn't exist
dir.create("./output/05_topic_prevalence", showWarnings = FALSE, recursive = TRUE)

cat("\n=\n")
cat("SCRIPT 8: Topic Prevalence Heatmap by Model and Plan\n")
cat("=\n\n")

# Configuration: Topics to exclude (uncomment or add topics to exclude)
EXCLUDE_TOPICS <- c(
  "sex", "race_ethnicity", "social_factors"
)

# 2. LOAD DATA

cat("Loading regex summary data\n")
data <- read_csv("./topics/regex_summary.csv", show_col_types = FALSE)

cat("Loaded", nrow(data), "rows\n")
cat("Models:", unique(data$model), "\n")
cat("Plans:", unique(data$plan), "\n")
cat("Topics:", unique(data$topic), "\n")

# Filter out excluded topics
if (length(EXCLUDE_TOPICS) > 0) {
  cat("Excluding topics:", paste(EXCLUDE_TOPICS, collapse = ", "), "\n")
  data <- data %>%
    filter(!topic %in% EXCLUDE_TOPICS)
}

cat("Topics to plot:", unique(data$topic), "\n\n")

# 3. PREPARE DATA

cat("Preparing data for visualization\n")

# Format labels
plot_data <- data %>%
  mutate(
    model_display = format_model_name(model),
    topic_display = format_label(topic),
    # Ensure factor ordering for models
    model_display = factor(model_display, levels = c("DeepSeek 3.2", "Gemini 2.5 Flash", "GPT-5", "GPT-5 mini", "GPT-5 nano"))
  )

# Order topics by average prevalence
topic_order <- plot_data %>%
  group_by(topic_display) %>%
  summarise(avg_prevalence = mean(prevalence_pct, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_prevalence)) %>%
  pull(topic_display)

plot_data <- plot_data %>%
  mutate(topic_display = factor(topic_display, levels = topic_order))

# 4. CREATE HEATMAP 

cat("Creating heatmap\n")

p <- ggplot(plot_data, aes(x = model_display, y = topic_display, fill = prevalence_pct)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", prevalence_pct)),
            size = 3, color = "black") +
  facet_wrap(~ plan, nrow = 1) +
  scale_fill_gradient2(
    low = "#f7fbff",
    mid = "#6baed6",
    high = "#08306b",
    midpoint = 25,
    limits = c(0, NA),
    name = "Prevalence (%)",
    labels = function(x) sprintf("%.0f%%", x)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
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

# 5. SAVE OUTPUT 

output_file <- "./output/05_topic_prevalence/keyword_prevalence_heatmap.png"

cat("Saving heatmap\n")
ggsave(output_file, p, width = 10, height = 8, dpi = 300)

cat("  Saved to:", output_file, "\n")