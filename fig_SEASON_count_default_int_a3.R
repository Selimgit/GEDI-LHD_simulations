# ============================================================
# fig_SEASON_count_default_int_a3.R
# Bias profiles, Leaf On vs Leaf Off, count-default vs int-a3.
# 2x2 panel: high sensitivity (>0.96) and mid sensitivity (0.90, 0.96]
# -> PLOTS/SEASON_count_default_int_a3.png
# Also produces the accompanying bias/RMSE/R2 table by percentile,
# sensitivity group, and leaf status.
# ============================================================

library(ggplot2)
library(patchwork)
source("00_functions.R")
source("01_load_data.R")
source("03_ribbon_plot_helper.R")

all_percentiles <- c(5, 10, 25, 50, 70, 90, 98)

cfg_count_def <- get_cfg("count", "default")
cfg_int_a3    <- get_cfg("int",   "a3")

res_default_high <- prepare_leaf_status_results(raw$count, cfg_count_def, sens_bands$high, all_percentiles)
res_default_mid  <- prepare_leaf_status_results(raw$count, cfg_count_def, sens_bands$mid,  all_percentiles)
res_a3_high      <- prepare_leaf_status_results(raw$int,   cfg_int_a3,    sens_bands$high, all_percentiles)
res_a3_mid       <- prepare_leaf_status_results(raw$int,   cfg_int_a3,    sens_bands$mid,  all_percentiles)

# ---- Shared x limits ----
fixed_x_limits <- c(-1.5, 3.25)

# ---- Shared gradient limits ----
diff_for <- function(res) {
  abs(res$bias$bias[res$bias$leaf_status == "Leaf On"] - res$bias$bias[res$bias$leaf_status == "Leaf Off"])
}
all_abs_diff <- c(diff_for(res_default_high), diff_for(res_default_mid), diff_for(res_a3_high), diff_for(res_a3_mid))
shared_gradient_max <- ceiling(max(all_abs_diff, na.rm = TRUE) * 10) / 10

# ---- Build panels ----
p_default_high <- plot_ribbon(res_default_high$bias, res_default_high$overall,
                              "count-default high sensitivity shots", fixed_x_limits, c(0, shared_gradient_max), all_percentiles)
p_a3_high      <- plot_ribbon(res_a3_high$bias, res_a3_high$overall,
                              "int-a3 high sensitivity shots",        fixed_x_limits, c(0, shared_gradient_max), all_percentiles)
p_default_mid  <- plot_ribbon(res_default_mid$bias, res_default_mid$overall,
                              "count-default low sensitivity shots",  fixed_x_limits, c(0, shared_gradient_max), all_percentiles)
p_a3_mid       <- plot_ribbon(res_a3_mid$bias, res_a3_mid$overall,
                              "int-a3 low sensitivity shots",         fixed_x_limits, c(0, shared_gradient_max), all_percentiles)

panel_2x2 <- (p_default_high | p_a3_high) / (p_default_mid | p_a3_mid) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right", legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"))

# ggsave("PLOTS/SEASON_count_default_int_a3.png", panel_2x2, width = 14, height = 12, dpi = 500)
# cat("\nPlot saved to PLOTS/SEASON_count_default_int_a3.png\n")

theme_set(theme_get() + theme(text = element_text(family = "Arial")))
ggsave("PLOTS/SEASON_count_default_int_a3.pdf", panel_2x2, width = 14, height = 12, device = cairo_pdf)

cat("\nPlot saved to PLOTS/SEASON_count_default_int_a3.pdf\n")

# ============================================================
# Table: bias, RMSE, R2 by percentile, sensitivity group, leaf status
# ============================================================

table_percentiles <- c(25, 50, 70, 98)

process_combination <- function(df_raw, cfg, group_name, sens_params) {
  filtered <- apply_base_filter(df_raw, cfg,
                                sens_lower = sens_params$min,
                                sens_upper = if (is.infinite(sens_params$max)) NULL else sens_params$max)
  if (isTRUE(sens_params$excl_lower)) {
    filtered <- filtered %>% filter(.data[[cfg$sens_col]] > sens_params$min)
  }

  map_dfr(table_percentiles, function(p) {
    on_metrics  <- compute_metrics_for_subset(filtered, cfg, p, 0) %>% mutate(leaf_status = "Leaf On")
    off_metrics <- compute_metrics_for_subset(filtered, cfg, p, 1) %>% mutate(leaf_status = "Leaf Off")
    bind_rows(on_metrics, off_metrics) %>% mutate(percentile = p)
  }) %>%
    mutate(dataset = paste0(cfg$weighting, "-", cfg$algo), sensitivity_group = group_name) %>%
    select(dataset, sensitivity_group, leaf_status, percentile, bias, rmse, r2, n)
}

final_table <- bind_rows(
  process_combination(raw$count, cfg_count_def, "high", sens_bands$high),
  process_combination(raw$count, cfg_count_def, "mid",  sens_bands$mid),
  process_combination(raw$int,   cfg_int_a3,    "high", sens_bands$high),
  process_combination(raw$int,   cfg_int_a3,    "mid",  sens_bands$mid)
) %>%
  arrange(dataset, sensitivity_group, leaf_status, percentile)

print(final_table)
write.csv(final_table, "TABLES/SEASON_count_default_int_a3_metrics.csv", row.names = FALSE)
