# ============================================================
# fig_SEASON_count_a3_frac_a3.R
# Bias profiles, Leaf On vs Leaf Off, count-a3 vs frac-a3.
# 2x2 panel: high sensitivity (>0.96) and mid sensitivity (0.90, 0.96]
# -> PLOTS/SEASON_count_a3_frac_a3.png
# ============================================================

library(ggplot2)
library(patchwork)
source("00_functions.R")
source("01_load_data.R")
source("03_ribbon_plot_helper.R")

all_percentiles <- c(5, 10, 25, 50, 70, 90, 98)

cfg_count_a3 <- get_cfg("count", "a3")
cfg_frac_a3  <- get_cfg("frac",  "a3")

res_count_a3_high <- prepare_leaf_status_results(raw$count, cfg_count_a3, sens_bands$high, all_percentiles)
res_count_a3_mid  <- prepare_leaf_status_results(raw$count, cfg_count_a3, sens_bands$mid,  all_percentiles)
res_frac_a3_high  <- prepare_leaf_status_results(raw$frac,  cfg_frac_a3,  sens_bands$high, all_percentiles)
res_frac_a3_mid   <- prepare_leaf_status_results(raw$frac,  cfg_frac_a3,  sens_bands$mid,  all_percentiles)

# ---- Shared x limits ----
fixed_x_limits <- c(-1.5, 3.25)   # ou garde le calcul auto si tu préfères

# ---- Shared gradient limits ----
diff_for <- function(res) {
  abs(res$bias$bias[res$bias$leaf_status == "Leaf On"] - res$bias$bias[res$bias$leaf_status == "Leaf Off"])
}
all_abs_diff <- c(diff_for(res_count_a3_high), diff_for(res_count_a3_mid), diff_for(res_frac_a3_high), diff_for(res_frac_a3_mid))
shared_gradient_max <- ceiling(max(all_abs_diff, na.rm = TRUE) * 10) / 10

# ---- Build panels ----
p_count_a3_high <- plot_ribbon(res_count_a3_high$bias, res_count_a3_high$overall,
                               "count-a3 high sensitivity shots", fixed_x_limits, c(0, shared_gradient_max), all_percentiles)
p_frac_a3_high  <- plot_ribbon(res_frac_a3_high$bias, res_frac_a3_high$overall,
                               "frac-a3 high sensitivity shots",  fixed_x_limits, c(0, shared_gradient_max), all_percentiles)
p_count_a3_mid  <- plot_ribbon(res_count_a3_mid$bias, res_count_a3_mid$overall,
                               "count-a3 low sensitivity shots",  fixed_x_limits, c(0, shared_gradient_max), all_percentiles)
p_frac_a3_mid   <- plot_ribbon(res_frac_a3_mid$bias, res_frac_a3_mid$overall,
                               "frac-a3 low sensitivity shots",   fixed_x_limits, c(0, shared_gradient_max), all_percentiles)

panel_2x2 <- (p_count_a3_high | p_frac_a3_high) / (p_count_a3_mid | p_frac_a3_mid) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right", legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"))

# ggsave("PLOTS/SEASON_count_a3_frac_a3.png", panel_2x2, width = 14, height = 12, dpi = 500)
# cat("\nPlot saved to PLOTS/SEASON_count_a3_frac_a3.png\n")

theme_set(theme_get() + theme(text = element_text(family = "Arial")))
ggsave("PLOTS/SEASON_count_a3_frac_a3.pdf", panel_2x2, width = 14, height = 12, device = cairo_pdf)

cat("\nPlot saved to PLOTS/SEASON_count_a3_frac_a3.pdf\n")
