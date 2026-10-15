# ============================================================
# fig_bias_combined.R
# Vertical faceted bias bar plots by Forest Type: count-default and
# combined count-default vs int-a3 panel.
# -> PLOTS/bias_count_default.png, PLOTS/bias_combined.png
# ============================================================

library(ggplot2)
library(patchwork)
library(cowplot)
source("00_functions.R")
source("01_load_data.R")
source("02_plot_helpers.R")

all_percentiles <- c(5, 10, 25, 50, 70, 90, 98)

cfg_int_a3    <- get_cfg("int",   "a3")
cfg_count_def <- get_cfg("count", "default")

INT   <- apply_base_filter(raw$int,   cfg_int_a3,    sens_lower = 0.90) %>% add_sens_group(cfg_int_a3)
COUNT <- apply_base_filter(raw$count, cfg_count_def, sens_lower = 0.90) %>% add_sens_group(cfg_count_def)

bias_INT_a3       <- compute_bias_by_forest_type(INT, cfg_int_a3, all_percentiles)
overall_INT_a3    <- compute_overall_bias_by_forest_type(INT, cfg_int_a3) %>% attach_labels(bias_INT_a3)

bias_count_def    <- compute_bias_by_forest_type(COUNT, cfg_count_def, all_percentiles)
overall_count_def <- compute_overall_bias_by_forest_type(COUNT, cfg_count_def) %>% attach_labels(bias_count_def)


colors_INT_a3    <- make_colors(bias_INT_a3)
colors_count_def <- make_colors(bias_count_def)

fixed_x_limits <- c(-1.5, 3.25)
fixed_x_breaks <- seq(-1.5, 3.25, by = 0.5)

p1 <- make_bias_panel(bias_INT_a3, overall_INT_a3, "Bias by Forest Type: int-a3",
                      colors_INT_a3, x_limits = fixed_x_limits, x_breaks = fixed_x_breaks)

p2 <- make_bias_panel(bias_count_def, overall_count_def, "Bias by Forest Type: count-default",
                      colors_count_def, x_limits = fixed_x_limits, x_breaks = fixed_x_breaks)
p_combined <- (p2 / p1) + plot_layout(heights = c(7, 7))

# ggsave("PLOTS/bias_count_default.png", p2,         width = 10, height = 7,  dpi = 500)
# ggsave("PLOTS/bias_combined.png",      p_combined, width = 12, height = 14, dpi = 500)
# cat("\nPlots saved to PLOTS/bias_count_default.png and PLOTS/bias_combined.png\n")

theme_set(theme_get() + theme(text = element_text(family = "Arial")))

#ggsave("PLOTS/bias_count_default.pdf", p2,         width = 10, height = 7,  device = cairo_pdf)
ggsave("PLOTS/bias_profile_int_a3_vs_count_default.pdf",      p_combined, width = 12, height = 14, device = cairo_pdf)

cat("\nPlots saved to PLOTS/bias_profile_int_a3_vs_count_default.pdf\n")

