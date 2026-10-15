# ============================================================
# fig_bias_comparison_count_frac_a3.R
# Vertical faceted bias bar plots by Forest Type: count-a3 vs frac-a3.
# -> PLOTS/bias_count_a3.png, PLOTS/bias_frac_a3.png,
#    PLOTS/bias_comparison_count_frac_a3.png
# ============================================================

library(ggplot2)
library(patchwork)
library(cowplot)
source("00_functions.R")
source("01_load_data.R")
source("02_plot_helpers.R")

all_percentiles <- c(5, 10, 25, 50, 70, 90, 98)

cfg_count_a3 <- get_cfg("count", "a3")
cfg_frac_a3  <- get_cfg("frac",  "a3")

COUNT <- apply_base_filter(raw$count, cfg_count_a3, sens_lower = 0.90) %>% add_sens_group(cfg_count_a3)
FRAC  <- apply_base_filter(raw$frac,  cfg_frac_a3,  sens_lower = 0.90) %>% add_sens_group(cfg_frac_a3)

bias_count_a3    <- compute_bias_by_forest_type(COUNT, cfg_count_a3, all_percentiles)
overall_count_a3 <- compute_overall_bias_by_forest_type(COUNT, cfg_count_a3) %>% attach_labels(bias_count_a3)

bias_frac_a3    <- compute_bias_by_forest_type(FRAC, cfg_frac_a3, all_percentiles)
overall_frac_a3 <- compute_overall_bias_by_forest_type(FRAC, cfg_frac_a3) %>% attach_labels(bias_frac_a3)


colors_count_a3 <- make_colors(bias_count_a3)
colors_frac_a3  <- make_colors(bias_frac_a3)

fixed_x_limits <- c(-1.5, 3.25)
fixed_x_breaks <- seq(-1.5, 3.25, by = 0.5)

p_count_a3 <- make_bias_panel(bias_count_a3, overall_count_a3, "Bias by Forest Type: count-a3",
                              colors_count_a3, x_limits = fixed_x_limits, x_breaks = fixed_x_breaks)

p_frac_a3  <- make_bias_panel(bias_frac_a3, overall_frac_a3, "Bias by Forest Type: frac-a3",
                              colors_frac_a3, x_limits = fixed_x_limits, x_breaks = fixed_x_breaks)

p_combined <- (p_count_a3 / p_frac_a3) + plot_layout(heights = c(7, 7)) 

p_final <- plot_grid(p_combined, ncol = 1, rel_heights = c(10, 1))

# ggsave("PLOTS/bias_count_a3.png", p_count_a3, width = 10, height = 7,  dpi = 500)
# ggsave("PLOTS/bias_frac_a3.png",  p_frac_a3,  width = 10, height = 7,  dpi = 500)
# ggsave("PLOTS/bias_comparison_count_frac_a3.png", p_final, width = 12, height = 14, dpi = 500)
# cat("\nPlots saved to PLOTS/bias_count_a3.png, PLOTS/bias_frac_a3.png, PLOTS/bias_comparison_count_frac_a3.png\n")


theme_set(theme_get() + theme(text = element_text(family = "Arial")))

# ggsave("PLOTS/bias_count_a3.pdf", p_count_a3, width = 10, height = 7)
# ggsave("PLOTS/bias_frac_a3.pdf",  p_frac_a3,  width = 10, height = 7)
ggsave("PLOTS/bias_profile_count_a3_vs_frac_a3.pdf", p_final, width = 12, height = 14)

cat("\nPlots saved to PLOTS/bias_profile_count_a3_vs_frac_a3.pdf\n")



