# ============================================================
# 02_plot_helpers.R
# Shared ggplot builders for bias-by-forest-type bar plots
# (fig_bias_combined.R and fig_bias_comparison_count_frac_a3.R)
# ============================================================

library(ggplot2)

make_bias_panel <- function(bias_df, overall_bias_df, title_str, base_colors,
                            x_limits = NULL, x_breaks = NULL) {

  if (is.null(x_limits)) {
    x_limits <- range(bias_df$bias, na.rm = TRUE)
  }
  if (is.null(x_breaks)) {
    x_breaks <- seq(floor(x_limits[1] / 0.5) * 0.5, ceiling(x_limits[2] / 0.5) * 0.5, by = 0.5)
  }

  df_light <- bias_df %>% filter(sens_group == "0.90 <= sens <= 0.96")
  df_dark  <- bias_df %>% filter(sens_group == "sens > 0.96")
  df_all   <- bias_df %>% filter(sens_group == "sens >= 0.90 (all)")

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_col(data = df_light, aes(x = bias, y = percentile, fill = Forest_type_label),
             width = 0.55, alpha = 0.35, position = "identity") +
    geom_col(data = df_dark, aes(x = bias, y = percentile, fill = Forest_type_label),
             width = 0.35, alpha = 0.85, position = "identity") +
    geom_path(data = df_all, aes(x = bias, y = as.numeric(percentile), color = Forest_type_label),
              linewidth = 0.9, alpha = 0.9) +
    geom_point(data = df_all, aes(x = bias, y = percentile), shape = 16, size = 2.5, color = "red") +
    geom_text(data = overall_bias_df,
              aes(x = Inf, y = Inf, label = sprintf("Mean |Bias| RH 1-99:\n%.2f m", overall_bias),
                  group = Forest_type_label),
              hjust = 1.05, vjust = 1.8, size = 4.7, inherit.aes = FALSE,
              lineheight = 1.2, color = "grey20", fontface = "bold") +
    facet_wrap(~ Forest_type_label) +
    scale_fill_manual(values = base_colors, guide = "none") +
    scale_color_manual(values = base_colors, guide = "none") +
    
    scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks,
      labels = function(x) ifelse(x == round(x), as.character(as.integer(x)), "")
    ) +
    
    labs(title = title_str, x = "Bias (m)", y = "RH Percentile") +
    theme_bw(base_size = 13) +
    theme(
      plot.title         = element_text(face = "bold", size = 20, hjust = 0),
      axis.title         = element_text(face = "bold", size = 18),
      axis.text          = element_text(size = 14, color = "grey20"),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      strip.text         = element_text(face = "bold", size = 17),
      legend.position    = "none"
    )
}

# Dummy plot whose only purpose is to produce the shared legend for the
# sensitivity-group bar/line encoding used across bias-by-forest-type figures
build_sens_group_legend <- function() {
  legend_data <- tibble(
    x = c(1, 2, 3), y = c(1, 1, 1),
    group = factor(
      c("0.90 <= sens <= 0.96", "sens > 0.96", "sens >= 0.90 (all)"),
      levels = c("0.90 <= sens <= 0.96", "sens > 0.96", "sens >= 0.90 (all)")
    )
  )

  p <- ggplot(legend_data) +
    geom_col(data = legend_data %>% filter(group == "0.90 <= sens <= 0.96"),
             aes(x = x, y = y, fill = group), alpha = 0.35, width = 0.6) +
    geom_col(data = legend_data %>% filter(group == "sens > 0.96"),
             aes(x = x, y = y, fill = group), alpha = 0.85, width = 0.6) +
    geom_line(data = legend_data %>% filter(group == "sens >= 0.90 (all)"),
              aes(x = x, y = y, color = group), linewidth = 1) +
    geom_point(data = legend_data %>% filter(group == "sens >= 0.90 (all)"),
               aes(x = x, y = y), color = "red", shape = 16, size = 3) +
    scale_fill_manual(
      name = NULL,
      values = c("0.90 <= sens <= 0.96" = "grey50", "sens > 0.96" = "grey20"),
      labels = c("0.90 <= sens <= 0.96" = "0.90 <= sensitivity <= 0.96  (light bars)",
                 "sens > 0.96"          = "sensitivity > 0.96  (dark bars)")
    ) +
    scale_color_manual(
      name = NULL,
      values = c("sens >= 0.90 (all)" = "grey30"),
      labels = c("sens >= 0.90 (all)" = "sensitivity >= 0.90, all  (red line & points)")
    ) +
    guides(
      fill  = guide_legend(override.aes = list(alpha = c(0.35, 0.85)), order = 1),
      color = guide_legend(override.aes = list(color = "grey30", linewidth = 1, shape = 16, size = 3), order = 2)
    ) +
    theme_void(base_size = 15) +
    theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = element_text(size = 15, color = "grey20"),
      legend.key.size  = unit(1.1, "lines"),
      legend.spacing.x = unit(0.4, "cm")
    )

  cowplot::get_legend(p)
}
