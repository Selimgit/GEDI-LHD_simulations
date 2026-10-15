# ============================================================
# 03_ribbon_plot_helper.R
# Shared ggplot builder for the Leaf On vs Leaf Off ribbon plots
# (fig_SEASON_count_default_int_a3.R and fig_SEASON_count_a3_frac_a3.R)
# ============================================================

library(ggplot2)

leaf_colors <- c("Leaf On" = "#2ca25f", "Leaf Off" = "#8c510a")

plot_ribbon <- function(df, overall_df, dataset_title, x_limits,
                        gradient_limits = c(0, NA), all_percentiles) {

  bias_wide <- df %>%
    select(leaf_status, percentile, bias) %>%
    pivot_wider(names_from = leaf_status, values_from = bias) %>%
    arrange(percentile) %>%
    mutate(percentile_num = as.numeric(as.character(percentile)))

  ribbon_segments <- map_dfr(seq_len(nrow(bias_wide) - 1), function(i) {
    p1 <- bias_wide$percentile_num[i]
    p2 <- bias_wide$percentile_num[i + 1]
    steps <- seq(p1, p2, length.out = 30)

    leaf_on_interp  <- approx(c(p1, p2), c(bias_wide$`Leaf On`[i],  bias_wide$`Leaf On`[i + 1]),  xout = steps)$y
    leaf_off_interp <- approx(c(p1, p2), c(bias_wide$`Leaf Off`[i], bias_wide$`Leaf Off`[i + 1]), xout = steps)$y

    tibble(y = steps,
           xmin = pmin(leaf_on_interp, leaf_off_interp),
           xmax = pmax(leaf_on_interp, leaf_off_interp),
           abs_diff = abs(leaf_on_interp - leaf_off_interp))
  })

  annotation_text <- paste0(
    "Mean |Bias| (RH 1-99)\n",
    sprintf("Leaf On :  %.2f m\n", overall_df$overall_bias[overall_df$leaf_status == "Leaf On"]),
    sprintf("Leaf Off : %.2f m",   overall_df$overall_bias[overall_df$leaf_status == "Leaf Off"])
  )

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    geom_rect(data = ribbon_segments,
              aes(xmin = xmin, xmax = xmax, ymin = y - 0.5, ymax = y + 0.5, fill = abs_diff),
              alpha = 0.6) +
    scale_fill_gradientn(
      colours = c("white", "#fee090", "#fdae61", "#e6550d"),
      name = "|\u0394 Bias|\n(m)", limits = gradient_limits,
      guide = guide_colorbar(title.position = "top", barwidth = unit(0.5, "cm"), barheight = unit(3, "cm"),
                             title.theme = element_text(size = 15, face = "bold"),
                             label.theme = element_text(size = 13))
    ) +
    geom_path(data = df, aes(x = bias, y = as.numeric(as.character(percentile)),
                             color = leaf_status, group = leaf_status),
              linewidth = 1.1, alpha = 0.9) +
    geom_point(data = df, aes(x = bias, y = as.numeric(as.character(percentile))),
               size = 2.5, shape = 16, color = "red") +
    geom_label(data = data.frame(x = Inf, y = 25, label = annotation_text),
               aes(x = x, y = y, label = label),
               hjust = 1, vjust = 0.5, size = 4.7, fontface = "bold", family = "mono",
               fill = "white", color = "black", label.padding = unit(0.5, "lines"),
               inherit.aes = FALSE) +
    scale_color_manual(values = leaf_colors, name = NULL) +
    scale_x_continuous(
      limits = x_limits,
      breaks = seq(x_limits[1], x_limits[2], by = 0.25),
      labels = function(x) ifelse(x == round(x / 0.5) * 0.5, formatC(x, format = "f", digits = 1), "")
    ) +    scale_y_continuous(breaks = all_percentiles) +
    labs(title = dataset_title, x = "Bias (m)", y = "RH Percentile") +
    theme_bw(base_size = 13) +
    theme(
      plot.title         = element_text(face = "bold", size = 20, hjust = 0),
      axis.title         = element_text(face = "bold", size = 18),
      axis.text          = element_text(size = 14, color = "grey20"),
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 14),
      panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "right",
      legend.title       = element_text(size = 15, face = "bold"),
      legend.text        = element_text(size = 15),
      legend.background  = element_blank(),
      legend.key         = element_blank()
    )
}

# Prepares bias/overall-bias data for one (weighting, algo, sensitivity band) combo
prepare_leaf_status_results <- function(df_raw, cfg, sens_params, all_percentiles) {
  filtered <- apply_base_filter(df_raw, cfg,
                                sens_lower = sens_params$min,
                                sens_upper = if (is.infinite(sens_params$max)) NULL else sens_params$max)
  # sens_params encodes exclusive lower / inclusive upper bounds used by the
  # SEASON figures (high: >0.96, mid: 0.90 < sens <= 0.96); apply_base_filter's
  # sens_lower is inclusive, so tighten the lower bound here when needed
  if (isTRUE(sens_params$excl_lower)) {
    filtered <- filtered %>% filter(.data[[cfg$sens_col]] > sens_params$min)
  }
  bias_df <- compute_bias_by_leaf_status(filtered, cfg, all_percentiles)
  overall <- compute_overall_bias_by_leaf_status(filtered, cfg)
  n_shots <- compute_n_shots_leaf_status(filtered)
  list(bias = bias_df, overall = overall, n = n_shots)
}

sens_bands <- list(
  high = list(min = 0.96, max = Inf,  excl_lower = TRUE),   # sens > 0.96
  mid  = list(min = 0.90, max = 0.96, excl_lower = TRUE)    # 0.90 < sens <= 0.96
)
