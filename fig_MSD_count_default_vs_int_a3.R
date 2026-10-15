# ============================================================
# fig_MSD_count_default_vs_int_a3.R
# MSD decomposition (SB, SDSD, LCS) summary tables for all
# (weighting x algorithm) combinations, and the comparison figure
# count-default vs int-a3 -> PLOTS/MSD_count_default_vs_int_a3.png
# ============================================================

library(ggplot2)
source("00_functions.R")
source("01_load_data.R")

# ---- 1. MSD summary tables, three sensitivity subsets ----

run_all_msd <- function(sens_lower, sens_upper, label) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("SUBSET:", label, "\n")
  cat(strrep("=", 60), "\n\n")
  res <- map_dfr(all_algo_configs, function(cfg) {
    df <- apply_base_filter(raw[[cfg$weighting]], cfg, sens_lower, sens_upper)
    compute_msd_scenario(df, cfg)
  })
  res <- res %>% arrange(weighting, algorithm)
  print(res, n = Inf)
  invisible(res)
}

res_90   <- run_all_msd(sens_lower = 0.90, sens_upper = NULL, label = "sens >= 0.90")
res_96   <- run_all_msd(sens_lower = 0.96, sens_upper = NULL, label = "sens > 0.96")
res_band <- run_all_msd(sens_lower = 0.90, sens_upper = 0.96, label = "0.90 <= sens <= 0.96")

write.csv(res_90,   "TABLES/MSD_summary_sens_ge_090.csv", row.names = FALSE)
write.csv(res_96,   "TABLES/MSD_summary_sens_gt_096.csv", row.names = FALSE)
write.csv(res_band, "TABLES/MSD_summary_sens_band.csv",   row.names = FALSE)

# ---- 2. Comparison figure: count-default vs int-a3 (sens >= 0.90) ----

percentiles_plot <- c(98, 90, 70, 50, 25, 10, 5)

cfg_count_def <- get_cfg("count", "default")
cfg_int_a3    <- get_cfg("int",   "a3")

make_plot_data <- function(cfg) {
  df <- apply_base_filter(raw[[cfg$weighting]], cfg, sens_lower = 0.90, sens_upper = NULL)
  compute_msd_components(df, cfg, percentiles_plot) %>%
    mutate(scenario = paste0(cfg$weighting, "-", cfg$algo))
}

plot_data <- bind_rows(
  make_plot_data(cfg_count_def),
  make_plot_data(cfg_int_a3)
) %>%
  pivot_longer(c(SB, SDSD, LCS), names_to = "component", values_to = "value") %>%
  mutate(percentile = factor(percentile, levels = c(98, 90, 70, 50, 25, 10, 5)))

annot <- res_90 %>%
  filter((weighting == "count" & algorithm == "default") |
           (weighting == "int"  & algorithm == "a3")) %>%
  mutate(
    scenario       = paste0(weighting, "-", algorithm),
    label          = sprintf("Mean over RH 1-99:\nMSD  = %.2f\nSB   = %.2f\nSDSD = %.2f",
                             MSD_mean, SB_mean, SDSD_mean),
    scenario_label = paste0(scenario, " (N = ", formatC(n, format = "d", big.mark = ","), ")"),
    y = factor("10", levels = c(98, 90, 70, 50, 25, 10, 5))
  )

plot_data <- plot_data %>%
  mutate(scenario_label = case_when(
    scenario == "count-default" ~ annot$scenario_label[annot$scenario == "count-default"],
    scenario == "int-a3"        ~ annot$scenario_label[annot$scenario == "int-a3"]
  ))

max_val <- ceiling(max(plot_data$value, na.rm = TRUE))

p <- ggplot(plot_data, aes(x = value, y = percentile, fill = component)) +
  geom_col(position = "stack", width = 0.7) +
  facet_wrap(~scenario_label, ncol = 2) +
  scale_x_continuous(breaks = seq(0, max_val, by = 2)) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(
    values = c("SB" = "#d65f91", "SDSD" = "#e6d965", "LCS" = "#5fa8d6"),
    name = "MSD component"
  ) +
  labs(
    x = expression(MSD~(m^2)), y = "RH Percentile"
  ) +
  theme_bw(base_size = 13) +
  theme(
    legend.position    = "bottom",
    strip.text         = element_text(face = "bold", size = 18),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  ) +
  geom_label(
    data = annot,
    aes(x = Inf, y = y, label = label),
    hjust = 1.05, vjust = 0.5, size = 3.5,
    fontface = "bold", family = "mono",
    fill = "white", color = "black",
    label.padding = unit(0.5, "lines"),
    inherit.aes = FALSE
  )

#ggsave("PLOTS/MSD_count_default_vs_int_a3.png", p, width = 10, height = 5, dpi = 300)
#cat("\nPlot saved to PLOTS/MSD_count_default_vs_int_a3.png\n")


ggsave("PLOTS/MSD_count_default_vs_int_a3.pdf", p, width = 10, height = 5, device = cairo_pdf)
cat("\nPlot saved to PLOTS/MSD_count_default_vs_int_a3.pdf\n")


