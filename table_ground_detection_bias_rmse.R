# ============================================================
# table_ground_detection_bias_rmse.R
# Ground-detection bias & RMSE by algorithm, for three sensitivity
# subsets: [0.90, 0.96], > 0.96, and >= 0.90 (all).
# Uses COUNT_complete.parquet only (weighting = count).
# ============================================================

source("00_functions.R")
source("01_load_data.R")

df <- raw$int

compute_algo_ground_stats <- function(data, algo) {
  cfg <- make_cfg("int", algo)

  base_filtered <- apply_base_filter(data, cfg, sens_lower = 0.90, sens_upper = NULL)

  sens_band   <- base_filtered %>% filter(.data[[cfg$sens_col]] <= 0.96)          # [0.90, 0.96]
  sens_gt_096 <- base_filtered %>% filter(.data[[cfg$sens_col]] >  0.96)          # > 0.96
  sens_gt_090 <- base_filtered                                                     # >= 0.90 (all)

  stats_for_subset <- function(subset_df, subset_label) {
    r <- subset_df[[cfg$elev_col]] - subset_df[["simu_true ground"]]
    tibble(
      algorithm = algo,
      subset    = subset_label,
      n         = sum(!is.na(r)),
      bias      = round(mean(r, na.rm = TRUE), 2),
      rmse      = round(sqrt(mean(r^2, na.rm = TRUE)), 2)
    )
  }

  bind_rows(
    stats_for_subset(sens_band,   "0.90 <= sens <= 0.96"),
    stats_for_subset(sens_gt_096, "sens > 0.96"),
    stats_for_subset(sens_gt_090, "sens >= 0.90")
  )
}

stats <- map_dfr(algo_list, ~ compute_algo_ground_stats(df, .x))

wide <- stats %>%
  filter(algorithm != "a4") %>%
  pivot_wider(
    id_cols     = algorithm,
    names_from  = subset,
    values_from = c(n, bias, rmse)
  ) %>%
  select(
    algorithm,
    `n_0.90 <= sens <= 0.96`, `bias_0.90 <= sens <= 0.96`, `rmse_0.90 <= sens <= 0.96`,
    `n_sens > 0.96`,          `bias_sens > 0.96`,          `rmse_sens > 0.96`,
    `n_sens >= 0.90`,         `bias_sens >= 0.90`,         `rmse_sens >= 0.90`
  )

cat("\n===== GROUND DETECTION - BIAS & RMSE BY SENSITIVITY SUBSET (PER-ALGORITHM FILTERS) =====\n\n")
print(wide, n = Inf)

cat("\n===== TOTAL N PER SUBSET (summed across algorithms) =====\n")
totals <- stats %>% group_by(subset) %>% summarise(total_n = sum(n), .groups = "drop")
print(totals)

write.csv(wide,   "TABLES/ground_detection_bias_rmse_by_algorithm.csv", row.names = FALSE)
write.csv(totals, "TABLES/ground_detection_n_totals.csv",               row.names = FALSE)
