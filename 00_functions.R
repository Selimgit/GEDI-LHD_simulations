# ============================================================
# 00_functions.R
# Shared configuration, filtering, and statistics functions used by
# every fig_*.R / table_*.R script. See README.md for the data and
# filter description.
# ============================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(scales)
})

# ------------------------------------------------------------
# 1. COLUMN NAME BUILDER
#    Returns the algorithm-specific column names for a given
#    weighting scheme (count/frac/int) and algorithm (default, a1..a6)
# ------------------------------------------------------------

algo_list      <- c("default", paste0("a", 1:6))
weighting_files <- c(count = "COUNT.parquet",
                     frac  = "FRAC.parquet",
                     int   = "INT.parquet")

make_cfg <- function(weighting, algo) {
  if (algo == "default") {
    list(
      weighting = weighting, algo = algo,
      file      = weighting_files[[weighting]],
      rh_prefix = "rh_",
      elev_col  = "elev_lowestmode",
      sens_col  = "sensitivity",
      qual_col  = "quality_flag",
      rh0_col   = "rh_0",
      rh100_col = "rh_100"
    )
  } else {
    list(
      weighting = weighting, algo = algo,
      file      = weighting_files[[weighting]],
      rh_prefix = paste0("rh_", algo, "_"),
      elev_col  = paste0("elev_lowestmode_", algo),
      sens_col  = paste0("sensitivity_", algo),
      qual_col  = paste0("quality_flag_", algo),
      rh0_col   = paste0("rh_", algo, "_0"),
      rh100_col = paste0("rh_", algo, "_100")
    )
  }
}

# All (weighting x algorithm) configs, used by MSD / table scripts
all_algo_configs <- c(
  map(algo_list, ~ make_cfg("count", .x)),
  map(algo_list, ~ make_cfg("frac",  .x)),
  map(algo_list, ~ make_cfg("int",   .x))
)

get_cfg <- function(weighting, algo) {
  all_algo_configs[[which(sapply(all_algo_configs,
                                 function(x) x$weighting == weighting & x$algo == algo))]]
}


# ------------------------------------------------------------
# 2. DUPLICATE SIMULATION HANDLING
#    Due to spatial overlap between adjacent LiDAR HD tiles,
#    ~10% of GEDI shots were simulated twice. The published
#    dataset keeps both simulations (see README.md). These
#    helpers let users identify or remove them.
#
#    Key: the 101 RH values (rh_0..rh_100) of the *original*
#    GEDI L2A product, which are identical across the two
#    simulations of the same shot.
# ------------------------------------------------------------

rh_key_cols <- paste0("rh_", 0:100)

#' Return only the rows that are duplicated on the RH key.
find_duplicate_sims <- function(df, key_cols = rh_key_cols) {
  df %>%
    group_by(across(all_of(key_cols))) %>%
    filter(n() > 1) %>%
    ungroup()
}

#' Collapse duplicate simulations to one row per GEDI shot
#' (keeps the first occurrence).
deduplicate_sims <- function(df, key_cols = rh_key_cols) {
  df %>%
    distinct(across(all_of(key_cols)), .keep_all = TRUE)
}

#' Quick report: how many rows / groups are duplicated.
report_duplicate_sims <- function(df, label = "data", key_cols = rh_key_cols) {
  n_total <- nrow(df)
  n_dup   <- df %>%
    group_by(across(all_of(key_cols))) %>%
    tally() %>%
    filter(n > 1) %>%
    nrow()
  cat(sprintf("%-8s : %6d rows | %5d duplicated groups | %5.1f%%\n",
              label, n_total, n_dup,
              if (n_total > 0) 100 * n_dup / n_total else 0))
  invisible(n_dup)
}


# ------------------------------------------------------------
# 3. HARMONIZED BASE FILTER
#    Confirmed correct thresholds:
#      - simu_ALS cover >= 0   (inclusive lower bound)
#      - sensitivity[_a{k}]    >= 0.90 (inclusive lower bound)
#    sens_upper: NULL = no upper bound; otherwise inclusive upper bound
#    forest_type_match: 0 (default, CLC-covered footprints only) or 1
#      (adds ~1000 nearest-neighbor-assigned footprints, see README.md)
# ------------------------------------------------------------

apply_base_filter <- function(df, cfg,
                              sens_lower = 0.90, sens_upper = NULL,
                              forest_type_match = 0, deduplicate = FALSE) {
  df <- df %>% filter(
    `simu_ground slope`  >= 0,
    `simu_true ground`   >= 0,
    `simu_ALS cover`     > 0,
    `simu_ALS cover`     <  1,
    `simu_pointDense`    >= 10,
    `simu_signal bottom` >= 0,
    `simu_rhReal 100`    >  5,
    `simu_rhReal 100`    <  60,
    `degrade_flag`         == 0,
    `twin_correl`    >  0.7,
    `Forest_type_match`  == forest_type_match,
    .data[[cfg$rh0_col]]   <  0,
    .data[[cfg$rh100_col]] <  60,
    .data[[cfg$sens_col]]  >= sens_lower,
    .data[[cfg$qual_col]]  == 1,
    !is.na(Forest_type)
  )
  if (!is.null(sens_upper)) {
    df <- df %>% filter(.data[[cfg$sens_col]] <= sens_upper)
  }
  if (deduplicate) {
    df <- deduplicate_sims(df)
  }
  df
}







# Tags rows with a sensitivity subgroup label (harmonized >= 0.90 lower bound)
add_sens_group <- function(df, cfg) {
  df %>% mutate(
    sens_group = case_when(
      .data[[cfg$sens_col]] > 0.96  ~ "sens > 0.96",
      .data[[cfg$sens_col]] >= 0.90 ~ "0.90 <= sens <= 0.96",
      TRUE ~ NA_character_
    )
  )
}

# ------------------------------------------------------------
# 3. RESIDUAL
#    residual = simulated_RH_from_ALS - elevation-corrected GEDI RH
# ------------------------------------------------------------

compute_residual <- function(df, cfg, p) {
  simu_col <- paste0("simu_rhReal ", p)
  rh_col   <- paste0(cfg$rh_prefix, p)
  offset   <- df[[cfg$elev_col]] - df[["simu_true ground"]]
  df[[simu_col]] - (df[[rh_col]] + offset)
}

# ------------------------------------------------------------
# 4. BIAS BY FOREST TYPE (bar plots: bias_combined, bias_comparison_*)
# ------------------------------------------------------------

compute_bias_by_forest_type <- function(df, cfg, percentiles) {
  n_per_type <- df %>%
    filter(!is.na(Forest_type)) %>%
    count(Forest_type) %>%
    mutate(label = paste0(Forest_type, " (N=", scales::comma(n), ")"))

  bias_sub <- map_dfr(percentiles, function(p) {
    tibble(percentile = p, residual = compute_residual(df, cfg, p),
           Forest_type = df$Forest_type, sens_group = df$sens_group)
  }) %>%
    filter(!is.na(residual), !is.na(Forest_type), !is.na(sens_group)) %>%
    group_by(Forest_type, percentile, sens_group) %>%
    summarise(bias = mean(residual, na.rm = TRUE), .groups = "drop")

  bias_all <- map_dfr(percentiles, function(p) {
    tibble(percentile = p, residual = compute_residual(df, cfg, p),
           Forest_type = df$Forest_type)
  }) %>%
    filter(!is.na(residual), !is.na(Forest_type)) %>%
    group_by(Forest_type, percentile) %>%
    summarise(bias = mean(residual, na.rm = TRUE), .groups = "drop") %>%
    mutate(sens_group = "sens >= 0.90 (all)")

  bind_rows(bias_sub, bias_all) %>%
    mutate(
      percentile  = factor(percentile, levels = percentiles),
      Forest_type = factor(Forest_type, levels = sort(unique(Forest_type))),
      sens_group  = factor(sens_group, levels = c("0.90 <= sens <= 0.96",
                                                   "sens > 0.96",
                                                   "sens >= 0.90 (all)"))
    ) %>%
    left_join(n_per_type %>% select(Forest_type, label), by = "Forest_type") %>%
    mutate(Forest_type_label = factor(label, levels = n_per_type$label))
}

compute_overall_bias_by_forest_type <- function(df, cfg) {
  map_dfr(1:99, function(p) {
    tibble(percentile = p, residual = compute_residual(df, cfg, p),
           Forest_type = df$Forest_type)
  }) %>%
    filter(!is.na(residual), !is.na(Forest_type)) %>%
    group_by(Forest_type, percentile) %>%
    summarise(bias_p = mean(residual, na.rm = TRUE), .groups = "drop") %>%
    group_by(Forest_type) %>%
    summarise(overall_bias = mean(abs(bias_p), na.rm = TRUE), .groups = "drop")
}

attach_labels <- function(overall_df, bias_df) {
  label_map <- bias_df %>% select(Forest_type, Forest_type_label) %>% distinct()
  overall_df %>%
    left_join(label_map, by = "Forest_type") %>%
    mutate(Forest_type_label = factor(Forest_type_label, levels = levels(bias_df$Forest_type_label)))
}

make_colors <- function(bias_df) {
  labels <- levels(bias_df$Forest_type_label)
  setNames(scales::brewer_pal(palette = "Dark2")(min(length(labels), 8)), labels)
}

# ------------------------------------------------------------
# 5. BIAS BY LEAF STATUS (SEASON figures; Broadleaf + Mixed only)
# ------------------------------------------------------------

compute_bias_by_leaf_status <- function(df, cfg, percentiles) {
  map_dfr(percentiles, function(p) {
    tibble(percentile = p, residual = compute_residual(df, cfg, p),
           Forest_type = df$Forest_type, leaf_off_flag = df$leaf_off_flag)
  }) %>%
    filter(!is.na(residual), Forest_type %in% c("Broadleaf", "Mixed"),
           leaf_off_flag %in% c(0, 1)) %>%
    mutate(leaf_status = if_else(leaf_off_flag == 1, "Leaf Off", "Leaf On")) %>%
    group_by(leaf_status, percentile) %>%
    summarise(bias = mean(residual, na.rm = TRUE),
              se   = sd(residual, na.rm = TRUE) / sqrt(n()), .groups = "drop") %>%
    mutate(leaf_status = factor(leaf_status, levels = c("Leaf On", "Leaf Off"))) %>%
    arrange(leaf_status, percentile)
}

compute_overall_bias_by_leaf_status <- function(df, cfg) {
  map_dfr(1:99, function(p) {
    tibble(percentile = p, residual = compute_residual(df, cfg, p),
           Forest_type = df$Forest_type, leaf_off_flag = df$leaf_off_flag)
  }) %>%
    filter(!is.na(residual), Forest_type %in% c("Broadleaf", "Mixed"),
           leaf_off_flag %in% c(0, 1)) %>%
    mutate(leaf_status = if_else(leaf_off_flag == 1, "Leaf Off", "Leaf On")) %>%
    group_by(leaf_status, percentile) %>%
    summarise(bias_p = mean(residual, na.rm = TRUE), .groups = "drop") %>%
    group_by(leaf_status) %>%
    summarise(overall_bias = mean(abs(bias_p), na.rm = TRUE), .groups = "drop") %>%
    mutate(leaf_status = factor(leaf_status, levels = c("Leaf On", "Leaf Off")))
}

compute_n_shots_leaf_status <- function(df) {
  df %>%
    filter(Forest_type %in% c("Broadleaf", "Mixed"), leaf_off_flag %in% c(0, 1)) %>%
    mutate(leaf_status = if_else(leaf_off_flag == 1, "Leaf Off", "Leaf On")) %>%
    group_by(leaf_status) %>%
    summarise(n_shots = n(), .groups = "drop") %>%
    mutate(leaf_status = factor(leaf_status, levels = c("Leaf On", "Leaf Off")))
}

# ------------------------------------------------------------
# 6. MSD DECOMPOSITION (SB, SDSD, LCS)
# ------------------------------------------------------------

compute_msd_components <- function(df, cfg, percentiles) {
  map_dfr(percentiles, function(p) {
    simu_col <- paste0("simu_rhReal ", p)
    rh_col   <- paste0(cfg$rh_prefix, p)
    if (!rh_col %in% names(df) || !simu_col %in% names(df)) return(NULL)

    offset <- df[[cfg$elev_col]] - df[["simu_true ground"]]
    y <- df[[rh_col]] + offset
    x <- df[[simu_col]]

    xbar <- mean(x, na.rm = TRUE); ybar <- mean(y, na.rm = TRUE)
    sx   <- sd(x, na.rm = TRUE);   sy   <- sd(y, na.rm = TRUE)
    r    <- cor(x, y, use = "complete.obs")

    tibble(percentile = p,
           SB   = (xbar - ybar)^2,
           SDSD = (sx - sy)^2,
           LCS  = 2 * sx * sy * (1 - r))
  }) %>%
    mutate(MSD = SB + SDSD + LCS)
}

compute_msd_scenario <- function(df, cfg, percentiles = 1:99) {
  msd_rows <- compute_msd_components(df, cfg, percentiles)
  msd_rows %>%
    summarise(MSD_mean  = round(mean(MSD,  na.rm = TRUE), 2),
              SB_mean   = round(mean(SB,   na.rm = TRUE), 2),
              SDSD_mean = round(mean(SDSD, na.rm = TRUE), 2),
              LCS_mean  = round(mean(LCS,  na.rm = TRUE), 2)) %>%
    mutate(weighting = cfg$weighting, algorithm = cfg$algo, n = nrow(df), .before = 1)
}

# ------------------------------------------------------------
# 7. BIAS / RMSE / R2 TABLE BY PERCENTILE x SENS GROUP x LEAF STATUS
# ------------------------------------------------------------

compute_metrics_for_subset <- function(df, cfg, p, leaf_status_value) {
  simu_col <- paste0("simu_rhReal ", p)
  rh_col   <- paste0(cfg$rh_prefix, p)

  valid <- !is.na(df$Forest_type) &
    df$Forest_type %in% c("Broadleaf", "Mixed") &
    df$leaf_off_flag == leaf_status_value

  if (sum(valid) == 0) return(tibble(bias = NA_real_, rmse = NA_real_, r2 = NA_real_, n = 0))

  observed  <- df[[rh_col]][valid] + (df[[cfg$elev_col]][valid] - df[["simu_true ground"]][valid])
  simulated <- df[[simu_col]][valid]
  residual  <- simulated - observed

  ss_res <- sum((simulated - observed)^2, na.rm = TRUE)
  ss_tot <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)

  tibble(bias = mean(residual, na.rm = TRUE),
         rmse = sqrt(mean(residual^2, na.rm = TRUE)),
         r2   = 1 - ss_res / ss_tot,
         n    = sum(valid))
}

# ------------------------------------------------------------
# 8. OUTPUT DIRECTORIES
# ------------------------------------------------------------

dir.create("PLOTS",  showWarnings = FALSE, recursive = TRUE)
dir.create("TABLES", showWarnings = FALSE, recursive = TRUE)
