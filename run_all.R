# ============================================================
# run_all.R
# Reproduces every figure and table in the manuscript.
# Run from inside the code/ folder, with the three .parquet files
# in the working directory (or edit weighting_files in 00_functions.R).
# ============================================================

source("00_functions.R")
source("01_load_data.R")

scripts <- c(
  "table_ground_detection_bias_rmse.R",
  "fig_MSD_count_default_vs_int_a3.R",
  "fig_bias_combined.R",
  "fig_bias_comparison_count_frac_a3.R",
  "fig_SEASON_count_default_int_a3.R",
  "fig_SEASON_count_a3_frac_a3.R"
)

for (s in scripts) {
  cat("\n", strrep("#", 60), "\n# Running: ", s, "\n", strrep("#", 60), "\n", sep = "")
  source(s)
}

cat("\nAll figures written to PLOTS/, all tables written to TABLES/\n")
