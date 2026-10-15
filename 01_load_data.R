# ============================================================
# 01_load_data.R
# Loads the three parquet files once. Source this after 00_functions.R.
# `raw` is a named list keyed by weighting scheme: raw$count, raw$frac, raw$int
# ============================================================


cat("Loading parquet files...\n")
raw <- list(
    count = read_parquet(weighting_files[["count"]]),
    frac  = read_parquet(weighting_files[["frac"]]),
    int   = read_parquet(weighting_files[["int"]]))



