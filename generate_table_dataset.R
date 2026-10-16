# ============================================================
# regenerate_table_dataset_distribution.R
# ============================================================

# --- 0. Session propre ---
rm(list = ls())
graphics.off()

# --- 1. Fonctions partagées ---
source("00_functions.R")

# --- 2. Charger COUNT frais (PAS _complete) ---
library(arrow)
COUNT <- read_parquet("COUNT.parquet")
cat("COUNT chargé :", nrow(COUNT), "lignes\n")

# --- 3. Filtre base (config GEDI default) ---
cfg <- make_cfg("count", "default")
df  <- apply_base_filter(COUNT, cfg)
cat("Après filtre base :", nrow(df), "lignes\n\n")

# --- 4. Vérifier les colonnes dont on a besoin ---
cat("Colonnes clés :\n")
cat("  Forest_type unique    :", paste(unique(df$Forest_type), collapse = ", "), "\n")
cat("  beam_type unique      :", paste(unique(df$beam_type),   collapse = ", "), "\n")
cat("  leaf_off_flag unique  :", paste(unique(df$leaf_off_flag), collapse = ", "), "\n")
cat("  selected_algorithm    :", paste(sort(unique(df$selected_algorithm)), collapse = ", "), "\n\n")

# --- 5. Construction du tableau ---
library(dplyr)

df_tab <- df %>%
  mutate(
    forest = factor(Forest_type,
                    levels = c("Broadleaf", "Coniferous", "Mixed")),
    beam = case_when(
      grepl("cover", beam_type, ignore.case = TRUE) ~ "Coverage",
      grepl("full",  beam_type, ignore.case = TRUE) ~ "Full power",
      TRUE ~ beam_type
    ),
    is_leaf_off = leaf_off_flag == 1,
    is_a1       = selected_algorithm == "1"
  )

# Table par (forest × beam)
tab <- df_tab %>%
  group_by(forest, beam) %>%
  summarise(
    N         = n(),
    leaf_off  = 100 * mean(is_leaf_off, na.rm = TRUE),
    CC        = mean(`simu_ALS cover`,    na.rm = TRUE),
    slope     = mean(`simu_ground slope`, na.rm = TRUE),
    a1_pct    = 100 * mean(is_a1, na.rm = TRUE),
    .groups   = "drop"
  )

# Ligne Total
total <- df_tab %>%
  summarise(
    forest   = factor("Total", levels = c("Broadleaf", "Coniferous", "Mixed", "Total")),
    beam     = "",
    N        = n(),
    leaf_off = 100 * mean(is_leaf_off, na.rm = TRUE),
    CC       = mean(`simu_ALS cover`,    na.rm = TRUE),
    slope    = mean(`simu_ground slope`, na.rm = TRUE),
    a1_pct   = 100 * mean(is_a1, na.rm = TRUE)
  )

tab_final <- bind_rows(tab, total)

# --- 6. Affichage arrondi ---
tab_print <- tab_final %>%
  mutate(
    N        = formatC(N, format = "d", big.mark = ","),
    leaf_off = sprintf("%.1f", leaf_off),
    CC       = sprintf("%.2f", CC),
    slope    = sprintf("%.0f", slope),
    a1_pct   = sprintf("%.0f", a1_pct)
  )

print(tab_print, n = Inf)