# Code and data for "Optimizing GEDI Simulator Configuration for European Temperate Forests"

## Reproducing the figures and tables

```r
setwd("code/")        # this folder
source("run_all.R")   # runs every script and regenerates PLOTS/ and console tables
```

Each `fig_*.R` / `table_*.R` script can also be run independently; each one
sources `00_functions.R` (shared filtering/statistics functions) and
`01_load_data.R` (loads the three parquet files once).

## Data description

`COUNT.parquet`, `FRAC.parquet`, and `INT.parquet`
all contain the **same set of GEDI L2A footprints**. The three files differ
only in the **waveform simulation weighting scheme** used to generate the
simulated GEDI waveforms (count-based, fraction-based, intensity-based
weighting). Within each file, columns further distinguish **seven ground-
finding algorithm settings**: the GEDI default algorithm, and six alternative
configurations `a1`–`a6`.

### Column naming convention

| Column pattern | Meaning |
|---|---|
| `simu_rhReal <p>` | Relative height at percentile `<p>`, simulated directly from the ALS point cloud. This is the reference ("ground truth") RH profile. |
| `simu_true ground` | Ground elevation derived directly from the ALS point cloud (reference/truth ground elevation). |
| `simu_ALS cover` | Canopy cover derived directly from the ALS point cloud. |
| `rh_<p>` / `rh_a{k}_<p>` | GEDI-simulated relative height at percentile `<p>`, for the default algorithm / algorithm `a{k}`, respectively. |
| `elev_lowestmode[_a{k}]` | Simulated ground elevation estimate from the GEDI waveform, for the default algorithm / algorithm `a{k}`. |
| `sensitivity[_a{k}]`, `quality_flag[_a{k}]` | GEDI sensitivity and quality flag for the corresponding algorithm. |

All `rh_*` / `elev_lowestmode*` columns are directly comparable to
`simu_rhReal <p>` / `simu_true ground` once the elevation offset
(`elev_lowestmode[...] - simu_true ground`) is applied — this offset
correction is what `compute_residual()` in `00_functions.R` does.

All `simu_*` columns have been derived using [gediMetric](https://bitbucket.org/StevenHancock/gedisimulator/src/master/).


### Filtering

All figures and tables use the same base filter, implemented once in
`apply_base_filter()` (see `00_functions.R`):

- `simu_ground slope >= 0`
- `simu_true ground >= 0`
- `0 <= simu_ALS cover < 1`
- `simu_pointDense >= 10`
- `simu_signal bottom >= 0`
- `5 < simu_rhReal 100 < 60`
- `degrade_flag == 0`
- `twin_correl > 0.7` mean correlation across the collocated subtrack
- `Forest_type_match == 0`
- `rh_*_0 < 0`, `rh_*_100 < 60` (algorithm-specific)
- `sensitivity[_a{k}] >= 0.90` (algorithm-specific; some figures further
  restrict to `> 0.96` or the `[0.90, 0.96]` band)
- `quality_flag[_a{k}] == 1` (algorithm-specific)
- `Forest_type` Corine Land Cover 2018 (revised in 2020)

**`Forest_type_match` filter:** `Forest_type_match == 0` restricts footprints
to areas where Corine Land Cover (CLC) has data coverage at the footprint
location. Setting `Forest_type_match == 1` instead would add approximately
1,000 additional footprints to the dataset, using a nearest-neighbor
CLC-based assignment of `Forest_type` for footprints falling outside direct
CLC coverage. The manuscript uses `Forest_type_match == 0` throughout for a
stricter, directly-labeled subset.



## Outputs

| Script | Output |
|---|---|
| `table_ground_detection_bias_rmse.R` | Ground-detection bias/RMSE table by algorithm and sensitivity subset (console + `TABLES/`) |
| `fig_MSD_count_default_vs_int_a3.R` | `PLOTS/MSD_count_default_vs_int_a3.png` + MSD summary tables (console + `TABLES/`) |
| `fig_bias_combined.R` | `PLOTS/bias_count_default.png`, `PLOTS/bias_combined.png` |
| `fig_bias_comparison_count_frac_a3.R` | `PLOTS/bias_count_a3.png`, `PLOTS/bias_frac_a3.png`, `PLOTS/bias_comparison_count_frac_a3.png` |
| `fig_SEASON_count_default_int_a3.R` | `PLOTS/SEASON_count_default_int_a3.png` + leaf-on/off metrics table |
| `fig_SEASON_count_a3_frac_a3.R` | `PLOTS/SEASON_count_a3_frac_a3.png` |


## License

The analysis scripts in this repository are released under the [MIT License](LICENSE).
The dataset is released under [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/).