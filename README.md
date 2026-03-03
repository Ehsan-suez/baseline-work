# Baseline Respiratory Infectious Diseases Forecasting Benchmark

This repository contains code, data, and results for evaluating how baseline forecasting model specifications affect probabilistic forecast performance across multiple infectious disease surveillance systems:

- **COVID-19 hospitalizations**
- **Influenza hospitalizations**
- **RSV hospitalizations**
- **Influenza-like illness (ILI%)**

The study systematically examines how baseline model design choices influence probabilistic forecast evaluation metrics such as the **Weighted Interval Score (WIS)** and **prediction interval coverage**.

---

## Repository Structure

```
baseline-work/
│
├── code/
│   ├── model_fitting/
│   │   ├── pull_cov_data_with_version.R
│   │   ├── run_covid_retro.R
│   │   ├── run_flu_retro.R
│   │   ├── run_ili_retro.R
│   │   ├── run_rsv_retro_as_of.R
│   │   ├── run_rsv_retro_final.R
│   │   └── run_rsv_retro_final.R seasonal_baseline.R
│   │
│   └── making_plots/
│       ├── figure_1.R
│       ├── figure_2.R
│       ├── figure_3.R
│       ├── figure_4.R
│       ├── table_1.R
│       ├── S1.R
│       ├── S2.R
│       ├── S3.R
│       ├── S4.R
│       ├── S5_6.R
│       ├── S7.R
│       ├── S8.R
│       └── S9.R
│
├── data/
│   ├── covid/
│   ├── Influenza/
│   ├── rsv/
│   ├── ili/
│   └── locations.csv
│
├── results/        # Generated forecasts and evaluation metrics
├── plots/          # Manuscript figures and tables
├── renv/           # Project-local package library
├── renv.lock       # Locked package versions
└── baseline-work.Rproj
```

---

## Reproducibility and Environment Control

This project uses **renv** for strict, project-local dependency management.

All forecast evaluation was conducted using:

- **R 4.3.0**
- **scoringutils 1.2.2**

Each model-fitting script includes a reproducibility guard that:

- Ensures `renv` is initialized  
- Activates the project-local environment  
- Verifies that `scoringutils` is exactly version 1.2.2  
- Stops execution if the environment is inconsistent  

This prevents accidental use of globally installed or updated package versions.

---

## Initial Setup (One Time Only)

After cloning the repository, from the project root run:

```r
install.packages("renv")   # if not already installed
renv::init()
renv::install("scoringutils@1.2.2")
renv::snapshot()
```

After this initial setup, scripts can be run directly.

If packages are missing or the environment needs to be restored:

```r
renv::restore()
```

---

## Execution Workflow

### Step 1 — Pull Archived COVID Data

To regenerate archived COVID-19 data by issue date:

```
code/model_fitting/pull_cov_data_with_version.R
```

This script retrieves historical forecast hub data required for retrospective simulation.

---

### Step 2 — Fit Models and Generate Evaluation Outputs

Run the scripts in:

```
code/model_fitting/
```

Recommended order:

1. `run_covid_retro.R`  
2. `run_flu_retro.R`  
3. `run_rsv_retro_as_of.R`  
4. `run_rsv_retro_final.R`  
5. `run_ili_retro_redo.R`
6. `seasonal_baseline.R`

Each script:

- Fits the baseline model  
- Generates probabilistic forecasts  
- Computes evaluation metrics (e.g., WIS, interval coverage)  
- Automatically creates required output directories if missing  
- Saves outputs to:

```
results/<disease>/forecasts/
results/<disease>/scores/
```

Outputs are versioned by forecast issue date.

---

### Step 3 — Generate Figures and Tables (Sequential Order)

After model fitting is complete, run the plotting scripts in the following order:

```
code/making_plots/
```

Recommended order:

1. `figure_1.R`  
2. `figure_2.R`  
3. `figure_3.R`  
4. `figure_4.R`  
5. `table_1.R`  
6. `S1.R`  
7. `S2.R`  
8. `S3.R`  
9. `S4.R`  
10. `S5_6.R`  
11. `S7.R`  
12. `S8.R`  
13. `S9.R`  

Each script:

- Reads processed outputs from `results/`  
- Generates a specific manuscript figure or table  
- Saves output files to `plots/paper/`  

Script names correspond directly to manuscript numbering.

---

## Data Notes

- COVID-19 and Influenza analyses use archived real-time hub data.  
- RSV analyses include both real-time (“as-of”) and finalized data to assess the impact of data revisions.  
- ILI analyses use finalized percent wILI values.  

The distinction between as-of and finalized data is particularly important for RSV due to substantial backfill and revision.

---

## Author

Ehsan Suez
PhD Candidate, University of Georgia
ehsan.suez@uga.edu
