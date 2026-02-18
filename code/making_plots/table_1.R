library(dplyr)
library(tidyr)

avg_clean <- avg_all %>%
  mutate(
    data_num = as.numeric(data_num),
    transformed = variation %in% c("Drift (transformed)", "Flatline (transformed)"),
    base_variation = case_when(
      variation %in% c("Flatline", "Flatline (transformed)") ~ "Flatline",
      variation %in% c("Drift", "Drift (transformed)") ~ "Drift",
      TRUE ~ NA_character_
    )
  )
best_flatline <- avg_clean %>%
  filter(base_variation == "Flatline") %>%
  group_by(dataset) %>%
  slice_min(mean_wis, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    dataset,
    best_flatline_variation = variation,
    transformation = ifelse(transformed, "Yes", "No"),
    best_data_num = data_num,
    best_flatline_wis = mean_wis
  )

best_flatline

library(dplyr)

best_config <- tibble::tribble(
  ~dataset,     ~transformation, ~min_w, ~max_w,
  "COVID-19",   "Either",          7,     14,
  "wILI",       "Yes",             6,     12,
  "Influenza",  "Yes",             5,     10,
  "RSV",        "Yes",            40,     50
)

avg2 <- avg_all %>%
  mutate(window = data,
         window_num = suppressWarnings(as.numeric(window)),
         is_all = window == "all",
         transformed = variation %in% c("Flatline (transformed)", "Drift (transformed)"),
         base = case_when(
           variation %in% c("Flatline", "Flatline (transformed)") ~ "Flatline",
           variation %in% c("Drift", "Drift (transformed)")       ~ "Drift"))
library(dplyr)
    
# =========================
# COVID-19 (7–14), transformed
# =========================
w_lo <- 7
w_hi <- 14
    
flat_all <- avg2 %>%
      filter(dataset == "COVID-19", base == "Flatline", is_all, transformed == TRUE) %>%
      pull(mean_wis) %>% first()
    
flat_lo <- avg2 %>%
      filter(dataset == "COVID-19", base == "Flatline", !is_all, transformed == TRUE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
flat_hi <- avg2 %>%
      filter(dataset == "COVID-19", base == "Flatline", !is_all, transformed == TRUE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
drift_lo <- avg2 %>%
      filter(dataset == "COVID-19", base == "Drift", !is_all, transformed == TRUE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
drift_hi <- avg2 %>%
      filter(dataset == "COVID-19", base == "Drift", !is_all, transformed == TRUE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
flat_nontr_lo <- avg2 %>%
      filter(dataset == "COVID-19", base == "Flatline", !is_all, transformed == FALSE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
flat_nontr_hi <- avg2 %>%
      filter(dataset == "COVID-19", base == "Flatline", !is_all, transformed == FALSE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
imp_all_lo   <- (flat_all - flat_lo) / flat_all * 100
imp_all_hi   <- (flat_all - flat_hi) / flat_all * 100
imp_drift_lo <- (drift_lo - flat_lo) / drift_lo * 100
imp_drift_hi <- (drift_hi - flat_hi) / drift_hi * 100
imp_nontr_lo <- (flat_nontr_lo - flat_lo) / flat_nontr_lo * 100
imp_nontr_hi <- (flat_nontr_hi - flat_hi) / flat_nontr_hi * 100
    
covid_summary <- tibble::tibble(
      Dataset = "COVID-19",
      Variant = "Flatline",
      Transformation = "Yes",
      `Data Fitting (recent)` = "7-14",
      `Improvement over All (%)` = paste0(round(imp_all_lo, 2), " – ", round(imp_all_hi, 2)),
      `Improvement over drift variant (%)` = paste0(round(imp_drift_lo, 2), " – ", round(imp_drift_hi, 2)),
      `Improvement over non-transformed data (%)` = paste0(round(imp_nontr_lo, 2), " – ", round(imp_nontr_hi, 2))
    )
    
    
# =========================
# wILI (6–12), transformed
# =========================
w_lo <- 6
w_hi <- 12
    
flat_all <- avg2 %>%
      filter(dataset == "wILI", base == "Flatline", is_all, transformed == TRUE) %>%
      pull(mean_wis) %>% first()
    
flat_lo <- avg2 %>%
      filter(dataset == "wILI", base == "Flatline", !is_all, transformed == TRUE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
flat_hi <- avg2 %>%
      filter(dataset == "wILI", base == "Flatline", !is_all, transformed == TRUE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
drift_lo <- avg2 %>%
      filter(dataset == "wILI", base == "Drift", !is_all, transformed == TRUE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
drift_hi <- avg2 %>%
      filter(dataset == "wILI", base == "Drift", !is_all, transformed == TRUE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
flat_nontr_lo <- avg2 %>%
      filter(dataset == "wILI", base == "Flatline", !is_all, transformed == FALSE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
flat_nontr_hi <- avg2 %>%
      filter(dataset == "wILI", base == "Flatline", !is_all, transformed == FALSE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
imp_all_lo   <- (flat_all - flat_lo) / flat_all * 100
imp_all_hi   <- (flat_all - flat_hi) / flat_all * 100
imp_drift_lo <- (drift_lo - flat_lo) / drift_lo * 100
imp_drift_hi <- (drift_hi - flat_hi) / drift_hi * 100
imp_nontr_lo <- (flat_nontr_lo - flat_lo) / flat_nontr_lo * 100
imp_nontr_hi <- (flat_nontr_hi - flat_hi) / flat_nontr_hi * 100
    
wili_summary <- tibble::tibble(
      Dataset = "wILI",
      Variant = "Flatline",
      Transformation = "Yes",
      `Data Fitting (recent)` = "6-12",
      `Improvement over All (%)` = paste0(round(imp_all_lo, 2), " – ", round(imp_all_hi, 2)),
      `Improvement over drift variant (%)` = paste0(round(imp_drift_lo, 2), " – ", round(imp_drift_hi, 2)),
      `Improvement over non-transformed data (%)` = paste0(round(imp_nontr_lo, 2), " – ", round(imp_nontr_hi, 2))
    )
    
    
# =========================
# Influenza (5–10), transformed
# =========================
w_lo <- 5
w_hi <- 10
    
flat_all <- avg2 %>%
      filter(dataset == "Influenza", base == "Flatline", is_all, transformed == TRUE) %>%
      pull(mean_wis) %>% first()
    
flat_lo <- avg2 %>%
      filter(dataset == "Influenza", base == "Flatline", !is_all, transformed == TRUE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
flat_hi <- avg2 %>%
      filter(dataset == "Influenza", base == "Flatline", !is_all, transformed == TRUE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
drift_lo <- avg2 %>%
      filter(dataset == "Influenza", base == "Drift", !is_all, transformed == TRUE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
drift_hi <- avg2 %>%
      filter(dataset == "Influenza", base == "Drift", !is_all, transformed == TRUE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
flat_nontr_lo <- avg2 %>%
      filter(dataset == "Influenza", base == "Flatline", !is_all, transformed == FALSE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
flat_nontr_hi <- avg2 %>%
      filter(dataset == "Influenza", base == "Flatline", !is_all, transformed == FALSE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
imp_all_lo   <- (flat_all - flat_lo) / flat_all * 100
imp_all_hi   <- (flat_all - flat_hi) / flat_all * 100
imp_drift_lo <- (drift_lo - flat_lo) / drift_lo * 100
imp_drift_hi <- (drift_hi - flat_hi) / drift_hi * 100
imp_nontr_lo <- (flat_nontr_lo - flat_lo) / flat_nontr_lo * 100
imp_nontr_hi <- (flat_nontr_hi - flat_hi) / flat_nontr_hi * 100
    
flu_summary <- tibble::tibble(
      Dataset = "Influenza",
      Variant = "Flatline",
      Transformation = "Yes",
      `Data Fitting (recent)` = "5-10",
      `Improvement over All (%)` = paste0(round(imp_all_lo, 2), " – ", round(imp_all_hi, 2)),
      `Improvement over drift variant (%)` = paste0(round(imp_drift_lo, 2), " – ", round(imp_drift_hi, 2)),
      `Improvement over non-transformed data (%)` = paste0(round(imp_nontr_lo, 2), " – ", round(imp_nontr_hi, 2))
    )
    
    
# =========================
# RSV (40–50), transformed
# =========================
w_lo <- 40
w_hi <- 50
    
flat_all <- avg2 %>%
      filter(dataset == "RSV", base == "Flatline", is_all, transformed == TRUE) %>%
      pull(mean_wis) %>% first()
    
flat_lo <- avg2 %>%
      filter(dataset == "RSV", base == "Flatline", !is_all, transformed == TRUE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
flat_hi <- avg2 %>%
      filter(dataset == "RSV", base == "Flatline", !is_all, transformed == TRUE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
drift_lo <- avg2 %>%
      filter(dataset == "RSV", base == "Drift", !is_all, transformed == TRUE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
drift_hi <- avg2 %>%
      filter(dataset == "RSV", base == "Drift", !is_all, transformed == TRUE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
flat_nontr_lo <- avg2 %>%
      filter(dataset == "RSV", base == "Flatline", !is_all, transformed == FALSE, window_num == w_lo) %>%
      pull(mean_wis) %>% first()
    
flat_nontr_hi <- avg2 %>%
      filter(dataset == "RSV", base == "Flatline", !is_all, transformed == FALSE, window_num == w_hi) %>%
      pull(mean_wis) %>% first()
    
imp_all_lo   <- (flat_all - flat_lo) / flat_all * 100
imp_all_hi   <- (flat_all - flat_hi) / flat_all * 100
imp_drift_lo <- (drift_lo - flat_lo) / drift_lo * 100
imp_drift_hi <- (drift_hi - flat_hi) / drift_hi * 100
imp_nontr_lo <- (flat_nontr_lo - flat_lo) / flat_nontr_lo * 100
imp_nontr_hi <- (flat_nontr_hi - flat_hi) / flat_nontr_hi * 100

rsv_summary <- tibble::tibble(
  Dataset = "RSV",
  Variant = "Flatline",
  Transformation = "Yes",
  `Data Fitting (recent)` = "40-50",
  `Improvement over All (%)` = paste0(round(imp_all_lo, 2), " – ", round(imp_all_hi, 2)),
  `Improvement over drift variant (%)` = paste0(round(imp_drift_lo, 2), " – ", round(imp_drift_hi, 2)),
  `Improvement over non-transformed data (%)` = paste0(round(imp_nontr_lo, 2), " – ", round(imp_nontr_hi, 2))
)


# =========================
# Combine all 4 rows
# =========================
final_summary <- dplyr::bind_rows(
  covid_summary,
  wili_summary,
  flu_summary,
  rsv_summary
)

final_summary


library(gt)

library(gt)

tab <- final_summary %>%
  gt() %>%
  tab_header(
    title = "Flatline baseline model performance summary",
  ) %>%
  cols_align(align = "center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Dataset)
  ) 
gtsave(tab, "plots/paper/table_flatline_summary.docx")

library(dplyr)
library(tidyr)
library(gt)

WINDOW_CHOSEN <- 10

# 1) Add flags and collapse duplicates (safe)
avg_u <- avg_all %>%
  mutate(
    data_chr = as.character(data),
    is_all   = data_chr == "all",
    is_w10   = (!is_all) & (as.numeric(data_num) == WINDOW_CHOSEN)
  ) %>%
  group_by(dataset, variation, data_chr) %>%
  summarise(
    mean_wis   = mean(mean_wis, na.rm = TRUE),
    mean_cov50 = mean(mean_cov50, na.rm = TRUE),
    mean_cov95 = mean(mean_cov95, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Build wide table: WIS at 10 and at all (plus optional cov)
wis_10_all <- avg_u %>%
  filter(data_chr %in% c(as.character(WINDOW_CHOSEN), "all")) %>%
  mutate(data_chr = ifelse(data_chr == "all", "all", paste0("w", data_chr))) %>%
  select(dataset, variation, data_chr, mean_wis, mean_cov50, mean_cov95) %>%
  pivot_wider(
    names_from  = data_chr,
    values_from = c(mean_wis, mean_cov50, mean_cov95)
  ) %>%
  arrange(dataset, variation)

print(wis_10_all)



library(dplyr)

flatline_tr_summary <- wis_10_all %>%
  group_by(dataset) %>%
  summarise(
    # Extract required WIS values
    flat_tr_10  = mean_wis_w10[variation == "Flatline (transformed)"],
    flat_tr_all = mean_wis_all[variation == "Flatline (transformed)"],
    drift_tr_10 = mean_wis_w10[variation == "Drift (transformed)"],
    flat_raw_10 = mean_wis_w10[variation == "Flatline"],
    
    # Targeted improvements
    `Improvement vs All (%)` =
      (flat_tr_all - flat_tr_10) / flat_tr_all * 100,
    
    `Improvement vs Drift (transformed) (%)` =
      (drift_tr_10 - flat_tr_10) / drift_tr_10 * 100,
    
    `Improvement vs Non-transformed Flatline (%)` =
      (flat_raw_10 - flat_tr_10) / flat_raw_10 * 100,
    
    .groups = "drop"
  ) %>%
  mutate(
    across(starts_with("Improvement"), ~ round(.x, 2))
  )

print(flatline_tr_summary)

final_table <- flatline_tr_summary %>%
  mutate(data = 10) %>%     # <-- add this
  select(
    dataset,
    data,
    `Improvement vs All (%)`,
    `Improvement vs Drift (transformed) (%)`,
    `Improvement vs Non-transformed Flatline (%)`
  )


library(gt)

tab <- final_table %>%
  gt() %>%
  cols_label(
    dataset = "Dataset",
    data = "Data",
    `Improvement vs All (%)` = "Improvement vs All (%)",
    `Improvement vs Drift (transformed) (%)` = "Improvement vs Drift (%)",
    `Improvement vs Non-transformed Flatline (%)` = "Improvement vs Raw Flatline (%)"
  ) %>%
  cols_align("center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

gtsave(tab, "plots/paper/table_flatline_summary.docx")

###
library(dplyr)
library(tidyr)
library(gt)

WINDOW_CHOSEN <- 10

# 1) Add flags and collapse duplicates (safe)
avg_u <- avg_all %>%
  mutate(
    data_chr = as.character(data),
    is_all   = data_chr == "all",
    is_w10   = (!is_all) & (as.numeric(data_num) == WINDOW_CHOSEN)
  ) %>%
  group_by(dataset, variation, data_chr) %>%
  summarise(
    mean_wis   = mean(mean_wis, na.rm = TRUE),
    mean_cov50 = mean(mean_cov50, na.rm = TRUE),
    mean_cov95 = mean(mean_cov95, na.rm = TRUE),
    .groups = "drop"
  )

# 2) Build wide table: WIS at 10 and at all (plus optional cov)
wis_10_all <- avg_u %>%
  filter(data_chr %in% c(as.character(WINDOW_CHOSEN), "all")) %>%
  mutate(data_chr = ifelse(data_chr == "all", "all", paste0("w", data_chr))) %>%
  select(dataset, variation, data_chr, mean_wis, mean_cov50, mean_cov95) %>%
  pivot_wider(
    names_from  = data_chr,
    values_from = c(mean_wis, mean_cov50, mean_cov95)
  ) %>%
  arrange(dataset, variation)

# 3) Summarize targeted improvements for transformed flatline
flatline_tr_summary <- wis_10_all %>%
  group_by(dataset) %>%
  summarise(
    # Extract required WIS values
    flat_tr_10  = mean_wis_w10[variation == "Flatline (transformed)"],
    flat_tr_all = mean_wis_all[variation == "Flatline (transformed)"],
    drift_tr_10 = mean_wis_w10[variation == "Drift (transformed)"],
    flat_raw_10 = mean_wis_w10[variation == "Flatline"],
    
    # Decomposition effects (percent improvement)
    `Full history vs 10 (%)` =
      (flat_tr_all - flat_tr_10) / flat_tr_all * 100,
    
    `Drift vs no drift (%)` =
      (drift_tr_10 - flat_tr_10) / drift_tr_10 * 100,
    
    `Transformed vs raw (%)` =
      (flat_raw_10 - flat_tr_10) / flat_raw_10 * 100,
    
    .groups = "drop"
  ) %>%
  mutate(across(ends_with("(%)"), ~ round(.x, 2)))

# 4) Add transformation column (MANUAL LOOKUP — edit these)
transform_lookup <- tibble::tribble(
  ~dataset,    ~transformation,
  "COVID-19",  "Yes",
  "Influenza", "Yes",
  "RSV",       "Yes",
  "wILI",      "Yes"
)

final_table <- flatline_tr_summary %>%
  left_join(transform_lookup, by = "dataset") %>%
  mutate(data = WINDOW_CHOSEN) %>%
  select(
    dataset,
    transformation,
    data,
    `Full history vs 10 (%)`,
    `Drift vs no drift (%)`,
    `Transformed vs raw (%)`
  )

# 5) Pretty table with gt
tab <- final_table %>%
  gt() %>%
  cols_label(
    dataset = "Dataset",
    transformation = "Transformation",
    data = "Window",
    `Full history vs 10 (%)` = "Full history vs 10 (%)",
    `Drift vs no drift (%)`  = "Drift effect (%)",
    `Transformed vs raw (%)` = "Transformation effect (%)"
  ) %>%
  cols_align("center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  )

gtsave(tab, "plots/paper/table_flatline_summary.docx")

