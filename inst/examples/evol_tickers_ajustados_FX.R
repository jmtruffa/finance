# Packages
library(quantmod)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

# -----------------------------
# Helpers
# -----------------------------
get_close <- function(sym, start) {
  x <- suppressWarnings(getSymbols(sym, src = "yahoo", from = start, auto.assign = FALSE))
  out <- if ("Adjusted" %in% colnames(x)) Ad(x) else Cl(x)
  tibble(date = as.Date(index(out)), value = as.numeric(out[, 1])) %>% arrange(date)
}

asof_value <- function(df, cutoff_date) {
  x <- df %>% filter(date <= cutoff_date, !is.na(value)) %>% arrange(date)
  if (nrow(x) == 0) return(list(date = as.Date(NA), value = NA_real_))
  list(date = x$date[nrow(x)], value = as.numeric(x$value[nrow(x)]))
}

# Your local FX tibble uses "local currency per USD", so convert to "USD per local currency"
fx_asof <- function(fx_tbl, fx_col, cutoff_date) {
  x <- fx_tbl %>%
    select(date, value = all_of(fx_col)) %>%
    filter(date <= cutoff_date, !is.na(value)) %>%
    arrange(date)
  if (nrow(x) == 0) return(list(date = as.Date(NA), value = NA_real_))
  list(date = x$date[nrow(x)], value = as.numeric(1 / x$value[nrow(x)]))
}

# -----------------------------
# Inputs
# -----------------------------
# Indices (native currency) + which FX series to use
IDX <- tibble::tribble(
  ~label,             ~ticker,     ~local_col,          ~fx_col,          ~fx_source,
  "KOSPI",            "^KS11",     "KOSPI_KRW",         "KRWUSD",          "yahoo",
  "EUROSTOXX50",      "^STOXX50E", "EUROSTOXX50_EUR",   "EURUSD",          "yahoo",
  "HANG SENG",        "^HSI",      "HANGSENG_HKD",      "HKDUSD_eff",      "yahoo",
  "NIKKEI",           "^N225",     "NIKKEI225_JPY",     "JPYUSD",          "yahoo",
  "NDX",              "^NDX",      "NDX_USD",           NA_character_,     "none",
  "SPX",              "^GSPC",     "SPX_USD",           NA_character_,     "none",
  
  # Argentina: same index, different FX columns from your `fx` tibble
  "MERV (MEP)",       "^MERV",     "MERV_ARS",          "mepAL",           "fx_tbl",
  "MERV (CCL)",       "^MERV",     "MERV_ARS",          "ccl3",            "fx_tbl",
  "MERV (A3500)",     "^MERV",     "MERV_ARS",          "A3500",           "fx_tbl",
  
  # Brazil: EWZ (USD) + BRL FX attribution
  "EWZ",              "EWZ",       "EWZ_USD",           NA_character_,          "yahoo",
  "ILF",              "ILF",       "ILF_USD",           NA_character_,          "yahoo"
)

# FX from Yahoo
# Note: BRL=X is usually BRL per USD, so we invert to create BRLUSD = USD per BRL
FX <- c(
  "EURUSD" = "EURUSD=X",   # USD per EUR
  "JPYUSD" = "JPYUSD=X",   # USD per JPY
  "KRWUSD" = "KRWUSD=X",   # USD per KRW
  "HKDUSD" = "HKDUSD=X",   # USD per HKD (preferred)
  "USDHKD" = "HKD=X",      # HKD per USD (fallback)
  "USDBRL_raw" = "BRL=X"   # BRL per USD (raw; will invert)
)

START <- as.Date("2024-01-01")

# -----------------------------
# Download series from Yahoo
# -----------------------------
idx_tickers <- unique(IDX$ticker)
idx_series <- setNames(idx_tickers, idx_tickers) %>% map(~get_close(.x, START))

fx_series <- setNames(unname(FX), unname(FX)) %>% map(~get_close(.x, START))
names(fx_series) <- names(FX)  # EURUSD, ..., USDBRL_raw

# -----------------------------
# Build wide px table (dates union)
# -----------------------------
# Indices long (ticker -> local_col)
idx_long <- imap_dfr(idx_series, ~{
  # For tickers used multiple times (e.g., ^MERV), local_col is the same; take first
  nm <- IDX %>% filter(ticker == .y) %>% pull(local_col) %>% unique() %>% .[[1]]
  tibble(date = .x$date, name = nm, value = .x$value)
})

# FX long (already named)
fx_long <- imap_dfr(fx_series, ~tibble(date = .x$date, name = .y, value = .x$value))

px <- bind_rows(idx_long, fx_long) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  arrange(date)

# Forward-fill Yahoo FX raw columns
ffill_cols <- intersect(c("EURUSD","JPYUSD","KRWUSD","HKDUSD","USDHKD","USDBRL_raw"), names(px))
px <- px %>% fill(all_of(ffill_cols), .direction = "down")

# HKDUSD_eff fallback: prefer HKDUSD else 1/USDHKD
if ("HKDUSD" %in% names(px) && "USDHKD" %in% names(px)) {
  px <- px %>%
    mutate(HKDUSD_eff = ifelse(!is.na(HKDUSD), HKDUSD, 1 / USDHKD)) %>%
    fill(HKDUSD_eff, .direction = "down")
} else if ("HKDUSD" %in% names(px)) {
  px <- px %>% mutate(HKDUSD_eff = HKDUSD) %>% fill(HKDUSD_eff, .direction = "down")
} else {
  px <- px %>% mutate(HKDUSD_eff = 1 / USDHKD) %>% fill(HKDUSD_eff, .direction = "down")
}

# BRLUSD: USD per BRL (invert BRL per USD)
if ("USDBRL_raw" %in% names(px)) {
  px <- px %>%
    mutate(BRLUSD = 1 / USDBRL_raw) %>%
    fill(BRLUSD, .direction = "down")
}

# -----------------------------
# Dates t0 (prev year end) and t1 (last available)
# -----------------------------
t1 <- max(px$date, na.rm = TRUE)
Y <- year(t1)
t0_cutoff <- as.Date(sprintf("%d-12-31", Y - 1))

# -----------------------------
# Build panel (rows x indices) like your image
# Assumes `fx` tibble exists in env with columns: date, mepAL, ccl3, A3500, ...
# -----------------------------
rows <- c("FX_base","FX_t","retorno FX","par FX","Idx_base","Idx_t","Retorno Idx","retorno usd")

panel <- matrix(NA_character_, nrow = length(rows), ncol = nrow(IDX),
                dimnames = list(rows, IDX$label))

for (i in seq_len(nrow(IDX))) {
  lab <- IDX$label[i]
  local_col <- IDX$local_col[i]
  fx_col <- IDX$fx_col[i]
  fx_source <- IDX$fx_source[i]
  
  # Index base/current (from px)
  df_local <- px %>% select(date, value = all_of(local_col)) %>% arrange(date)
  p0 <- asof_value(df_local, t0_cutoff)$value
  p1 <- asof_value(df_local, t1)$value
  r_idx <- if (!is.na(p0) && p0 != 0) (p1 / p0 - 1) else NA_real_
  
  # FX base/current
  if (is.na(fx_col)) {
    fx0 <- 1.0; fx1 <- 1.0; r_fx <- 0.0; fx_label <- "N/A"
  } else if (fx_source == "fx_tbl") {
    fx0 <- fx_asof(fx, fx_col, t0_cutoff)$value
    fx1 <- fx_asof(fx, fx_col, t1)$value
    r_fx <- if (!is.na(fx0) && fx0 != 0) (fx1 / fx0 - 1) else NA_real_
    fx_label <- fx_col
  } else {
    df_fx <- px %>% select(date, value = all_of(fx_col)) %>% arrange(date)
    fx0 <- asof_value(df_fx, t0_cutoff)$value
    fx1 <- asof_value(df_fx, t1)$value
    r_fx <- if (!is.na(fx0) && fx0 != 0) (fx1 / fx0 - 1) else NA_real_
    fx_label <- fx_col
  }
  
  # USD return via composition (matches your sheet)
  r_usd <- (1 + r_idx) * (1 + r_fx) - 1
  
  panel["FX_base", lab] <- sprintf("%.6f", fx0)
  panel["FX_t", lab] <- sprintf("%.6f", fx1)
  panel["retorno FX", lab] <- ifelse(is.na(r_fx), "", sprintf("%.2f%%", 100 * r_fx))
  panel["par FX", lab] <- fx_label
  panel["Idx_base", lab] <- sprintf("%.2f", p0)
  panel["Idx_t", lab] <- sprintf("%.2f", p1)
  panel["Retorno Idx", lab] <- ifelse(is.na(r_idx), "", sprintf("%.2f%%", 100 * r_idx))
  panel["retorno usd", lab] <- ifelse(is.na(r_usd), "", sprintf("%.2f%%", 100 * r_usd))
}

panel_df <- as.data.frame(panel)
x <- panel_df["retorno usd", ] %>%
  unlist(use.names = FALSE) %>%
  gsub("%", "", ., fixed = TRUE) %>%
  trimws() %>%
  as.numeric()

ord <- order(x, decreasing = TRUE, na.last = TRUE)
panel_df_sorted <- panel_df[, ord]
panel_df_sorted
