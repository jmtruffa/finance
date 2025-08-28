#' @title extend_CER
#' @description Devuelve el CER con proyección futura basada en una serie de inflaciones mensuales. Agrega tantos meses como elementos en el vector de inflación.
#' @return CER nuevo con proyección futura y una columna indicando qué valores son reales (previos) y qué valores proyectados
#' @export
extend_CER <- function(infl_vec, ...) {
  # Lee la serie real desde la BD usando los argumentos pasados por ...
  CER_df <- dbGetTable("CER", ...)

  # --- Helpers base R (sin lubridate) ---
  add_one_month_keep_day <- function(d) {
    # d es Date; mantenemos el día (es 15 por construcción)
    y <- as.integer(format(d, "%Y"))
    m <- as.integer(format(d, "%m"))
    day <- as.integer(format(d, "%d"))  # será 15
    if (m == 12) { y <- y + 1; m <- 1 } else { m <- m + 1 }
    as.Date(sprintf("%04d-%02d-%02d", y, m, day))
  }
  is_day_15 <- function(x) as.integer(format(x, "%d")) == 15

  # Validaciones mínimas
  stopifnot(all(c("date", "CER") %in% names(CER_df)))
  if (!inherits(CER_df$date, "Date")) {
    CER_df$date <- as.Date(CER_df$date)
  }
  if (!is.numeric(CER_df$CER)) stop("CER_df$CER debe ser numérico.")
  if (!is.numeric(infl_vec))   stop("infl_vec debe ser numérico.")

  # Ancla: último día 15 real
  i15 <- is_day_15(CER_df$date)
  if (!any(i15)) stop("No se encontró ningún día 15 en la serie real (CER_df).")
  anchor_15 <- max(CER_df$date[i15], na.rm = TRUE)
  last_real_date <- max(CER_df$date, na.rm = TRUE)
  if (last_real_date != anchor_15) {
    stop(sprintf(
      "Se esperaba que la serie real termine en un día 15. Último real: %s; último 15: %s",
      format(last_real_date), format(anchor_15)
    ))
  }
  anchor_val <- CER_df$CER[match(anchor_15, CER_df$date)]

  # Proyección por ventanas 16→15 (calendario corrido; diff_dias = 15→15)
  out_dates <- as.Date(character())
  out_vals  <- numeric(0)
  start_15  <- anchor_15

  for (i in seq_along(infl_vec)) {
    infl_i   <- infl_vec[i]
    end_15   <- add_one_month_keep_day(start_15)      # 15 del mes siguiente
    diff_dias <- as.integer(end_15 - start_15)        # días entre 15(m+1) y 15(m)

    # Fechas a proyectar: del 16 al 15 siguiente (inclusive)
    dates_i <- seq(from = start_15 + 1L, to = end_15, by = "day")
    if (length(dates_i) != diff_dias) {
      stop(sprintf("Inconsistencia: length(dates_i)=%d vs diff_dias=%d",
                   length(dates_i), diff_dias))
    }

    # Crecimiento diario constante
    g <- (1 + infl_i)^(1 / diff_dias)
    vals_i <- anchor_val * g^(seq_along(dates_i))     # 16: *g ... 15(next): *g^diff

    # Acumular tramo
    out_dates <- c(out_dates, dates_i)
    out_vals  <- c(out_vals,  vals_i)

    # Nuevo ancla: 15 siguiente y su CER
    start_15  <- end_15
    anchor_val <- tail(vals_i, 1)
  }

  proj_df <- data.frame(
    date   = out_dates,
    CER    = out_vals,
    source = "proy",
    stringsAsFactors = FALSE
  )

  real_df <- data.frame(
    date   = CER_df$date,
    CER    = CER_df$CER,
    source = "real",
    stringsAsFactors = FALSE
  )

  extended <- rbind(real_df, proj_df)
  extended <- extended[order(extended$date), ]
  rownames(extended) <- NULL
  extended
}
