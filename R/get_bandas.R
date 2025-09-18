#' @title get_bandas
#' @description Devuelve las bandas superior e inferior según la "FASE 3" del programa monetario del gobierno Milei. Comienza el 14-abr-25 con 1000 y 1400. Ajusta al 1% mensual
#' @return Un data frame con las bandas superior e inferior y la fecha.
#' @export
get_bandas<- function(
    end_date    = "2027-12-31",
    start_date  = "2025-04-14",
    upper_start = 1400,
    lower_start = 1000,
    monthly_rate = 0.01   # 1% cada 30 días
) {
  # sin días hábiles: usamos días calendario
  # factor diario equivalente para ±1% cada 30 días
  daily_up   <- (1 + monthly_rate)^(1/30)
  daily_down <- (1 - monthly_rate)^(1/30)

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  dates      <- seq(start_date, end_date, by = "day")

  # días transcurridos desde el inicio (0 para start_date)
  days_since <- as.integer(dates - start_date)

  # fórmula cerrada equivalente a "nuevo = anterior * factor_diario"
  banda_superior <- upper_start * (daily_up)^(days_since)
  banda_inferior <- lower_start * (daily_down)^(days_since)

  tibble::tibble(
    date = dates,
    banda_inferior = banda_inferior,
    banda_superior = banda_superior
  )
}

