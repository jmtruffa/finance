#' @title get_bandas
#' @description Devuelve las bandas superior e inferior según la "FASE 3" del programa monetario del gobierno Milei. Comienza el 14-abr-25 con 1000 y 1400. Ajusta al 1% mensual
#' @return Un data frame con las bandas superior e inferior y la fecha.
#' @export
get_bandas <- function(end_date = "2027-12-31") {
  library(bizdays)
  library(lubridate)
  library(dplyr)
  library(tibble)

  cal <- create.calendar(name = "cal")

  start_date <- as.Date("2025-04-14")   # donde arrancan las bandas
  end_date   <- as.Date(end_date)

  dates <- bizseq(start_date, end_date, cal = "cal")   # vector Date

  # Diferencia en meses enteros
  full_months <- (year(dates) - year(start_date)) * 12 + (month(dates) - month(start_date))

  # Día dentro del mes y duración del mes
  day_of_month <- day(dates)
  days_in_month <- days_in_month(dates)

  # Día de start_date dentro de su mes
  start_day <- day(start_date)

  # Si estamos en el mismo mes del start_date, ajustar desde ese día
  same_month <- (year(dates) == year(start_date)) & (month(dates) == month(start_date))
  day_offset <- ifelse(same_month, day_of_month - start_day, day_of_month - 1)
  day_offset <- pmax(day_offset, 0)

  # Proporción dentro del mes
  month_fraction <- day_offset / days_in_month

  # Resultado final en meses decimales, ajustado por duración real del mes
  meses_dec <- full_months + month_fraction

  df_bandas <- tibble(
    date           = dates,
    banda_inferior = 1000 * (1 - 0.01) ^ meses_dec,
    banda_superior = 1400 * (1 + 0.01) ^ meses_dec
  )

  return(df_bandas)
}
