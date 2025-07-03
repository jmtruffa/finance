#' @title get_bandas
#' @description Devuelve las bandas superior e inferior según la "FASE 3" del programa monetario del gobierno Milei. Comienza el 14-abr-25 con 1000 y 1400. Ajusta al 1% mensual
#' @return Un data frame con las bandas superior e inferior y la fecha.
#' @export
get_bandas = function() {
library(bizdays)
library(lubridate)
library(functions)

cal <- create.calendar(
  name      = "cal",
  holidays  = functions::getFeriados(server = server, port = port),
  weekdays  = c("saturday", "sunday")
)
start_date <- as.Date("2025-04-14")   # donde arrancan las bandas
end_date   <- as.Date("2026-12-31")

dates <- bizseq(start_date, end_date, cal = "cal")   # vector Date
meses_dec <- time_length(interval(start_date, dates), unit = "months")

df_bandas <- tibble(
  date            = dates,
  banda_inferior  = 1000 * (1 - 0.01) ^ meses_dec,
  banda_superior  = 1400 * (1 + 0.01) ^ meses_dec
)
return(df_bandas)
}
