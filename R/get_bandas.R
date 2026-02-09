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

#' @title get_bandas2
#' @description Bandas superior e inferior que ajustan en la misma dirección.
#' Permite usar una tasa mensual fija (monthly_rate) o un vector de tasas mensuales (monthly_rates).
#' El ajuste se aplica mensualmente (al cambio de mes) y se expande a días calendario.
#' @return tibble con date, banda_inferior, banda_superior, y la tasa_mensual aplicada en cada día.
#' @export
# get_bandas2 <- function(
#     end_date      = "2027-12-31",
#     start_date    = "2025-04-14",
#     upper_start   = 1400,
#     lower_start   = 1000,
#     monthly_rate  = NULL,          # tasa única (ej: 0.01)
#     monthly_rates = NULL           # vector (ej: c(0.01, 0.008, 0.012, ...))
# ) {
#   # Validaciones
#   if (is.null(monthly_rate) && is.null(monthly_rates)) {
#     stop("Debe proveer monthly_rate o monthly_rates.")
#   }
#   if (!is.null(monthly_rate) && !is.null(monthly_rates)) {
#     stop("Proveer solo uno: monthly_rate o monthly_rates.")
#   }
#
#   start_date <- as.Date(start_date)
#   end_date   <- as.Date(end_date)
#   if (end_date < start_date) stop("end_date debe ser >= start_date.")
#
#   dates <- seq(start_date, end_date, by = "day")
#
#   # Identificador de mes (primer día del mes) para mapear tasas mensuales
#   month_id <- as.Date(format(dates, "%Y-%m-01"))
#   month_levels <- sort(unique(month_id))
#   n_months <- length(month_levels)
#
#   # Construir vector de tasas mensuales por mes
#   if (!is.null(monthly_rate)) {
#     rates_by_month <- rep(monthly_rate, n_months)
#   } else {
#     if (length(monthly_rates) < n_months) {
#       stop(sprintf(
#         "monthly_rates tiene largo %d pero se necesitan %d (uno por mes desde start_date hasta end_date).",
#         length(monthly_rates), n_months
#       ))
#     }
#     rates_by_month <- monthly_rates[seq_len(n_months)]
#   }
#
#   # Mapea cada día a la tasa de su mes
#   rate_day <- rates_by_month[match(month_id, month_levels)]
#
#   # Factor diario equivalente dentro de cada mes: (1+r_m)^(1/30) (días calendario, como tu función original)
#   daily_factor_day <- (1 + rate_day)^(1/30)
#
#   # Factor acumulado día a día (producto acumulado)
#   cum_factor <- cumprod(daily_factor_day)
#
#   tibble::tibble(
#     date = dates,
#     tasa_mensual = rate_day,
#     banda_inferior = lower_start * cum_factor,
#     banda_superior = upper_start * cum_factor
#   )
# }
get_bandas2 <- function(
    end_date      = "2027-12-31",
    start_date    = "2025-04-14",
    upper_start   = 1400,
    lower_start   = 1000,
    monthly_rate  = NULL,          # tasa única (ej: 0.01)
    monthly_rates = NULL           # vector (ej: c(0.01, 0.008, 0.012, ...))
) {
  # Validaciones
  if (is.null(monthly_rate) && is.null(monthly_rates)) {
    stop("Debe proveer monthly_rate o monthly_rates.")
  }
  if (!is.null(monthly_rate) && !is.null(monthly_rates)) {
    stop("Proveer solo uno: monthly_rate o monthly_rates.")
  }

  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  if (end_date < start_date) stop("end_date debe ser >= start_date.")

  dates <- seq(start_date, end_date, by = "day")

  # Identificador de mes (primer día del mes) para mapear tasas mensuales
  month_id     <- as.Date(format(dates, "%Y-%m-01"))
  month_levels <- sort(unique(month_id))
  n_months     <- length(month_levels)

  # Construir vector de tasas mensuales por mes
  if (!is.null(monthly_rate)) {
    rates_by_month <- rep(monthly_rate, n_months)
  } else {
    if (length(monthly_rates) < n_months) {
      stop(sprintf(
        "monthly_rates tiene largo %d pero se necesitan %d (uno por mes desde start_date hasta end_date).",
        length(monthly_rates), n_months
      ))
    }
    rates_by_month <- monthly_rates[seq_len(n_months)]
  }

  # Mapea cada día a la tasa de su mes
  rate_day <- rates_by_month[match(month_id, month_levels)]

  # Factores diarios equivalentes (días calendario, como en tu función original)
  daily_up   <- (1 + rate_day)^(1/30)  # <- CLAVE: la superior crece
  daily_down <- (1 - rate_day)^(1/30)  # <- CLAVE: la inferior decrece a la misma tasa

  # Factores acumulados día a día
  cum_up   <- cumprod(daily_up)        # <- CLAVE: aplica solo a la superior
  cum_down <- cumprod(daily_down)      # <- CLAVE: aplica solo a la inferior

  tibble::tibble(
    date = dates,
    tasa_mensual = rate_day,
    banda_inferior = lower_start * cum_down,
    banda_superior = upper_start * cum_up
  )
}


## implementación
# df <- get_bandas2(start_date = "2026-01-01",
#                   end_date = "2026-12-31",
#                   upper_start = get_bandas(end_date = "2025-12-31") %>% tail(n=1) %>% pull(banda_superior),
#                   lower_start = get_bandas(end_date = "2025-12-31") %>% tail(n=1) %>% pull(banda_inferior),
#                   monthly_rates = c(.025, .023, .022, .022, .021, 0.021, rep(.02, 3), rep(.019, 3))) %>%
#   # rename(inferior_est_propia = banda_inferior,
#   #        superior_est_propia = banda_superior) %>%
#   select(-tasa_mensual)
#
# df_rem <- get_bandas2(start_date = "2026-01-01",
#                       end_date = "2026-12-31",
#                       upper_start = get_bandas(end_date = "2025-12-31") %>% tail(n=1) %>% pull(banda_superior),
#                       lower_start = get_bandas(end_date = "2025-12-31") %>% tail(n=1) %>% pull(banda_inferior),
#                       monthly_rate = c(.019, .017, .018, .016, .015, rep(.01362, 7))) %>%
#   # rename(inferior_est_REM = banda_inferior,
#   #        superior_est_REM = banda_superior) %>%
#   select(-tasa_mensual)
#
# df_actual <- get_bandas(end_date = "2026-12-31") %>%
#   filter(date > "2025-12-31")
#
# df_fx = fx %>%
#   select(-canje) %>%
#   left_join(
#     get_bandas(end_date = "2025-12-31")
#     ) %>%
#   filter(date >= "2025-04-14")  %>%
#   add_row(get_bandas(end_date = "2025-12-31", ) %>% filter(date >= "2025-12-17")) %>%
#   add_row(df) %>%
#   rename(inferior_est_propia = banda_inferior,
#          superior_est_propia = banda_superior) %>%
#   left_join(
#     df_rem %>%
#       transmute(
#         date,
#         inferior_est_REM = banda_inferior,
#         superior_est_REM = banda_superior
#       ),
#     by = "date"
#   ) %>%
#   left_join(
#     df_actual %>%
#       transmute(
#         date,
#         inferior_est_actual = banda_inferior,
#         superior_est_actual = banda_superior
#       ),
#     by = "date"
#   )
#
#
#
# labels_bandas <- df_fx %>%
#   summarise(
#     date = max(date),
#     superior_est_propia = last(na.omit(superior_est_propia)),
#     inferior_est_propia = last(na.omit(inferior_est_propia)),
#     superior_est_REM    = last(na.omit(superior_est_REM)),
#     inferior_est_REM    = last(na.omit(inferior_est_REM)),
#     superior_est_actual = last(na.omit(superior_est_actual)),
#     inferior_est_actual = last(na.omit(inferior_est_actual))
#
#   ) %>%
#   pivot_longer(
#     -date,
#     names_to = "serie",
#     values_to = "valor"
#   ) %>%
#   mutate(
#     tipo = case_when(
#       grepl("propia", serie) ~ "Estimación Propia",
#       grepl("REM", serie)    ~ "Estimación REM",
#       grepl("actual", serie) ~ "Bandas Actual"
#     )
#   )
#
# labels_bandas <- labels_bandas %>%
#   mutate(
#     label_txt = case_when(
#       tipo == "Estimación Propia" ~ paste0("Est. Propia: ", round(valor, 0)),
#       tipo == "Estimación REM"    ~ paste0("Est. REM: ", round(valor, 0)),
#       tipo == "Bandas Actual"      ~ paste0("Bandas Actuales: ", round(valor, 0))
#     )
#   )
#
# colores <- c(
#   "Estimación Propia" = "grey50",
#   "Estimación REM"    = "black",
#   "Bandas Actual"      = "darkgreen",
#   "tc_A3500"          = unname(col_tc["tc_A3500"]),
#   "tc_ccl3"           = unname(col_tc["tc_ccl3"]),
#   "tc_mepAL"          = unname(col_tc["tc_mepAL"])
# )
#
# g_tipos_de_cambio_y_bandas_2026=df_fx %>%
#   ggplot(aes(x = date)) +
#   theme_usado() +
#   # bandas
#   geom_line(aes(y = superior_est_propia, colour = "Estimación Propia"), linetype = "dashed") +
#   geom_line(aes(y = inferior_est_propia, colour = "Estimación Propia"), linetype = "dashed") +
#   geom_line(aes(y = superior_est_REM,    colour = "Estimación REM"),    linetype = "dashed") +
#   geom_line(aes(y = inferior_est_REM,    colour = "Estimación REM"),    linetype = "dashed") +
#   geom_line(aes(y = superior_est_actual, colour = "Bandas Actual"),      linetype = "dashed") +
#   geom_line(aes(y = inferior_est_actual, colour = "Bandas Actual"),      linetype = "dashed") +
#   # tipos de cambio
#   geom_line(aes(y = mepAL, colour = "tc_mepAL"),  size = 1) +
#   geom_line(aes(y = ccl3,  colour = "tc_ccl3"),   size = 1) +
#   geom_line(aes(y = A3500, colour = "tc_A3500"),  size = 1) +
#   # etiquetas bandas
#   geom_text(
#     data = labels_bandas,
#     aes(
#       x = date,
#       y = valor,
#       label = label_txt,
#       colour = tipo
#     ),
#     hjust = -0.1,
#     size = 3,
#     show.legend = FALSE
#   )+
#   scale_colour_manual(
#     values = colores,
#     breaks = c("tc_A3500","tc_ccl3","tc_mepAL","Estimación Propia","Estimación REM"),
#     labels = c("A3500","CCL","MEP","Estimación Propia","Estimación REM"),
#     name = NULL
#   ) +
#   scale_x_date(
#     date_breaks = "1 month",
#     labels = scales::label_date("%m-%y"),
#     expand = expansion(mult = c(0.01, 0.08))
#   ) +
#   scale_y_continuous(
#     breaks = scales::breaks_extended(10),
#     labels = scales::label_currency(big.mark = ".", decimal.mark = ",")
#   ) +
#   labs(
#     x = NULL,
#     y = "Pesos",
#     title = "TIPOS DE CAMBIO Y BANDAS ACTUALES Y NUEVA IMPLEMENTACIÓN",
#     subtitle = paste0("Valores de TC al: ", max(fx$date)),
#     caption = paste0(.pie, ' en base a datos de mercado y BCRA')
#   )
# grabaGrafo(variable = g_tipos_de_cambio_y_bandas_2026, name = "g_tipos_de_cambio_y_bandas_2026")
# df_fx %>%
#   ggplot(aes(x = date)) +
#   theme_usado() +
#
#   # bandas
#   geom_line(aes(y = superior_est_propia), linetype = "dashed", colour = "grey50") +
#   geom_line(aes(y = inferior_est_propia),  linetype = "dashed", colour = "grey50") +
#
#   geom_line(aes(y = superior_est_REM), linetype = "dashed", colour = "black") +
#   geom_line(aes(y = inferior_est_REM),  linetype = "dashed", colour = "black") +
#
#   geom_line(aes(y = mepAL, colour = "tc_mepAL"), size = 1) +
#   geom_line(aes(y = ccl3, colour = "tc_ccl3"), size = 1) +
#   geom_line(aes(y = A3500, colour = "tc_A3500"), size = 1) +
#
#   # puntos + etiquetas: tipos de cambio
#   scale_colour_manual(values = col_tc, name = NULL, labels = c("tc_A3500", "tc_ccl3", "tc_MEP")) +
#
#
#   scale_x_date(date_breaks="1 month", label = scales::label_date("%m-%y")) +
#   scale_y_continuous(breaks = breaks_extended(10), labels = label_currency(big.mark = ".", decimal.mark = ",")) +
#   labs(x = NULL,
#        y = "Pesos",
#        title = "TIPOS DE CAMBIO Y BANDAS ACTUALES Y NUEVA IMPLEMENTACIÓN",
#        subtitle = paste0("Valores de TC al: " , max(df_fx$date)))



