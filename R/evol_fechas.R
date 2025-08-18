#' @title Evolución de un campo entre dos fechas
#' @description
#' Esta función permite comparar un campo específico entre dos fechas en un dataframe.
#' #' @param df Dataframe que contiene los datos.
#' @param fecha1 Fecha inicial para la comparación.
#' @param fecha2 Fecha final para la comparación.
#' @param campo Nombre del campo a comparar.
#' @param campo_fecha Nombre del campo que contiene las fechas (por defecto "date").
#' @return Dataframe con la evolución del campo entre las dos fechas, incluyendo la variación porcentual.
#' @examples evol_fechas(df, fecha1= "2025-07-15", fecha2 = "2025-07-31", campo = "price", campo_fecha = "date")
#'
evol_fechas <- function(df, fecha1, fecha2, campo, campo_fecha = "date") {
  library(dplyr)
  library(rlang)

  # Asegurar que las fechas sean tipo Date
  fecha1 <- as.Date(fecha1)
  fecha2 <- as.Date(fecha2)

  # Convertir nombres de columnas a símbolos
  campo_sym <- sym(campo)
  fecha_sym <- sym(campo_fecha)

  # Filtrar por fecha1 y conservar todas las columnas excepto campo_a_comparar y campo_fecha
  df1 <- df %>%
    filter(!!fecha_sym == fecha1) %>%
    select(-all_of(c(campo, campo_fecha)), .valor_fecha1 = !!campo_sym)

  # Filtrar por fecha2 y quedarse con todos los campos
  df2 <- df %>%
    filter(!!fecha_sym == fecha2)

  # Determinar claves comunes para el join
  join_keys <- intersect(names(df1), names(df2))

  # Unir y calcular variación
  df_comparado <- df2 %>%
    left_join(df1, by = join_keys) %>%
    mutate(variacion_pct = round((.data[[campo]] / .valor_fecha1 - 1) * 100, 2))

  return(df_comparado)
}
