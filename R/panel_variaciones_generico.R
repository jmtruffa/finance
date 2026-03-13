#' Panel de variaciones de precios (genérico)
#'
#'Calcula variaciones de precios por ticker (1D, 5D, 1M, YTD) usando calendarios
#'de días hábiles por ticker, agrupa instrumentos y devuelve una o varias
#'\pkg{flextable} en función de \code{max_tickers_por_panel}.
#'
#' @param datos data.frame con columnas \code{date}, \code{ticker}, \code{price}.
#'   \code{date} debe ser convertible a \code{Date}.
#' @param calendarios Mapeo ticker -> calendario. Puede ser:
#'   (i) named character vector con nombres de calendario \pkg{bizdays}, o
#'   (ii) data.frame con columnas \code{ticker} y \code{calendario}.
#' @param grupos (Opcional) Mapeo ticker -> grupo. Puede ser named vector o
#'   data.frame con columnas \code{ticker} y \code{grupo}. Si falta, usa \code{"OTROS"}.
#' @param nombres_display (Opcional) Mapeo ticker -> nombre para mostrar. Puede ser
#'   named vector o data.frame con columnas \code{ticker} y \code{nombre}.
#' @param fecha_referencia Fecha base para el cálculo (por defecto \code{Sys.Date()}).
#' @param max_tickers_por_panel Máximo de filas por panel antes de dividir (default 15).
#' @param titulo Título a insertar como header en cada \pkg{flextable}.
#' @param nota_pie Nota al pie a insertar en cada \pkg{flextable}.
#' @param cal_fallback Nombre del calendario \pkg{bizdays} a usar si falta el específico.
#' @param server Host/alias para obtener feriados via \code{functions::getFeriados()}.
#' @param port Puerto para obtener feriados via \code{functions::getFeriados()}.
#'
#' @details
#' Las fechas de referencia se calculan en días hábiles usando el calendario asignado
#' al ticker. Si el calendario no existe, se usa \code{cal_fallback}. Si \code{cal_fallback}
#' no existe, se crea con feriados de Argentina obtenidos desde \code{functions::getFeriados()}.
#'
#' @return Un objeto \code{flextable} si el panel entra en un solo bloque, o una lista
#' de \code{flextable} si requiere división.
#'
#' @examples
#' \dontrun{
#' datos <- data.frame(
#'   date = as.Date(c("2026-01-02","2026-01-03")),
#'   ticker = c("AL30","AL30"),
#'   price = c(100, 101)
#' )
#' calendarios <- c(AL30 = "cal")
#' ft <- panel_variaciones_generico(datos, calendarios)
#' }
#'
#' @importFrom dplyr %>% filter arrange select mutate across bind_rows
#' @importFrom purrr map map_dfr
#' @importFrom lubridate %m-% months year
#' @importFrom bizdays offset adjust.next adjust.previous create.calendar
#' @importFrom flextable flextable set_header_labels bold color vline align
#' @importFrom flextable set_table_properties add_header_lines add_footer_lines
#' @export
panel_variaciones_generico <- function(...) { ... }

crear_calendario_fallback <- function(cal_nombre = "cal", server = NULL, port = NULL) {
  # Verificar si el calendario ya existe intentando usarlo
  cal_existe <- tryCatch({
    # Intentar usar el calendario con una fecha de prueba
    bizdays::offset(as.Date("2024-01-01"), 0, cal_nombre)
    TRUE
  }, error = function(e) FALSE)

  if (cal_existe) {
    return(cal_nombre)
  }

  # Usar valores por defecto si no se proporcionan
  # Primero verificar si existen variables globales (establecidas por setup())
  if (is.null(server) || is.null(port)) {
    # Si alguna variable global no existe, llamar a setup() para establecerlas
    if (!exists("server", envir = .GlobalEnv) || !exists("port", envir = .GlobalEnv)) {
      if (exists("setup", envir = asNamespace("functions"))) {
        functions::setup()
      }
    }

    # Usar las variables globales (ya sea existentes o recién establecidas)
    if (is.null(server)) {
      if (exists("server", envir = .GlobalEnv)) {
        server <- get("server", envir = .GlobalEnv)
      } else {
        server <- "local"  # Fallback si setup() no está disponible
      }
    }

    if (is.null(port)) {
      if (exists("port", envir = .GlobalEnv)) {
        port <- get("port", envir = .GlobalEnv)
      } else {
        port <- 5432  # Fallback si setup() no está disponible
      }
    }
  }

  # Obtener feriados de Argentina
  feriados <- functions::getFeriados(server = server, port = port)

  # Crear calendario
  bizdays::create.calendar(
    name = cal_nombre,
    holidays = feriados,
    weekdays = c('saturday', 'sunday')
  )

  return(cal_nombre)
}

# -----------------------------
# Fase 1: Función auxiliar para calcular fechas de referencia
# -----------------------------
calcular_fechas_referencia <- function(calendario, fecha_ref, cal_fallback = "cal") {
  require(bizdays)
  require(lubridate)

  # Convertir fecha a Date si es necesario
  fecha_ref <- as.Date(fecha_ref)

  # Si calendario es NULL o NA, usar fallback
  if (is.null(calendario) || is.na(calendario) || calendario == "") {
    calendario <- cal_fallback
  }

  # Validar que el calendario existe, si no usar fallback
  cal_existe <- tryCatch({
    # Intentar usar el calendario con una fecha de prueba
    bizdays::offset(fecha_ref, 0, calendario)
    TRUE
  }, error = function(e) FALSE)

  if (!cal_existe) {
    warning(paste("Calendario", calendario, "no existe. Usando calendario de fallback:", cal_fallback))
    calendario <- cal_fallback
  }

  # Ajustar fecha de referencia al último día hábil del calendario
  fecha_ref <- bizdays::adjust.previous(fecha_ref, calendario)
  
  # Calcular fechas de referencia
  # 1 día hábil antes
  date1d <- bizdays::offset(fecha_ref, -1, calendario)

  # 5 días hábiles antes
  date5d <- bizdays::offset(fecha_ref, -5, calendario)

  # Mismo día del mes anterior (ajustado)
  date30d <- fecha_ref %m-% months(1)
  date30d <- bizdays::adjust.next(date30d, calendario)

  # Último día hábil del año anterior
  end_of_last_year <- as.Date(paste0(lubridate::year(fecha_ref) - 1, "-12-31"))
  dateYTD <- bizdays::adjust.previous(end_of_last_year, calendario)

  return(list(
    date1d = date1d,
    date5d = date5d,
    date30d = date30d,
    dateYTD = dateYTD,
    fecha_actual = fecha_ref
  ))
}

# -----------------------------
# Función auxiliar: asof_value (similar a evol_tickers_ajustados_FX.r)
# -----------------------------
asof_value <- function(df, cutoff_date) {
  x <- df %>%
    filter(date <= cutoff_date, !is.na(value)) %>%
    arrange(date)
  if (nrow(x) == 0) return(list(date = as.Date(NA), value = NA_real_))
  list(date = x$date[nrow(x)], value = as.numeric(x$value[nrow(x)]))
}

# -----------------------------
# Fase 2: Función para calcular retornos por ticker
# -----------------------------
calcular_retornos_ticker <- function(ticker, datos, fechas_ref, fecha_actual) {
  require(dplyr)

  # Filtrar datos del ticker
  datos_ticker <- datos %>%
    filter(ticker == .env$ticker) %>%
    select(date, value = price) %>%
    arrange(date)

  if (nrow(datos_ticker) == 0) {
    return(data.frame(
      ticker = ticker,
      precio = NA_real_,
      `1d` = NA_real_,
      `5d` = NA_real_,
      `1m` = NA_real_,
      YTD = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  # Obtener precios en las fechas de referencia
  precio_actual <- asof_value(datos_ticker, fecha_actual)$value
  precio_1d <- asof_value(datos_ticker, fechas_ref$date1d)$value
  precio_5d <- asof_value(datos_ticker, fechas_ref$date5d)$value
  precio_1m <- asof_value(datos_ticker, fechas_ref$date30d)$value
  precio_YTD <- asof_value(datos_ticker, fechas_ref$dateYTD)$value

  # Calcular retornos
  ret_1d <- if (!is.na(precio_1d) && precio_1d != 0) {
    (precio_actual - precio_1d) / precio_1d
  } else {
    NA_real_
  }

  ret_5d <- if (!is.na(precio_5d) && precio_5d != 0) {
    (precio_actual - precio_5d) / precio_5d
  } else {
    NA_real_
  }

  ret_1m <- if (!is.na(precio_1m) && precio_1m != 0) {
    (precio_actual - precio_1m) / precio_1m
  } else {
    NA_real_
  }

  ret_YTD <- if (!is.na(precio_YTD) && precio_YTD != 0) {
    (precio_actual - precio_YTD) / precio_YTD
  } else {
    NA_real_
  }

  # Usar tibble para preservar nombres no estándar correctamente
  require(tibble)
  resultado <- tibble(
    ticker = ticker,
    precio = precio_actual,
    `1d` = ret_1d,
    `5d` = ret_5d,
    `1m` = ret_1m,
    YTD = ret_YTD
  )

  return(resultado)
}

# -----------------------------
# Fase 3: Función principal de cálculo
# -----------------------------
calcular_panel_variaciones <- function(datos, calendarios, grupos, fecha_referencia, cal_fallback = "cal", nombres_display = NULL) {
  require(dplyr)
  require(purrr)

  # Validar inputs
  if (!all(c("date", "ticker", "price") %in% colnames(datos))) {
    stop("datos debe contener las columnas: date, ticker, price")
  }

  # Convertir calendarios a named vector si es dataframe
  if (is.data.frame(calendarios)) {
    if (!all(c("ticker", "calendario") %in% colnames(calendarios))) {
      stop("Si calendarios es dataframe, debe tener columnas: ticker, calendario")
    }
    calendarios_vec <- setNames(calendarios$calendario, calendarios$ticker)
  } else {
    calendarios_vec <- calendarios
  }

  # Convertir grupos a named vector si es dataframe
  if (is.null(grupos)) {
    grupos_vec <- setNames(rep("OTROS", length(unique(datos$ticker))), unique(datos$ticker))
  } else if (is.data.frame(grupos)) {
    if (!all(c("ticker", "grupo") %in% colnames(grupos))) {
      stop("Si grupos es dataframe, debe tener columnas: ticker, grupo")
    }
    grupos_vec <- setNames(grupos$grupo, grupos$ticker)
  } else {
    grupos_vec <- grupos
  }

  # Convertir nombres_display a named vector si es dataframe
  if (is.null(nombres_display)) {
    nombres_display_vec <- NULL
  } else if (is.data.frame(nombres_display)) {
    if (!all(c("ticker", "nombre") %in% colnames(nombres_display))) {
      stop("Si nombres_display es dataframe, debe tener columnas: ticker, nombre")
    }
    nombres_display_vec <- setNames(nombres_display$nombre, nombres_display$ticker)
  } else {
    nombres_display_vec <- nombres_display
  }

  # Obtener lista única de tickers
  tickers_unicos <- unique(datos$ticker)

  # Asegurar que todos los tickers tengan grupo asignado
  grupos_completos <- grupos_vec
  tickers_sin_grupo <- setdiff(tickers_unicos, names(grupos_completos))
  if (length(tickers_sin_grupo) > 0) {
    grupos_completos[tickers_sin_grupo] <- "OTROS"
  }

  # Preparar nombres de visualización (si no hay, usar ticker)
  nombres_display_completos <- if (is.null(nombres_display_vec)) {
    setNames(tickers_unicos, tickers_unicos)
  } else {
    nombres_display_vec
  }
  tickers_sin_nombre <- setdiff(tickers_unicos, names(nombres_display_completos))
  if (length(tickers_sin_nombre) > 0) {
    nombres_display_completos[tickers_sin_nombre] <- tickers_sin_nombre
  }

  # Calcular retornos para cada ticker
  resultados <- map_dfr(tickers_unicos, function(tick) {
    # Obtener calendario para este ticker
    cal_ticker <- if (tick %in% names(calendarios_vec)) {
      calendarios_vec[[tick]]
    } else {
      cal_fallback
    }

    # Calcular fechas de referencia
    fechas_ref <- calcular_fechas_referencia(cal_ticker, fecha_referencia, cal_fallback)

    # Calcular retornos
    retornos <- calcular_retornos_ticker(tick, datos, fechas_ref, fecha_referencia)

    # Agregar grupo
    retornos$grupo <- grupos_completos[[tick]]

    # Agregar nombre de visualización
    retornos$ticker_display <- nombres_display_completos[[tick]]

    return(retornos)
  })

  # Ordenar por grupo y luego por ticker
  # Verificar que las columnas existan antes de seleccionarlas
  columnas_esperadas <- c("ticker", "ticker_display", "grupo", "precio", "1d", "5d", "1m", "YTD")
  columnas_existentes <- intersect(columnas_esperadas, colnames(resultados))

  if (length(columnas_existentes) != length(columnas_esperadas)) {
    columnas_faltantes <- setdiff(columnas_esperadas, columnas_existentes)
    warning(paste("Columnas faltantes:", paste(columnas_faltantes, collapse = ", ")))
  }

  resultados <- resultados %>%
    arrange(grupo, ticker) %>%
    select(all_of(columnas_existentes))

  return(resultados)
}

# -----------------------------
# Fase 4: Formateo y creación de flextable
# -----------------------------
formatear_panel_flextable <- function(panel_df, titulo = "Panel de Variaciones", nota_pie = NULL) {
  require(dplyr)
  require(flextable)

  # Crear columnas formateadas
  panel_formateado <- panel_df %>%
    mutate(
      # Formatear precio a 2 decimales
      precio_formatted = sprintf("%.2f", precio),
      # Formatear retornos a porcentajes
      across(c(`1d`, `5d`, `1m`, YTD),
             ~sprintf("%0.2f%%", . * 100),
             .names = "{.col}_formatted")
    ) %>%
    mutate(
      across(c(`1d`, `5d`, `1m`, YTD),
             ~ifelse(. < 0, "red", "darkgreen"),
             .names = "{.col}_color")
    )

  # Usar ticker_display si existe, sino usar ticker
  columna_ticker <- if ("ticker_display" %in% colnames(panel_formateado)) {
    "ticker_display"
  } else {
    "ticker"
  }

  # Crear flextable
  ft <- flextable(panel_formateado,
                  col_keys = c(columna_ticker, "precio_formatted", "1d_formatted", "5d_formatted", "1m_formatted", "YTD_formatted")) %>%
    # Renombrar encabezados
    set_header_labels(
      ticker_display = "Ticker",
      ticker = "Ticker",
      precio_formatted = "Último",
      `1d_formatted` = "1D",
      `5d_formatted` = "5D",
      `1m_formatted` = "1M",
      `YTD_formatted` = "YTD"
    ) %>%
    # Negrita en ticker
    bold(j = columna_ticker, part = "body") %>%
    bold(j = columna_ticker, part = "header") %>%
    bold(j = c(columna_ticker, "precio_formatted", "1d_formatted", "5d_formatted", "1m_formatted", "YTD_formatted"), part = "header") %>%
    # Colores condicionales
    color(j = "1d_formatted", color = panel_formateado$`1d_color`) %>%
    color(j = "5d_formatted", color = panel_formateado$`5d_color`) %>%
    color(j = "1m_formatted", color = panel_formateado$`1m_color`) %>%
    color(j = "YTD_formatted", color = panel_formateado$`YTD_color`) %>%
    # Bordes
    vline(j = c(columna_ticker, "precio_formatted"), border = fp_border_default(color = "black")) %>%
    # Alineación
    align(align = "center", part = "all") %>%
    # Propiedades de tabla
    set_table_properties(layout = "autofit", width = 0.8)

  # Agregar título si se proporciona
  if (!is.null(titulo)) {
    ft <- ft %>% add_header_lines(titulo)
  }

  # Agregar nota al pie si se proporciona
  if (!is.null(nota_pie)) {
    ft <- ft %>%
      add_footer_lines(nota_pie) %>%
      align(align = "right", part = "footer")
  }

  return(ft)
}

# -----------------------------
# Fase 5: División en múltiples paneles respetando grupos
# -----------------------------
dividir_en_paneles <- function(panel_df, max_tickers = 15) {
  require(dplyr)

  # Si hay menos o igual a max_tickers, retornar lista con un solo elemento
  if (nrow(panel_df) <= max_tickers) {
    return(list(panel_df))
  }

  # Agrupar por grupo
  grupos_unicos <- unique(panel_df$grupo)
  paneles <- list()
  panel_actual <- panel_df[0, ]  # Dataframe vacío con la misma estructura

  for (grupo in grupos_unicos) {
    # Obtener tickers de este grupo
    tickers_grupo <- panel_df %>% filter(grupo == .env$grupo)

    # Si el grupo completo cabe en el panel actual
    if (nrow(panel_actual) + nrow(tickers_grupo) <= max_tickers) {
      panel_actual <- bind_rows(panel_actual, tickers_grupo)
    } else {
      # Si hay algo en panel_actual, guardarlo
      if (nrow(panel_actual) > 0) {
        paneles[[length(paneles) + 1]] <- panel_actual
      }

      # Si el grupo es muy grande, dividirlo
      if (nrow(tickers_grupo) > max_tickers) {
        n_chunks <- ceiling(nrow(tickers_grupo) / max_tickers)
        for (i in 1:n_chunks) {
          inicio <- (i - 1) * max_tickers + 1
          fin <- min(i * max_tickers, nrow(tickers_grupo))
          paneles[[length(paneles) + 1]] <- tickers_grupo[inicio:fin, ]
        }
        panel_actual <- panel_df[0, ]  # Dataframe vacío con la misma estructura
      } else {
        # El grupo cabe en un panel nuevo
        panel_actual <- tickers_grupo
      }
    }
  }

  # Agregar el último panel si tiene contenido
  if (nrow(panel_actual) > 0) {
    paneles[[length(paneles) + 1]] <- panel_actual
  }

  return(paneles)
}

# -----------------------------
# Fase 6: Función principal unificada
# -----------------------------
panel_variaciones_generico <- function(
  datos,
  calendarios,
  grupos = NULL,
  nombres_display = NULL,
  fecha_referencia = Sys.Date(),
  max_tickers_por_panel = 15,
  titulo = "Panel de Variaciones",
  nota_pie = NULL,
  cal_fallback = "cal",
  server = NULL,
  port = NULL
) {
  # Usar valores por defecto si no se proporcionan
  # Primero verificar si existen variables globales (establecidas por setup())
  if (is.null(server) || is.null(port)) {
    # Si alguna variable global no existe, llamar a setup() para establecerlas
    if (!exists("server", envir = .GlobalEnv) || !exists("port", envir = .GlobalEnv)) {
      if (exists("setup", envir = asNamespace("functions"))) {
        functions::setup()
      }
    }

    # Usar las variables globales (ya sea existentes o recién establecidas)
    if (is.null(server)) {
      if (exists("server", envir = .GlobalEnv)) {
        server <- get("server", envir = .GlobalEnv)
      } else {
        server <- "local"  # Fallback si setup() no está disponible
      }
    }

    if (is.null(port)) {
      if (exists("port", envir = .GlobalEnv)) {
        port <- get("port", envir = .GlobalEnv)
      } else {
        port <- 5432  # Fallback si setup() no está disponible
      }
    }
  }
  require(dplyr)
  require(bizdays)
  require(lubridate)
  require(flextable)

  # 1. Validar parámetros de entrada
  if (!is.data.frame(datos)) {
    stop("datos debe ser un dataframe")
  }

  if (is.null(calendarios)) {
    stop("calendarios es requerido (puede ser named vector o dataframe)")
  }

  # 2. Crear calendario de fallback si no existe
  cal_fallback_nombre <- crear_calendario_fallback(cal_fallback, server, port)

  # 3. Preparar mapeo de grupos (se hace dentro de calcular_panel_variaciones)

  # 4. Calcular panel con grupos
  panel_calculado <- calcular_panel_variaciones(
    datos = datos,
    calendarios = calendarios,
    grupos = grupos,
    fecha_referencia = fecha_referencia,
    cal_fallback = cal_fallback_nombre,
    nombres_display = nombres_display
  )

  # 5. Dividir en paneles si es necesario (respetando grupos)
  paneles_divididos <- dividir_en_paneles(panel_calculado, max_tickers_por_panel)

  # 6. Crear flextables para cada panel
  flextables <- map(paneles_divididos, function(panel) {
    formatear_panel_flextable(panel, titulo = titulo, nota_pie = nota_pie)
  })

  # 7. Retornar lista de flextables (o uno solo si hay solo uno)
  if (length(flextables) == 1) {
    return(flextables[[1]])
  } else {
    return(flextables)
  }
}

