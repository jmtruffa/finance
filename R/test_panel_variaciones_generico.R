# ============================================================================
# Script de prueba para panel_variaciones_generico
# ============================================================================
# Este script crea datos mock y prueba la función panel_variaciones_generico
# ============================================================================

# Cargar librerías necesarias
library(dplyr)
library(lubridate)
library(bizdays)

# -----------------------------
# Crear datos mock
# -----------------------------

# Fechas: últimos 2 años de datos diarios
fechas <- seq(from = as.Date("2023-01-01"), 
              to = Sys.Date(), 
              by = "day")

# Tickers de ejemplo (mezcla de ADRs, acciones locales, índices)
tickers <- c("GGAL", "PAM", "BMA", "AAPL", "MSFT", "KO", "^GSPC", "^NDX")

# Crear dataframe con datos de precios simulados
set.seed(123)  # Para reproducibilidad
datos <- expand.grid(
  date = fechas,
  ticker = tickers,
  stringsAsFactors = FALSE
) %>%
  # Simular precios con tendencia y ruido
  group_by(ticker) %>%
  mutate(
    # Precio base diferente para cada ticker
    base_price = case_when(
      ticker == "GGAL" ~ 10,
      ticker == "PAM" ~ 25,
      ticker == "BMA" ~ 5,
      ticker == "AAPL" ~ 150,
      ticker == "MSFT" ~ 300,
      ticker == "KO" ~ 60,
      ticker == "^GSPC" ~ 4000,
      ticker == "^NDX" ~ 15000,
      TRUE ~ 100
    ),
    # Tendencia temporal con ruido
    trend = seq_along(date) * 0.001,
    noise = rnorm(n(), mean = 0, sd = 0.02),
    price = base_price * (1 + trend) * (1 + noise)
  ) %>%
  ungroup() %>%
  select(date, ticker, price) %>%
  arrange(ticker, date)

# -----------------------------
# Crear calendarios de prueba
# -----------------------------

# Crear algunos calendarios de ejemplo
# Nota: En producción, estos calendarios deberían existir previamente

# Calendario USA (ejemplo simplificado)
# Verificar si el calendario existe intentando usarlo
cal_usa_existe <- tryCatch({
  bizdays::offset(as.Date("2024-01-01"), 0, "cal_usa_test")
  TRUE
}, error = function(e) FALSE)

if (!cal_usa_existe) {
  # Crear calendario USA con algunos feriados conocidos
  feriados_usa <- as.Date(c(
    "2023-01-01", "2023-01-16", "2023-02-20", "2023-04-07", 
    "2023-05-29", "2023-06-19", "2023-07-04", "2023-09-04",
    "2023-10-09", "2023-11-11", "2023-11-23", "2023-12-25",
    "2024-01-01", "2024-01-15", "2024-02-19", "2024-03-29",
    "2024-05-27", "2024-06-19", "2024-07-04", "2024-09-02",
    "2024-10-14", "2024-11-11", "2024-11-28", "2024-12-25"
  ))
  
  bizdays::create.calendar(
    name = "cal_usa_test",
    holidays = feriados_usa,
    weekdays = c("saturday", "sunday")
  )
}

# Calendario Argentina (se usará el fallback "cal" si no existe)
# El fallback se creará automáticamente usando functions::getFeriados()

# Mapeo de calendarios por ticker
calendarios <- c(
  "GGAL" = "cal",           # Argentina (usará fallback)
  "PAM" = "cal",            # Argentina (usará fallback)
  "BMA" = "cal",            # Argentina (usará fallback)
  "AAPL" = "cal_usa_test",  # USA
  "MSFT" = "cal_usa_test",  # USA
  "KO" = "cal_usa_test",    # USA
  "^GSPC" = "cal_usa_test", # USA
  "^NDX" = "cal_usa_test"   # USA
)

# -----------------------------
# Crear grupos de tickers
# -----------------------------

grupos <- c(
  "GGAL" = "ADR",
  "PAM" = "ADR",
  "BMA" = "ADR",
  "AAPL" = "INTERNACIONALES",
  "MSFT" = "INTERNACIONALES",
  "KO" = "INTERNACIONALES",
  "^GSPC" = "INDICES",
  "^NDX" = "INDICES"
)

# -----------------------------
# Crear nombres de visualización
# -----------------------------

nombres_display <- c(
  "^GSPC" = "SPX",
  "^NDX" = "NDX"
  # Los demás usarán su ticker original
)

# -----------------------------
# Probar la función
# -----------------------------

# Opción 1: Llamada básica (usará setup() si no existen variables globales)
resultado <- panel_variaciones_generico(
  datos = datos,
  calendarios = calendarios,
  grupos = grupos,
  nombres_display = nombres_display,
  fecha_referencia = Sys.Date() - 1,
  max_tickers_por_panel = 15,
  titulo = "Panel de Variaciones - Prueba",
  nota_pie = "Datos mock para prueba"
)

# Ver resultado
print("Resultado de la función:")
print(resultado)

# Si es una lista (múltiples paneles), mostrar cada uno
if (is.list(resultado) && !inherits(resultado, "flextable")) {
  cat("\nNúmero de paneles generados:", length(resultado), "\n\n")
  for (i in seq_along(resultado)) {
    cat("=== Panel", i, "===\n")
    print(resultado[[i]])
    cat("\n")
  }
}

# -----------------------------
# Prueba con más tickers (para probar división en paneles)
# -----------------------------

# Agregar más tickers para forzar división en paneles
tickers_extendidos <- c(tickers, 
                        "YPF", "TEO", "EDN", "LOMA", "TX", 
                        "TSLA", "GOOGL", "AMZN", "META", "NVDA")

# Crear datos extendidos
datos_extendidos <- expand.grid(
  date = fechas,
  ticker = tickers_extendidos,
  stringsAsFactors = FALSE
) %>%
  group_by(ticker) %>%
  mutate(
    base_price = runif(1, 10, 200),
    trend = seq_along(date) * 0.001,
    noise = rnorm(n(), mean = 0, sd = 0.02),
    price = base_price * (1 + trend) * (1 + noise)
  ) %>%
  ungroup() %>%
  select(date, ticker, price) %>%
  arrange(ticker, date)

# Calendarios extendidos (todos usan fallback para simplificar)
calendarios_extendidos <- setNames(
  rep("cal", length(tickers_extendidos)),
  tickers_extendidos
)

# Grupos extendidos
grupos_extendidos <- c(
  grupos,
  "YPF" = "ADR",
  "TEO" = "ADR",
  "EDN" = "ADR",
  "LOMA" = "ADR",
  "TX" = "ADR",
  "TSLA" = "INTERNACIONALES",
  "GOOGL" = "INTERNACIONALES",
  "AMZN" = "INTERNACIONALES",
  "META" = "INTERNACIONALES",
  "NVDA" = "INTERNACIONALES"
)

# Probar con más tickers (debería generar múltiples paneles)
cat("\n\n=== Prueba con más tickers (debería dividir en paneles) ===\n")
resultado_extendido <- panel_variaciones_generico(
  datos = datos_extendidos,
  calendarios = calendarios_extendidos,
  grupos = grupos_extendidos,
  fecha_referencia = Sys.Date(),
  max_tickers_por_panel = 5,  # Reducido para forzar división
  titulo = "Panel de Variaciones - Prueba Extendida",
  nota_pie = "Prueba con múltiples paneles"
)

if (is.list(resultado_extendido) && !inherits(resultado_extendido, "flextable")) {
  cat("Número de paneles generados:", length(resultado_extendido), "\n")
}

# -----------------------------
# Prueba sin grupos (debería asignar grupo "OTROS")
# -----------------------------

cat("\n\n=== Prueba sin especificar grupos ===\n")
resultado_sin_grupos <- panel_variaciones_generico(
  datos = datos,
  calendarios = calendarios,
  grupos = NULL,  # Sin grupos
  fecha_referencia = Sys.Date(),
  titulo = "Panel sin grupos especificados",
  nota_pie = "Todos putos"
)

print(resultado_sin_grupos)

