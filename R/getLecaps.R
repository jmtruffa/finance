#' @title getLecaps
#' @description Devuelve las Lecaps vigentes segun fecha provista
#' @param date Fecha en formato "YYYY-MM-DD". Por defecto la fecha actual.
#' @param ... Parametros de conexion a la base de datos
#' @return Vector con los tickers de las Lecaps vigentes
#' @examples
#' getLecaps("2024-08-10")
#' getLecaps()
#' @export
#' @importFrom functions dbExecuteQuery
#'
getLecaps = function(date = Sys.Date(), ...) {
  require(functions)
  query = paste0("SELECT ticker FROM lecaps WHERE date_vto >= '",date, "' AND date_liq <= '",date, "'")
  functions::dbExecuteQuery(query, ...)
}

