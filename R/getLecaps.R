#' @title getLecaps
#' @description Devuelve las Lecaps
#'
#' @return Vector con los tickers de las Lecaps vigentes y resto de info.
#' @examples
#' getLecaps()
#' @export
#' @importFrom functions dbExecuteQuery
#'
getLecaps = function(...) {
  require(functions)
  #query = paste0("SELECT ticker FROM lecaps WHERE date_vto >= '",date, "' AND date_liq <= '",date, "'")
  query = paste0("SELECT * FROM lecaps")
  functions::dbExecuteQuery(query = query, ...)
}

