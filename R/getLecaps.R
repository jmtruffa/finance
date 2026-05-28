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
  query = paste0("select bt.ticker as ticker, bd.issue_date as date_liq, bd.maturity as date_vto, bd.cap_rate as tasa, bd.cap_final_value as vf
from bonds_db bd join bonds_tickers bt on bd.id = bt.bond_id
where tipo_instrumento = 'LECAP'
")
  functions::dbExecuteQuery(query = query, ...)
}

