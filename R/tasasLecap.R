#' @title Tasas Lecap
#' @description Calcula las tasas y duration de las lecap. Obtiene de la tabla "lecaps" los
#' vencimientos y valores finales de cada lecap de la base de datos. Calcula las tasas directas,
#' tna, tea, tem, tna360, tea360, tem360 y duration de cada lecap.
#' Necesita que el df tenga date, price, ticker
#' @param df data.frame con los datos de los bonos
#' @param settle fecha de liquidación de la operación. Puede ser "t+0" o "t+1" versus la fecha del precio
#' @param ... argumentos adicionales para dbGetTable
#' @return data.frame recibido con el agregado de todos los calculos realizados
#' @examples
#' tasasLecap(df, settle = "t+1")
#'
#'
tasasLecap = function(df, settle = "t+1", ...) {
  require(functions)
  require(bizdays)
  require(tidyverse)
  cal = create.calendar('cal', dbGetTable("calendarioFeriados", server = server, port = port)$date, weekdays = c('saturday','sunday'))
  settle = ifelse(settle == 't+0', 0, 1)
  datos = functions::dbGetTable(table = "lecaps", server = server, port = port)
  df= left_join(df, datos)
  df$settle = bizdays::add.bizdays(df$date, settle, cal = cal)
  df$dias360 = functions::days360(df$settle, df$date_vto)
  df$dias = as.numeric(df$date_vto - df$settle)
  df$tdirecta = (df$vf / df$price) - 1
  df$tna = df$tdirecta * 365 / df$dias
  df$tea = ((1 + df$tdirecta)^(365/df$dias)) - 1
  df$tem = ((1 + df$tdirecta)^(30/df$dias)) - 1
  df$tna360 = df$tdirecta * 360 / df$dias360
  df$tea360 = ((1 + df$tdirecta)^(365/df$dias360)) - 1
  df$tem360 = ((1 + df$tdirecta)^(30/df$dias360)) - 1
  df$duration = df$dias
  df$mduration = round(df$duration / (1 + df$tea),0)
  df = df %>% filter(dias360 !=0)
  return(df)
}
