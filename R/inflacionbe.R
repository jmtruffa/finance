#' @title Calcula la inflación BE
#' @description Calcula la inflación BE a una fecha puntual. Para ejecutar un rago, deberá ser llamada cada vez y concatenar.
#' @param fecha Fecha de cálculo
#' @param ... Argumentos adicionales como server y port
#' @return Dataframe con la inflación BE
#' @examples
#' inflacionbe("2021-01-01")
#' inflacionbe("2021-01-01", server = "medina", port = 14493)
#' @export
#' @import tidyverse
#' @import finance
#' @import methodsPPI
#' @import zoo
#' @importFrom outliers outlier
#' @importFrom lubridate year month day
#' @importFrom bizdays create.calendar add.bizdays
#' @importFrom stringr str_detect
inflacionbe = function(fecha, ...) {
  require(tidyverse)
  require(finance)
  require(methodsPPI)
  tmpCalendar <- create.calendar('tmpCalendar', getFeriados(), weekdays = c('saturday','sunday'))
  fecha = zoo::as.Date(fecha)
  
  # LECAPS
  lecaps = getLecaps(...) %>% # viejo era getLecaps(fecha, ...)
    mutate(
      type = ifelse(str_detect(ticker, "S"), "LETRAS", "BONOS")
    )
  
  curva_lecaps = methodsPPI::getPPIPrices(
    token = methodsPPI::getPPILogin2()$token, 
    ticker = lecaps$ticker, 
    type = lecaps$type,
    from = fecha, 
    to = fecha, 
    settlement = "A-24HS",
    # poner luego elipsis para server y port y sacar eso.
    server = server,
    port = port)
  print(paste0("Fallaron: ", curva_lecaps[[2]]$ticker))
  curva_lecaps = tasasLecap(curva_lecaps[[1]], ...)
  
  lecap = curva_lecaps %>% select(date, ticker, tir = tea, duration = mduration)
  
  # CER
  tickersBonosCER = map_dfr(.x = "bonosCER", .f = methodsPPI::sets)
  resultBonosCER = getPPIPrices(token = methodsPPI::getPPILogin2()$token,
                                               ticker = tickersBonosCER$ticker, 
                                               type = tickersBonosCER$type, 
                                               from = fecha, 
                                               to = fecha, 
                                               settlement = "A-24HS")
  resultBonosCER[[2]]
  resultBonosCER = resultBonosCER[[1]]
  
  aprBonosCER = getYields(resultBonosCER$ticker,
                          settlementDate = as.character(bizdays::offset(resultBonosCER$date, 1, cal = tmpCalendar)),
                          precios = resultBonosCER$price,
                          initialFee = 0,
                          endpoint = 'yield')
  bonosCER = cbind(resultBonosCER, aprBonosCER)
  cer = bonosCER %>% select(date, ticker, tir = yield, duration = mduration)
  
  # Settle date as next working day
  print(paste0("Fecha: ", fecha))
  settle <- add.bizdays(fecha, 1, cal = cal)
  print(paste0("Settle:", settle))
  
  # Load venc data
  venc = dbGetTable("vencTitulos", server = server, port = port)
  
  # Adjust CER data
  cer = cer %>% filter(date == fecha) %>% drop_na(tir)
  cerAdjusted = cer[cer$tir != outliers::outlier(cer$tir), ] %>% left_join(venc) %>% mutate(diasVto = as.numeric(vto - as.Date(settle)), diasVto2 = diasVto^2)
  
  # Adjust lecap data
  lecap = lecap %>% filter(date == fecha) %>% drop_na(tir)
  lecapAdjusted = lecap[lecap$tir != outliers::outlier(lecap$tir), ] %>% left_join(venc) %>% mutate(diasVto = as.numeric(vto - as.Date(settle)), diasVto2 = diasVto^2)
  
  # Define model
  # plazo máximo que tomará de los bonos CER para calcular el modelo
  plazoMax = max(lecapAdjusted$diasVto) + round(365 * 1.5, digits = 0)
  cerAdjusted = cerAdjusted %>% filter(diasVto <= plazoMax)
  modelCER = lm(tir ~ diasVto + diasVto2, data = cerAdjusted)
  modeloLecap = lm(tir ~ diasVto + diasVto2, data = lecapAdjusted)
  
  # Generate sequence of dates
  anio_final = year(max(lecapAdjusted$vto))
  mes_final = month(max(lecapAdjusted$vto))
  dia_actual = day(settle)
  
  if (dia_actual < 15) {
    fecha_inicio = as.Date(sprintf("%04d-%02d-15", year(settle), month(settle)))
  } else {
    fecha_inicio = as.Date(sprintf("%04d-%02d-15", year(settle), month(settle) + 1))
  }
  
  fecha_final = as.Date(sprintf("%04d-%02d-15", anio_final, mes_final))
  fechas_tasa_nominal = seq.Date(from = fecha_inicio, to = fecha_final, by = "month")
  fechas_tasa_real = add.bizdays(fechas_tasa_nominal, 10, cal = cal)
  dias_hasta_nominal = as.numeric(fechas_tasa_nominal - as.Date(settle))
  dias_hasta_real = as.numeric(fechas_tasa_real - as.Date(settle))
  tasa_nominal = predict(modeloLecap, newdata = tibble(diasVto = dias_hasta_nominal, diasVto2 = dias_hasta_nominal^2))
  tasa_real = predict(modelCER, newdata = tibble(diasVto = dias_hasta_real, diasVto2 = dias_hasta_real^2))
  BE_inflation_anual <- (1 + tasa_nominal) / (1 + tasa_real) - 1
  primer_mes = dias_hasta_nominal[1]
  resto = as.numeric(diff(fechas_tasa_nominal))
  dias_BE_inflation_mensual <- c(primer_mes, resto)
  BE_inflation_mensual = c(
    ((1 + BE_inflation_anual[1]) ^ (primer_mes / 365)) ^ (30 / primer_mes) - 1,
    (1 + BE_inflation_anual[2:length(BE_inflation_anual)]) ^ (resto / 365) - 1
  )
  
  be_inflation <- tibble(
    fecha = as.Date(fecha), # Add the fecha column
    fechas_tasa_nominal,
    fechas_tasa_real,
    dias_hasta_nominal,
    dias_hasta_real,
    tasa_nominal,
    tasa_real,
    BE_inflation_anual,
    dias_BE_inflation_mensual,
    BE_inflation_mensual
  )
  return(be_inflation)
}




