#' Henter data registrert for NorSpis
#'
#' Henter data for NorSpisregisteret fra "staging"
#'
#' @inheritParams NorSpisFigAndeler
#'
#' @return Lager dataramma RegData for utvalgte data fra NorSpis
#' @export
#'
#'
NIRRegDataSQL <- function(datoFra = '2017-01-01', datoTil = '2099-01-01') {
  
  registryName <- "norspis"
  dbType <- "mysql"
  
  query <- paste0('SELECT
	f.Alder,
	f.TypeOfAdmission,
	el.UrineOutput
FROM
	ForlopsOversikt f 
LEFT JOIN EnkeltLeddNum el 
WHERE DateAdmittedIntensive >= \'', datoFra, '\' AND DateAdmittedIntensive <= \'', datoTil, '\'')
  
  
  RegData <- rapbase::LoadRegData(registryName, query, dbType)
  
  return(RegData)
}