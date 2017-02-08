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
NorSpisRegDataSQL <- function(datoFra = '2015-01-01', datoTil = '2099-01-01') {
  
  registryName <- "norspis"
  dbType <- "mysql"
  
  query <- paste0('SELECT
f.AvdRESH,
f.erMann,
f.PasientAlder,
f.ForlopsID,
el.ForlopsID,
f.HovedDato,
f.SykehusNavn
	
FROM
	ForlopsOversikt f 
LEFT JOIN EnkeltLeddNum el ON f.ForlopsID = el.ForlopsID')
  
#  FROM AlleVarNum INNER JOIN ForlopsOversikt ON AlleVarNum.MCEID = ForlopsOversikt.ForlopsID
#  WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')

  
  
# WHERE DateAdmittedIntensive >= \'', datoFra, '\' AND DateAdmittedIntensive <= \'', datoTil, '\'')

  merge(NorSpisForlop, by = c("ForlopsID" ), NorSpisEnkeltledd, all = FALSE)  #by.x = "ForlopsID", by.y = "ForlopsID",
  
  
  RegData <- rapbase::LoadRegData(registryName, query, dbType)
  
  return(RegData)
}