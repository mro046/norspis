#' Hente data fra enkeltspørsmål (enkeltledd), NorSpis
#'
#' Henter data fra staging
#'
#' @inheritParams NorSpisFigAndeler
#'
#' @return RegData data frame
#' @export
#'
NSPHentRegData <- function(datoFra = '2015-01-01', datoTil = '2099-01-01') {

  registryName <- "norspis"
  dbType <- "mysql"

  query <- paste0('SELECT
	BirthDate,
	Education,
	MaritalStatus,
	MCEType,
	OpAnesthetic,
  ForlopsOversikt.AvdRESH,
	ForlopsOversikt.BasisRegStatus,
	ForlopsOversikt.HovedDato,
	ForlopsOversikt.OppflgRegStatus,
	ForlopsOversikt.OppflgStatus,
	ForlopsOversikt.SykehusNavn
FROM EnkeltLeddNum INNER JOIN ForlopsOversikt ON EnkeltLeddNum.MCEID = ForlopsOversikt.ForlopsID
                  WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')

RegData <- rapbase::LoadRegData(registryName, query, dbType)


return(RegData)
}



