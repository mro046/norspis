#' Hente data fra enkeltspørsmål (enkeltledd), NorSpis
#'
#' Henter data fra staging
#'
#' @inheritParams NorSpisFigAndeler
#'
#' @return RegData data frame
#' @export
#'
NorSpisELAlleScorData <- function(datoFra = '2015-01-01', datoTil = '2099-01-01') {

  registryName <- "norspis"
  dbType <- "mysql"

  query <- paste0('SELECT  
      B17FysMishandl, 
	B18PsykMishandl, 
      B19Overgrep, 
      B20Mobbing,
      B01Sivilstatus,
      B04PabegyntUtd, 
      B05FullfortUtd,
      B06Hovedaktivitet,
      B07Hovedinntekt,
      B08StartAldrProbl,
      B11FamilieSF,
      B12cAldrForsteBeh,
      B12TidlBehSF,
      B17FysMishandl, 
      B18PsykMishandl, 
      B19Overgrep, 
      B20Mobbing, 
      B21SelvskadTidl, 
      B22SelvskadSisteAr, 
      B23SelvmordFTidl, 
      B24SelvmordFSisteAr, 
      B25Avhengighet,
      BehUtfallsvurdSamlet,
      BehVidereBeh,
      DiagVSF,
      ErMann,
      MedAnnenMedBeh,
      MedAntidepressiva, 
      MedBenzodiazepiner, 
      MedBMI,
      MedNevroleptika, 
      Norsktalende,
      PasientAlder,
      PO01Forstod,
      PO02Tillit,
      PO03InfoDiagnose,
      PO04Tilpasset,
      PO05Involvert,
      PO06Organisert,
      PO07Tilfredsstillende,
      PO08Tilgjengelighet,
      PO09Utbytte,
      PO10Pasientsikkerhet,
      PT01OnsketInvolv, 
      PT02BleInvolv, 
      PT03Utfallsvurd,
      PT04KontaktBrukerorg, 
      PT05OrientertBrukerorg,
      ForlopsOversikt.AvdRESH,
      ForlopsOversikt.BasisRegStatus,
      ForlopsOversikt.HovedDato,
      ForlopsOversikt.OppflgRegStatus,
      ForlopsOversikt.OppflgStatus,
      ForlopsOversikt.SykehusNavn
FROM EnkeltLeddNum 
INNER JOIN ForlopsOversikt 
ON EnkeltLeddNum.ForlopsID = ForlopsOversikt.ForlopsID
LEFT JOIN AlleScorer  
ON ForlopsOversikt.ForlopsID = AlleScorer.ForlopsID
WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')
				  
#  FROM AlleVarNum
#  INNER JOIN ForlopsOversikt
#  ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID
#  LEFT JOIN FollowupsNum
#  ON ForlopsOversikt.ForlopsID = FollowupsNum.ForlopsID
# WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')

				  
RegData <- rapbase::LoadRegData(registryName, query, dbType)


return(RegData)
}



