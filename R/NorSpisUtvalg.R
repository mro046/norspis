#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams NIRFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Standard: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

NIRUtvalg <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='', InnMaate='', 
			fargepalett='BlaaOff')    
{

# Definer intersect-operator
 "%i%" <- intersect
      
 dodInt <- as.numeric(dodInt)
 

Ninn <- dim(RegData)[1]
indAld <- which(RegData$alder >= minald & RegData$alder <= maxald)
indDato <- which(RegData$HovedDato >= as.POSIXlt(datoFra) & RegData$HovedDato <= as.POSIXlt(datoTil))
indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
indInnMaate <- if (InnMaate %in% c(0,6,8)) {which(RegData$InnMaate == InnMaate)
				} else {indInnMaateUt <- 1:Ninn}

indMed <- indAld %i% indDato %i% indKj %i% indInnMaate 

RegData <- RegData[indMed,]


N <- dim(RegData)[1]	#N=0 gir feilmelding

utvalgTxt <- c(paste(
	'Registreringsperiode: ', if (N>0) {min(RegData$HovedDato, na.rm=T)} else {datoFra}, 
			' til ', if (N>0) {max(RegData$HovedDato, na.rm=T)} else {datoTil}, sep='' ),
	if ((minald>0) | (maxald<130)) {
		paste('Pasienter fra ', if (N>0) {sprintf('%.1f',min(RegData$alder, na.rm=T))} else {minald}, 
						' til ', if (N>0) {sprintf('%.1f',max(RegData$alder, na.rm=T))} else {maxald}, ' år', sep='')},
	if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
	if (InnMaate %in% c(0,6,8)) {paste('Innmåte: ', 
			c('Elektivt',0,0,0,0,0, 'Akutt medisinsk',0, 'Akutt kirurgi')[InnMaate+1], sep='')}
)


UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett)
return(invisible(UtData)) 
}