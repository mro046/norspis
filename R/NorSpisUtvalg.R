#' Funksjon som gjør utvalg av dataene, returnerer det filtrerte datasettet og utvalgsteksten.
#'
#' @inheritParams NorSpisFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Standard: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet (RegData), utvalgstekst for 
#' figur (UtvalgTxt), fargepalett, indekser for hovedgruppe og sammenlikningsgruppe (ind), 
#' tekststreng som angir fargepalett,  om vi har sammenlikning eller ikke (medSml), tekst som angir
#' hovedgruppa (hovedgrTxt) og gruppa det evt. sammenliknes mot (smltxt)
#'
#' @export
                    
NorSpisUtvalg <- function(RegData, datoFra, datoTil, aar=0, minald=0, maxald=130, erMann='', 
					enhetsUtvalg=0, reshID=0, fargepalett='BlaaOff')  #grType=99, 
{
      
#Legge inn feilmelding hvis reshID=0 og enhetsUtvalg ulik 0.      
      
# Definer intersect-operator
 "%i%" <- intersect

      #Enhetsutvalg:
      #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne, 
      #trengs ikke data for hele landet:
      indEgen1 <- match(reshID, RegData$ReshId)
      if (enhetsUtvalg %in% c(2,3,4,6,7)) {	
            RegData <- switch(as.character(enhetsUtvalg),
                              '2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
                              '3' = subset(RegData,EnhType==EnhType[indEgen1]),
                              '4' = RegData[which(RegData$EnhType == RegData$EnhType[indEgen1]),],	#kun egen shgruppe
                              '6' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),],	#sml region
                              '7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
      }
 
RegData$EnhNavn <- as.factor(RegData$EnhNavn) #gjøres her om til faktor for å forsikre oss om at også tomme sykehus kommer med, e.g. i gjsngrvar
Ninn <- dim(RegData)[1]
indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
indDato <- which(RegData$HovedDato >= as.POSIXlt(datoFra) & RegData$HovedDato <= as.POSIXlt(datoTil))
indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}

indMed <- indAld %i% indDato %i% indKj 

RegData <- RegData[indMed,]


N <- dim(RegData)[1]	#N=0 gir feilmelding

utvalgTxt <- c(paste0(
	'Registreringsperiode: ', if (N>0) {min(RegData$HovedDato, na.rm=T)} else {datoFra}, 
			' til ', if (N>0) {max(RegData$HovedDato, na.rm=T)} else {datoTil}),
	if ((minald>0) | (maxald<130)) {
		paste0('Pasienter fra ', if (N>0) {sprintf('%.1f',min(RegData$Alder, na.rm=T))} else {minald}, 
						' til ', if (N>0) {sprintf('%.1f',max(RegData$Alder, na.rm=T))} else {maxald}, ' år')},
	if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])}
)

      #Enhetsutvalg:
      indEgen1 <- match(reshID, RegData$ReshId)
      if (enhetsUtvalg %in% c(1,2,3,6)) {	#Involverer egen enhet
            hovedgrTxt <- as.character(RegData$EnhNavn[indEgen1]) } else {
                  hovedgrTxt <- switch(as.character(enhetsUtvalg), 	
                                  '0' = 'Hele landet',
                                  '4' = grTypetextstreng[RegData$EnhType[indEgen1]],
                                  '5' = grTypetextstreng[RegData$EnhType[indEgen1]],
                                  '7' = as.character(RegData$Region[indEgen1]),
                                  '8' = as.character(RegData$Region[indEgen1]))
            }
      
      
      ind <- list(Hoved=0, Rest=0)
      smltxt <- ''
      if (enhetsUtvalg %in% c(0,2,4,7)) {		#Ikke sammenlikning
            medSml <- 0
            ind$Hoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
            ind$Rest <- NULL
      } else {						#Skal gjøre sammenlikning
            medSml <- 1
            if (enhetsUtvalg %in% c(1,3,6)) {	#Involverer egen enhet
                  ind$Hoved <-which(RegData$ReshId==reshID) } else {
                        ind$Hoved <- switch(as.character(enhetsUtvalg),
                                            '5' = which(RegData$EnhType == RegData$EnhType[indEgen1]),	#shgr
                                            '8' = which(RegData$Region == RegData$Region[indEgen1]))}	#region
            smltxt <- switch(as.character(enhetsUtvalg),
                             '1' = 'landet forøvrig',
                             '3' = paste0('andre ', grTypetextstreng[RegData$EnhType[indEgen1]]),	#RegData inneh. kun egen shgruppe
                             '5' = 'andre typer sykehus',
                             '6' = paste0(RegData$Region[indEgen1], ' forøvrig'),	#RegData inneh. kun egen region
                             '8' = 'andre regioner')
            ind$Rest <- switch(as.character(enhetsUtvalg),
                               '1' = which(RegData$ReshId != reshID),
                               '3' = which(RegData$ReshId != reshID),	#RegData inneh. kun egen shgruppe
                               '5' = which(RegData$EnhType != RegData$EnhType[indEgen1]),
                               '6' = which(RegData$ReshId !=reshID),	#RegData inneh. kun egen region
                               '8' = which(RegData$Region != RegData$Region[indEgen1]))
      }							
      


      
UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett, ind=ind, 
                     medSml=medSml, hovedgrTxt=hovedgrTxt,smltxt=smltxt) 

#      ind <- NorSpisUtvalg$ind
#      medSml <- NorSpisUtvalg$medSml
      
					 
return(invisible(UtData)) 
}