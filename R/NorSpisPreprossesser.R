#' Preprosesser data fra NorSpis
#'
#' Denne funksjonen navner om variabler og beregner evt. nye.
#'
#' @inheritParams NorSpisFigAndeler
#'
#' @return Data En liste med det filtrerte datasettet (og sykehusnavnet som tilsvarer reshID, ikke pt)
#'
#' @export
#'
NorSpisPreprosess <- function(RegData=RegData)	#, reshID=reshID)
{
  #Kun ferdigstilte registreringer:
  # Rapporteket får kun levert ferdigstilte registreringer fra MRS/NHN.

  #Kjønn
  RegData$ErMann <- NULL

  #Riktig navn på regions-variabel:
  #Mangler regionsvariabel!!!
#	RegData$Region <- RegData$RHF

# Endre variabelnavn:
	names(RegData)[which(names(RegData) == 'PasientAlder')] <- 'Alder'
	
# Riktig format
	RegData$ShNavn <- as.character(RegData$ShNavn)

	#Riktig format på datovariable:
	RegData$InnDato <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d") 
	#RegData$InnDato <- strptime(RegData$DateAdmittedIntensive, format="%Y-%m-%d") # %H:%M:%S" )  #"%d.%m.%Y"	"%Y-%m-%d"
	#RegData$Aar <- 1900 + strptime(RegData$DateAdmittedIntensive, format="%Y")$year
	
  return(invisible(RegData))
}

