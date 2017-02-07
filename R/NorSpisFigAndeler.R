#' Søylediagram, horisontalt eller vertikalt, som viser andeler av valgt variabel.
#'
#' Søylediagrammet viser fordelinga til den valgte variabelen. Søylene er horisontale eller vertikale
#' avhengig av hvor stor plass kategorinavnet til søyla tar.
#'
#' #' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Aldersfordeling, 5-årige grupper
#'     \item BMI: Pasientenes BMI (Body Mass Index)
#'     \item Norsktalende: Snakker pasienten norsk
#'     \item Utdanning: Utdanningsnivå
#'    }
#'
#'
#' @param RegData Dataramme med alle nødvendige variable fra registeret
#' @param outfile Navn på fil figuren skrives ned til
#' @param reshID Avdelingsid (reshID) for egen avdeling,
#' @param hentData Angir om funksjonen skal kjøre spørring for å hente data eller ikke.
#'					0: ikke kjør (standard)
#'					1: kjør
#' @param preprosess Skal data preprosesseres, dvs. gjøre standard omregning av variable og beregne nye.
#'						TRUE (standard) / FALSE
#' @param datoFra Operasjonsdato, fra og med. Standard: '2012-01-01'
#' @param datoTil Operasjonsdato, til og med. Standard: '3000-01-01' (siste registreringsdato)
#' @param minald Alder, fra og med
#' @param maxald Alder, til og med
#' @param erMann Kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param enhetsUtvalg Sammenlikning eller ikke: 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.
#'
#' Detajer...:
#'
#' @return En figur med søylediagram (fordeling) av ønsket variabel
#'
#' @export

NorSpisFigAndeler  <- function(RegData, valgtVar, datoFra='2016-01-01', datoTil='3000-12-31',
		minald=0, maxald=130, erMann='', outfile='', hentData=0, preprosess=1,
		reshID, enhetsUtvalg=1)
{

	if (hentData == 1) {
		RegData <- NorSpisRegDataSQL(datoFra=datoFra, datoTil=datoTil)
	  }

# Preprosessere data
     if (preprosess==1){
       RegData <- NorSpisPreprosess(RegData=RegData)
     }


#----------- Figurparametre ------------------------------

retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
subtxt <- ''	#Benevning
flerevar <- 0
antDes <- 1
NB <- ''

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$AvdRESH)
if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$AvdRESH == reshID),]	#kun egen enhet
	}

#if (valgtVar=='Alder') {
#	gr <- c(0,50,100,150)
#	RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
#	#grtxt <- c('0-11', '11-12', '13-14', '15-16', '17-18', '19-20', '21-25', '26-30', '31-35', '36-40', '41-45', '46-50', '51-60, '61+')
#	#grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '50+')	#c(names(AndelLand)[-length(gr)], '90+')
#	subtxt <- 'Aldersgruppe'
#	Tittel <- 'Aldersfordeling'
#}

#if (valgtVar %in% c('Alder','B08StartAldrProbl', 'B12dAldrForsteBeh')) {
#      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
#      gr <- c(0,seq(5,50,5),150)
#      RegData$VariabelGr <- 99
#      indDum <- which(RegData[ ,valgtVar] %in% c(1:150))
#      RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
#      RegData$VariabelGr <- cut(RegData$valgtVar, breaks=gr, include.lowest=TRUE, right=FALSE)
#      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '50+')	#c(names(AndelLand)[-length(gr)], '90+')
#      subtxt <- 'Aldersgruppe'
#      Tittel <- switch(valgtVar,
#                       Alder = 'Aldersfordeling',
#                       B08StartAldrProbl = 'Alder da problemene startet',
#                       B12dAldrForsteBeh = 'Tidligere behandling: Alder ved start av første behandling')
#}



if (valgtVar=='Alder') {
gr <- c(0,seq(5,50,5),150)
RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
#grtxt <- c('0-11', '11-12', '13-14', '15-16', '17-18', '19-20', '21-25', '26-30', '31-35', '36-40', '41-45', '46-50', '51-60, '61+')
grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '50+')	#c(names(AndelLand)[-length(gr)], '90+')
subtxt <- 'Aldersgruppe'
Tittel <- 'Aldersfordeling'
}

if (valgtVar=='Norsktalende') {
      #0=Nei, 1=Ja, 2= Delvis, 9=Ukjent
      grtxt <- c('Nei','Ja', 'Delvis', 'Ukjent', 'Ikke registrert')
      RegData$VariabelGr <- 99
      indDum <- which(RegData$Norsktalende %in% c(0:2,9))
      RegData$VariabelGr[indDum] <- RegData$Norsktalende[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:2,9,99))
      Tittel <- 'Norsktalende'
}

if (valgtVar=='B04PabegyntUtd') {
  #OBS - se over/korrigere kategoriene
      retn <- 'H'
      grtxt <- c('Grunnskole','Videregående skole (1-3 år)',
                 'Høgskole eller universitet, mindre enn 4 år', 'Høgskole eller universitet, 4 år eller mer','Ukjent',
                 'Ikke registrert')
      RegData$VariabelGr <- 99
      indDum <- which(RegData$B04PabegyntUtd %in% c(1:4,9))
      RegData$VariabelGr[indDum] <- RegData$B04PabegyntUtd[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9,99))
      Tittel <- 'Høyeste påbegynte utdanning'
}

if (valgtVar=='B05FullfortUtd') {
  #OBS - se over/korrigere kategoriene
      retn <- 'H'
      grtxt <- c('Ikke fullført grunnskole','Grunnskole','Videregående skole (1-3 år)',
                 'Høgskole/universitet (<4 år)', 'Høgskole/universitet (>=4 år)','Ukjent',
                 'Ikke registrert')
      RegData$VariabelGr <- 99
      indDum <- which(RegData$B05FullfortUtd %in% c(1:5,9))
      RegData$VariabelGr[indDum] <- RegData$B05FullfortUtd[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9,99))
      Tittel <- 'Høyeste fullførte utdanning'
}

if (valgtVar=='MedBMI') {
  gr <- c(-1, 0, 18.5, 25, 30, 1000)
  #RegData$VariabelGr <- -1
  RegData$VariabelGr <- cut(RegData$MedBMI, breaks=gr, include.lowest=TRUE, right=FALSE)
  ind <- which(RegData$MedBMI>0)
  RegData$VariabelGr[ind] <- RegData$MedBMI[ind]
  # RegData$VariabelGr[ind] <- cut(RegData[ind ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
  # RegData$VariabelGr <- cut(RegData[,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
  # grtxt <- c('', '<18,5', levels(RegData$VariabelGr)[3:(length(gr)-2)],'30+')
  grtxt <- c('', '<18,5', '18,5-25', '25-30','30+')
  grtxt2 <- c('Ukjent', 'Undervekt', 'Normalvekt', 'Overvekt', 'Fedme')
  subtxt <- "Body Mass Index"
  Tittel <-  'Pasientenes BMI (Body Mass Index)'
}

if (valgtVar=='B06Hovedaktivitet') {
  # 1=Heltidsarbeid, 2=Deltidsarbeid, 3=På arbeidsmarkedstiltak, 4=Vernepliktig, 5=Skoleelev/lærling, 6=Student, 7=Sykemeldt, 8=Ufør, 9=Annen
  retn <- 'H'
  grtxt <- c('Heltidsarbeid', 'Deltidsarbeid', 'På arbeidsmarkedstiltak', 'Vernepliktig', 'Skoleelev/lærling', 'Student', 'Sykemeldt', 'Ufør', 'Annen', 'Ikke registrert')
  RegData$VariabelGr <- 99
  indDum <- which(RegData$B06Hovedaktivitet %in% c(1:9))
  RegData$VariabelGr[indDum] <- RegData$B06Hovedaktivitet[indDum]
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:9,99))
  Tittel <- 'Hovedaktivitet'
}

if (valgtVar=='B07Hovedinntekt') {
  retn <- 'H'
  grtxt <- c('Arbeidsinntekt', 'Sykepenger/trygd/pensjon', 'Blir forsørget', 'Sosialhjelp', 'Stipend/lån', 'Kursstønad/lønn i arbeidsmarkedstiltak', 'Andre inntekter', 'Ikke registrert')
  RegData$VariabelGr <- 99
  indDum <- which(RegData$B07Hovedinntekt %in% c(1:6))
  RegData$VariabelGr[indDum] <- RegData$B07Hovedinntekt[indDum]
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6,9,99))
  Tittel <- 'Pasientenes hovedinntekt'
}

#if (valgtVar=='B08StartAldrProbl') {
#  retn <- 'H'
#  grtxt <- c('Arbeidsinntekt', 'Sykepenger/trygd/pensjon', 'Blir forsørget', 'Sosialhjelp', 'Stipend/lån', 'Kursstønad/lønn i arbeidsmarkedstiltak', 'Andre inntekter', 'Ikke registrert')
#  RegData$VariabelGr <- 99
#  indDum <- which(RegData$B07Hovedinntekt %in% c(1:6))
#  RegData$VariabelGr[indDum] <- RegData$B07Hovedinntekt[indDum]
#  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6,9,99))
#  Tittel <- 'Pasientenes alder da problemene startet'
#}

if (valgtVar=='B11FamilieSF') {
  retn <- 'H'
  grtxt <- c('Nei', 'Ja', 'Vet ikke')
  RegData$VariabelGr <- 99
  indDum <- which(RegData$B11FamilieSF %in% c(0,1,9))
  RegData$VariabelGr[indDum] <- RegData$B11FamilieSF[indDum]
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9,99))
  Tittel <- 'Spiseforstyrrelse hos andre i familien?'
}

if (valgtVar=='B12TidlBehSF') {
  retn <- 'H'
  grtxt <- c('Nei', 'Ja')
  RegData$VariabelGr <- 99
  indDum <- which(RegData$B12TidlBehSF %in% c(0,1))
  RegData$VariabelGr[indDum] <- RegData$B12TidlBehSF[indDum]
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
  Tittel <- 'Tidligere behandling for spiseforstyrrelser?'
}


#if (valgtVar=='B12dArTilBehstart') {

#B12dMndTilBehstart OG B12dMndTilBehstart

#if (valgtVar=='B17FysMishandl') {
#  retn <- 'H'
#  grtxt <- c('Nei', 'Ja', 'Ukjent')
#  RegData$VariabelGr <- 99
#  indDum <- which(RegData$B17FysMishandl %in% c(0,1,9))
#  RegData$VariabelGr[indDum] <- RegData$B17FysMishandl[indDum]
#  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
#  Tittel <- 'Negative hendelse: Tidligere fysisk mishandling'
#}

#if B17FysMishandl, B18PsykMishandl, B19Overgrep, B20Mobbing -> 0,1,2,3,4 negative hendelser
#alle negative hendelsene i én figur



if (valgtVar %in% c('B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing', 'B21SelvskadTidl', 'B22SelvskadSisteAr', 'B23SelvmordFTidl', 'B24SelvmordFSisteAr', 'B25Avhengighet')) {
  retn <- 'H'
#  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
  grtxt <- c('Nei', 'Ja', 'Ukjent')
      RegData$VariabelGr <- 99
      indDum <- which(RegData[ ,valgtVar] %in% c(0,1,9))
      RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9,99))
      Tittel <- switch(valgtVar,
                   B17FysMishandl = 'Negativ hendelse: Tidligere fysisk mishandling',
                   B18PsykMishandl = 'Negativ hendelse: Tidligere psykisk mishandling',
                   B19Overgrep = 'Negativ hendelse: Tidligere misbruk/overgrep',
                   B20Mobbing = 'Negativ hendelse: Tidligere mobbing',
                   B21SelvskadTidl = 'Selvskading tidligere',
                   B22SelvskadSisteAr = 'Selvskading siste år',
                   B23SelvmordFTidl = 'Selvmordsforsøk tidligere',
                   B24SelvmordFSisteAr = 'Selvmordsforsøk siste år',
                   B25Avhengighet = 'Misbruk/avhengighet')
}

if (valgtVar %in% c('PT01OnsketInvolv', 'PT02BleInvolv', 'PT04KontaktBrukerorg', 'PT05OrientertBrukerorg')) {
      retn <- 'H'
      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
      grtxt <- c('Nei', 'Ja')
      RegData$VariabelGr <- 99
      indDum <- which(RegData[ ,valgtVar] %in% c(0,1))
      RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
      Tittel <- switch(valgtVar,
                       PT01OnsketInvolv = 'Pasienttilfredshet: Ønske om involvering av andre nære i behandlingen',
                       PT02BleInvolv = 'Pasienttilfredshet: Ble nære involvert i behandlingen?',
                       PT04KontaktBrukerorg = 'Pasienttilfredshet: Noen gang kontak med brukerorganisasjoner?',
                       PT05OrientertBrukerorg = 'Pasienttilfredshet: Informasjon om brukerorganisasjoner ila. behandlingen?')
}

if (valgtVar=='PT03Utfallsvurd') {
      grtxt <- c('Ikke noe problem lenger', 'Klar bedring', 'Noe bedring', 'Uendret', 'Forverring')
      RegData$VariabelGr <- 99
      indDum <- which(RegData$PT03Utfallsvurd %in% c(1:5))
      RegData$VariabelGr[indDum] <- RegData$PT03Utfallsvurd[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5, 99))
      Tittel <- 'Pasienttilfredshet: "Hvordan vurderer du utfallet av mottatt behandling?"'
}

#if (MedPsykofarmaka == 1) Hvordan legge inn betingelsen?
if (valgtVar %in% c('MedAntidepressiva', 'MedBenzodiazepiner', 'MedNevroleptika', 'MedAnnenMedBeh')) {
      retn <- 'H'
      #RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ] #LENA? Hjelpeargument?
      grtxt <- c('Nei', 'Ja')
      RegData$VariabelGr <- 99
      indDum <- which(RegData[ ,valgtVar] %in% 0,1)
      RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
      Tittel <- switch(valgtVar,
                       MedAntidepressiva = 'Antidepressiva',
                       MedBenzodiazepiner = 'Benzodiazepiner' ,
                       MedNevroleptika = 'Nevroleptika')
}

if (valgtVar=='BehUtfallsvurdSamlet') {
      grtxt <- c('Ikke noe problem lenger', 'Klar bedring', 'Noe bedring', 'Uendret', 'Forverring', 'Ikke registrert')
      RegData$VariabelGr <- 99
      indDum <- which(RegData$BehUtfallsvurdSamlet %in% c(1:5))
      RegData$VariabelGr[indDum] <- RegData$BehUtfallsvurdSamlet[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5, 99))
      Tittel <- 'Behandlers samlede utfallsvurdering'
}


if (valgtVar=='BehVidereBeh') {
      grtxt <- c('Nei', 'Ja', 'Ikke registrert')
      RegData$VariabelGr <- 99
      indDum <- which(RegData$BehVidereBeh %in% c(0,1))
      RegData$VariabelGr[indDum] <- RegData$BehVidereBeh[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
      Tittel <- 'Videre behandling annen instans?'
}

#----

if (valgtVar %in% c('ArbeidstausPreOp', 'Arbeidstaus3mnd', 'Arbeidstaus12mnd')) {
  retn <- 'H'
  RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ] #LENA? Hjelpeargument?
  grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt',
		'Delvis sykemeldt', 'Attføring/rehab.', 'Uførepensjon', 'Ufør og sykem.', 'Ikke utfylt')
	RegData$VariabelGr <- 99
	indDum <- which(RegData[ ,valgtVar] %in% 1:10)
	RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:10,99))
	Tittel <- switch(valgtVar,
	    ArbeidstausPreOp = 'Arbeidsstatus før operasjon',
	    Arbeidstaus3mnd = 'Arbeidsstatus 3 mnd. etter operasjon' ,
	    Arbeidstaus12mnd = 'Arbeidsstatus 12 mnd. etter operasjon')
}

#Lage indekser for:PO01Forstod	PO02Tillit	PO03InfoDiagnose	PO04Tilpasset	PO05Involvert	PO06Organisert	PO07Tilfredsstillende	PO08Tilgjengelighet	PO09Utbytte	PO10Pasientsikkerhet



if (valgtVar == 'Morsmal') {
  RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ]
  grtxt <- c('Norsk', 'Samisk', 'Annet', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Morsmal %in% 1:3)
	RegData$VariabelGr[indDum] <- RegData$Morsmal[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9))
	Tittel <- 'Morsmål'
}
if (valgtVar=='Utdanning') {
	retn <- 'H'
	grtxt <- c('Grunnskole++, 7-10år','Real-, yrkes- el vg skole',
				 'Allmennfaglig vg skole','Høyskole/universitet, <4 år','Høyskole/universitet, 4år+', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Utdanning %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData$Utdanning[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9))
	Tittel <- 'Utdanningsnivå'
}


#------------Gjøre utvalg-------------------------
NorSpisUtvalg <- NorSpisUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald,
		erMann=erMann)
RegData <- NorSpisUtvalg$RegData
utvalgTxt <- NorSpisUtvalg$utvalgTxt



#Generere hovedgruppe og sammenlikningsgruppe
#Trenger indeksene før genererer tall for figurer med flere variable med ulike utvalg
indEgen1 <- match(reshID, RegData$AvdRESH)
if (enhetsUtvalg %in% c(1,2)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$SykehusNavn[indEgen1]) } else {
		shtxt <- 'Hele landet'
			}

  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
    indRest <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      indHoved <-which(as.numeric(RegData$AvdRESH)==reshID)
      smltxt <- 'landet forøvrig'
      indRest <- which(as.numeric(RegData$AvdRESH) != reshID)
    }
  }


#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
Andeler <- list(Hoved = 0, Rest =0)
NRest <- 0
AntRest <- 0

if (flerevar == 0 ) {
AntHoved <- table(RegData$VariabelGr[indHoved])
NHoved <- sum(AntHoved)
Andeler$Hoved <- 100*AntHoved/NHoved
	if (medSml==1) {
		AntRest <- table(RegData$VariabelGr[indRest])
		NRest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
		Andeler$Rest <- 100*AntRest/NRest
	}
}


#FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG. EKSEMPLER FOR ULIKE SITUASJONER.
#DENNE DELEN VIL VI TRENGE FOR F.EKS. "HONOS, ENKELTLEDD"
if (valgtVar %in% c('Komorbiditet', 'KomplOpr', 'Kompl3mnd', 'OprIndik', 'OprIndikSmerter',
                    'OprIndikMyelopati', 'Radiologi')){
  flerevar <-  1
  utvalg <- c('Hoved', 'Rest')	#Hoved vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
  RegDataLand <- RegData
  NHoved <-length(indHoved)
  NRest <- length(indRest)

  for (teller in 1:(medSml+1)) {
  #  Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.
    RegData <- RegDataLand[switch(utvalg[teller], Hoved = indHoved, Rest=indRest), ]


     if (valgtVar=='OprIndik') {
         retn <- 'H'
         #OprIndiasjonasjonUfylt <>1 - tom variabel,
         #Svært få (ca 20 av 3000) har tom registrering. Setter derfor felles N lik alle reg.
         indSmerterk <- which(RegData$OprIndikSmerter == 1)
         indMyelopati <- which(RegData$OprIndikMyelopati == 1)
         Nmyelopati <- sum(RegData$OprIndikMyelopati, na.rm=T)
         AntVar <- cbind(
              #length(indAnnet),
              Pareser = sum(RegData$OprIndikParese, na.rm=T), #length(indPareser),
              Myelopati = length(indMyelopati),
              Smerter = length(indSmerterk),
              SmerterMyelop = length(intersect(indMyelopati, indSmerterk)),
              Annet = sum(RegData$OprIndikAnnet, na.rm=T)
         )
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('Pareser', 'Myelopati', 'Smerter', 'Sm. og Myelop.', 'Annet')
         Tittel <- 'Operasjonsårsak'
    }


    if (valgtVar=='Komorbiditet') {
         retn <- 'H'
          RegData <- RegData[which(RegData$AndreRelSykdommer>-1), ]
         RegData$SykdReumatisk <- 0
          indSykdReumatisk <- (RegData$SykdAnnenreumatisk ==1 | (RegData$SykdBechtrew==1 | RegData$SykdReumatoidartritt==1))
          RegData$SykdReumatisk[indSykdReumatisk] <- 1
         Variable <- c('SykdAnnenendokrin', 'SykdAnnet','SykdCarpalTunnelSyndr', 'SykdCerebrovaskular',
               'SykdDepresjonAngst', 'SykdHjertekar', 'SykdHodepine', 'SykdHypertensjon', 'SykDiabetesMellitus',
              'SykdKreft', 'SykdKroniskLunge', 'SykdKroniskNevrologisk', 'SykdKrSmerterMuskelSkjelSyst',
             'SykdOsteoporose', 'SykdSkulderImpigment', 'SykdWhiplashNorSpis')
         AntVar <- colSums (RegData[ ,c("SykdReumatisk", Variable, "AndreRelSykdommer")], na.rm = TRUE)
         NVar<-rep(dim(RegData)[1], length(AntVar))
         grtxt <- c('Annen Reumatisk', 'Annen endokrin', 'Andre', 'Carpal TS', 'Cerebrovaskulær', 'Depresjon/Angst',
         'Hjerte-/Karsykd.', 'Hodepine', 'Hypertensjon', 'Diabetes', 'Kreft', 'Kr. lungesykdom',
         'Kr. nevrologisk', 'Kr. muskel/skjelettsm.', 'Osteoporose', 'Skuldersyndrom', 'Whiplash/skade', 'Tot. komorb')

         Tittel <- 'Komorbiditet'
    }

  if (valgtVar=='OprIndikSmerter') {
  	retn <- 'H'
  	indSmerteArm <- which(RegData$OprIndikSmerteLokArm == 1)
  	indSmerteNorSpis <- which(RegData$OprIndikSmerteLokNorSpis == 1)
  	Nsmerte <- sum(RegData$OprIndikSmerter, na.rm=T)
  	AntVar <- cbind(
  		Smerte = Nsmerte,
  		SmerteArm = length(indSmerteArm),
  		SmerteNorSpis = length(indSmerteNorSpis),
  		SmerteArmNorSpis = length(intersect(indSmerteArm, indSmerteNorSpis))
  	)
  	NVar<- cbind(
  		Smerte = length(which(RegData$OprIndikSmerter > -1)),
  		SmerteArm = Nsmerte,
  		SmerteNorSpis = Nsmerte,
  		SmerteArmNorSpis = Nsmerte
  	)
  	grtxt <- c('Smerter', '...Arm', '...Nakke', '...Arm og Nakke')
  	Tittel <- 'Operasjonsårsak: Smerter'
  		}

#Generelt for alle figurer med sammensatte variable:
  	if (teller == 1) {
  		AntHoved <- AntVar
  		NHoved <- max(NVar, na.rm=T)
  		Andeler$Hoved <- 100*AntVar/NVar
  	}
  	if (teller == 2) {
  		AntRest <- AntVar
  		NRest <- max(NVar,na.rm=T)	#length(indRest)- Kan inneholde NA
  		Andeler$Rest <- 100*AntVar/NVar
  	}
  } #end medSml (med sammenligning)
}	#end sjekk om figuren inneholder flere variable



#-----------Figur---------------------------------------
#Hvis for få observasjoner..
if ( NHoved %in% 1:5 | 	(medSml ==1 & NRest<5)) {
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(Tittel)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
	if ( outfile != '') {dev.off()}

} else {

#-----------Figur---------------------------------------
#Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr
cexgr <- 1	#Kan endres for enkeltvariable


#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NorSpisUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
antDesTxt <- paste('%.', antDes, 'f', sep='')
grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)', sep='')
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

farger <- FigTypUt$farger
fargeHoved <- farger[1]
fargeRest <- farger[3]
antGr <- length(grtxt)
lwdRest <- 1	#tykkelse på linja som repr. landet
cexleg <- 1	#Størrelse på legendtekst

#Horisontale søyler
if (retn == 'H') {
	xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel,
		col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
	if (NHoved>0) {mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)}

	if (medSml == 1) {
		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
		legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''),
						paste(smltxt, ' (N=', NRest,')', sep='')),
			border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
			lty=NA, ncol=1,lwd=lwdRest, cex=cexleg) #
		} else {
		legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
			border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		}
}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	if (length(grtxt2) == 1) {grtxt2 <- paste('(', sprintf(antDesTxt, Andeler$Hoved), '%)', sep='')}
	ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
		xlab=subtxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=subtxt,
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
if (medSml == 1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
	legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')),
		border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
		lwd=lwdRest, ncol=2, cex=cexleg)	#
	} else {
	legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
		border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
	}
}

title(Tittel, line=1, font.main=1)

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
}

#Beregninger som returneres fra funksjonen.
AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
rownames(AndelerUt) <- c('Hoved', 'Rest')
AntallUt <- rbind(AntHoved, AntRest)
rownames(AntallUt) <- c('Hoved', 'Rest')

UtData <- list(paste(toString(Tittel),'.', sep=''), AndelerUt, AntallUt, grtxt )
names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
return(invisible(UtData))

}
