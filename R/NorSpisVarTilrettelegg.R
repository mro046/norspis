#' Funksjon for å tilrettelegge variable for beregning. 
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk. 
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt. 
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen. 
#' Her kan mye hentes til analysebok
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Aldersfordeling, 10-årige grupper 
#'     \item InnMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'     \item liggetid: Liggetid 
#'     \item NEMS: Skår for ressursbruk. (Nine Equivalents of Nursing Manpower Use Score)
#'     \item Nas: Skår for sykepleieraktiviteter. (Nursing Activities Score)
#'     \item respiratortid: Tid tilbrakt i respirator
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
#'    }
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Hele landet
#'     \item 1: Egen enhet mot resten av landet (Standard)
#'     \item 2: Egen enhet
#'     \item 3: Egen enhet mot egen sykehustype
#'     \item 4: Egen sykehustype
#'     \item 5: Egen sykehustype mot resten av landet
#'     \item 6: Egen enhet mot egen region [NB: Intensivregiisteret mangler pt. variabel for region]
#'     \item 7: Egen region [NB: Mangler pt. variabel for region]
#'	   \item 8: Egen region mot resten [NB: Mangler pt. variabel for region]
#'    	}							
#'    				
#' @inheritParams NorSpisFigAndeler
#'				
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

NorSpisVarTilrettelegg  <- function(RegData, valgtVar, grVar=''){

      grtxt <- 0
      xAkseTxt <- ''
      KImaal <- NA
      retn='V'
      tittel <- '' 
      flerevar <- 0
      sortAvtagende <- T
      
      
      
#if (valgtVar %in% c('Alder','B08StartAldrProbl', 'B12dAldrForsteBeh')) {
#      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
#      gr <- c(0,seq(5,50,5),150)
#      RegData$VariabelGr <- 99
#      indDum <- which(RegData[ ,valgtVar] %in% c(1:150))
#      RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
#      RegData$VariabelGr <- cut(RegData$valgtVar, breaks=gr, include.lowest=TRUE, right=FALSE)
#      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '50+')	#c(names(AndelLand)[-length(gr)], '90+')
#      subtxt <- 'Aldersgruppe'
#      tittel <- switch(valgtVar,
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
tittel <- 'Aldersfordeling'
}

if (valgtVar=='Norsktalende') {
      #0=Nei, 1=Ja, 2= Delvis, 9=Ukjent
      grtxt <- c('Nei','Ja', 'Delvis', 'Ukjent', 'Ikke registrert')
      RegData$VariabelGr <- 99
      indDum <- which(RegData$Norsktalende %in% c(0:2,9))
      RegData$VariabelGr[indDum] <- RegData$Norsktalende[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:2,9,99))
      tittel <- 'Norsktalende'
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
      tittel <- 'Høyeste påbegynte utdanning'
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
      tittel <- 'Høyeste fullførte utdanning'
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
  tittel <-  'Pasientenes BMI (Body Mass Index)'
}

if (valgtVar=='B06Hovedaktivitet') {
  # 1=Heltidsarbeid, 2=Deltidsarbeid, 3=På arbeidsmarkedstiltak, 4=Vernepliktig, 5=Skoleelev/lærling, 6=Student, 7=Sykemeldt, 8=Ufør, 9=Annen
  retn <- 'H'
  grtxt <- c('Heltidsarbeid', 'Deltidsarbeid', 'På arbeidsmarkedstiltak', 'Vernepliktig', 'Skoleelev/lærling', 'Student', 'Sykemeldt', 'Ufør', 'Annen', 'Ikke registrert')
  RegData$VariabelGr <- 99
  indDum <- which(RegData$B06Hovedaktivitet %in% c(1:9))
  RegData$VariabelGr[indDum] <- RegData$B06Hovedaktivitet[indDum]
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:9,99))
  tittel <- 'Hovedaktivitet'
}

if (valgtVar=='B07Hovedinntekt') {
  retn <- 'H'
  grtxt <- c('Arbeidsinntekt', 'Sykepenger/trygd/pensjon', 'Blir forsørget', 'Sosialhjelp', 'Stipend/lån', 'Kursstønad/lønn i arbeidsmarkedstiltak', 'Andre inntekter', 'Ikke registrert')
  RegData$VariabelGr <- 99
  indDum <- which(RegData$B07Hovedinntekt %in% c(1:6))
  RegData$VariabelGr[indDum] <- RegData$B07Hovedinntekt[indDum]
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6,9,99))
  tittel <- 'Pasientenes hovedinntekt'
}

#if (valgtVar=='B08StartAldrProbl') {
#  retn <- 'H'
#  grtxt <- c('Arbeidsinntekt', 'Sykepenger/trygd/pensjon', 'Blir forsørget', 'Sosialhjelp', 'Stipend/lån', 'Kursstønad/lønn i arbeidsmarkedstiltak', 'Andre inntekter', 'Ikke registrert')
#  RegData$VariabelGr <- 99
#  indDum <- which(RegData$B07Hovedinntekt %in% c(1:6))
#  RegData$VariabelGr[indDum] <- RegData$B07Hovedinntekt[indDum]
#  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6,9,99))
#  tittel <- 'Pasientenes alder da problemene startet'
#}

if (valgtVar=='B11FamilieSF') {
  retn <- 'H'
  grtxt <- c('Nei', 'Ja', 'Vet ikke')
  RegData$VariabelGr <- 99
  indDum <- which(RegData$B11FamilieSF %in% c(0,1,9))
  RegData$VariabelGr[indDum] <- RegData$B11FamilieSF[indDum]
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9,99))
  tittel <- 'Spiseforstyrrelse hos andre i familien?'
}

if (valgtVar=='B12TidlBehSF') {
  retn <- 'H'
  grtxt <- c('Nei', 'Ja')
  RegData$VariabelGr <- 99
  indDum <- which(RegData$B12TidlBehSF %in% c(0,1))
  RegData$VariabelGr[indDum] <- RegData$B12TidlBehSF[indDum]
  RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
  tittel <- 'Tidligere behandling for spiseforstyrrelser?'
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
#  tittel <- 'Negative hendelse: Tidligere fysisk mishandling'
#}

#if B17FysMishandl, B18PsykMishandl, B19Overgrep, B20Mobbing -> 0,1,2,3,4 negative hendelser
#alle negative hendelsene i én figur



if (valgtVar %in% c('B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing', 
                    'B21SelvskadTidl', 'B22SelvskadSisteAr', 'B23SelvmordFTidl', 'B24SelvmordFSisteAr', 
                    'B25Avhengighet')) {
  retn <- 'H'
#  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
  grtxt <- c('Nei', 'Ja', 'Ukjent')
      RegData$VariabelGr <- 99
      indDum <- which(RegData[ ,valgtVar] %in% c(0,1,9))
      RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9,99))
      tittel <- switch(valgtVar,
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

if (valgtVar %in% c('PT01OnsketInvolv', 'PT02BleInvolv', 'PT04KontaktBrukerorg', 
                    'PT05OrientertBrukerorg')) {
      retn <- 'H'
      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
      grtxt <- c('Nei', 'Ja')
      RegData$VariabelGr <- 99
      indDum <- which(RegData[ ,valgtVar] %in% c(0,1))
      RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
      tittel <- switch(valgtVar,
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
      tittel <- 'Pasienttilfredshet: "Hvordan vurderer du utfallet av mottatt behandling?"'
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
      tittel <- switch(valgtVar,
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
      tittel <- 'Behandlers samlede utfallsvurdering'
}


if (valgtVar=='BehVidereBeh') {
      grtxt <- c('Nei', 'Ja', 'Ikke registrert')
      RegData$VariabelGr <- 99
      indDum <- which(RegData$BehVidereBeh %in% c(0,1))
      RegData$VariabelGr[indDum] <- RegData$BehVidereBeh[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
      tittel <- 'Videre behandling annen instans?'
}

#Lage indekser for:PO01Forstod	PO02Tillit	PO03InfoDiagnose	PO04Tilpasset	PO05Involvert	PO06Organisert	PO07Tilfredsstillende	PO08Tilgjengelighet	PO09Utbytte	PO10Pasientsikkerhet

      
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
                  tittel <- 'Operasjonsårsak'
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
                  
                  tittel <- 'Komorbiditet'
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
                  tittel <- 'Operasjonsårsak: Smerter'
            }
      }
}




      
      
      
UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, KImaal=KImaal, retn=retn,
               tittel=tittel, flerevar=flerevar, sortAvtagende=sortAvtagende)
#RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
return(invisible(UtData)) 
      
}