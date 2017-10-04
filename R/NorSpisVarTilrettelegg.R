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
#'     \item alder: Aldersfordeling, 5-årige grupper 
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
      variable <- 'Ingen'
      deltittel <- ''
      RegData$Variabel <- 0
      

      
#--------Variabler  - tilrettelagt og sortert alfabetisk 
#--------(tre figurtyper er representert:                   NorSpisFigAndeler, NorSpisFigAndelerGrVar, NorSpisFigGjsnGrVar)


#if (valgtVar=='Alder') {   ##En egen snutt for alder (her med 10-årsintervaller), men alder er også i snutten under##
#      gr <- c(0,seq(10,50,10),150)
#      RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
#      #grtxt <- c('0-11', '11-12', '13-14', '15-16', '17-18', '19-20', '21-25', '26-30', '31-35', '36-40', '41-45', '46-50', '51-60, '61+')
#      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '50+')	#c(names(AndelLand)[-length(gr)], '90+')
#      xAkseTxt <- 'Aldersgruppe'
#      tittel <- 'Aldersfordeling'
#}
      
      
if (valgtVar %in% c('Alder','B08StartAldrProbl', 'B12cAldrForsteBeh')) {                  #BRUKES I: Andeler
      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
      RegData <- RegData[which(RegData$RegRegtype == 1 | RegData$RegRegtype ==2 | RegData$RegRegtype ==3 | RegData$RegRegtype ==4), ]#velger kun startregistreringer og utredninger fordi ellers får vi dobbelt opp med pasienter (1=Utredning voksen, 2=Utredning ungdom/barn, 3=Startregistrering voksen, 4=Startregistrering ungdom/barn)
      #feil1: teller en pasient flere ganger hvis mottar behandling to steder ganger - må fjerne duplikat
      #feil2: teller også en pasient flere ganger hvis mottar behandling samme sted to ganger - må fjerne også denne formen for duplikat
      #feil3: hvis retter feil 1 og 2 kan utvalget "egen enhet mot resten av landet" vli feil - for hvor skal pasienten telles? Egen enhet: ja. Landet: ja, hvis utvalget er "hele landet", men usikkert hvis mottar beh. to egen enhet pluss landet (alternativene blir å telle pasoenten kun ved egen enhet eller både ved egen enhet og "hele landet"). 
      #riktig blir kanskje: Telle hver PasientID(variabelnavn) kun én gang for hvert behandlingssted(altså 2 ganger hvis mottar behandling 2 steder)
      #...men kun engang i statistikken for hele landet 
      
      gr <- c(0,seq(5,30,5),150)
      #indDum <- which(RegData[ ,valgtVar] %in% c(1:150))
      #RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '30+')	#c(names(AndelLand)[-length(gr)], '90+')
      subtxt <- 'Aldersgruppe'
      tittel <- switch(valgtVar,
                       Alder = 'Aldersfordeling',
                       B08StartAldrProbl = 'Alder da problemene startet',
                       B12cAldrForsteBeh = 'Tidligere behandling: Alder ved start av første behandling')
}          


if (valgtVar == 'AlderGjsn') { #endret fra alder fordi annen tiltretteligging heter alder. #BRUKES I: GjsnGrVar 
      RegData <- RegData[which(RegData[ ,'Alder'] >0), ]
      RegData$Variabel <- RegData[ ,'Alder']
      deltittel <- 'alder'
      xaksetxt <- 'Alder (år)'
}
      
if (valgtVar=='alder_u18') {	                                                            #BRUKES I: AndelerGrVar
      RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
      RegData$Variabel[which(RegData$Alder<18)] <- 1
      tittel <- 'Pasienter under 18 år'
}
      
if (valgtVar=='B01Sivilstatus') {                                                         #BRUKES I: Andeler
      retn <- 'V'
      grtxt <- c('Enslig','Samboer','Gift','Skilt','Enke/\nenkemann','Annen') 
    
      #sortere bort ugyldige rader
      koder <- c(1:5,9)
      indDum <- which(RegData$B01Sivilstatus %in% koder)#
      RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige 
      
      RegData$VariabelGr <- factor(RegData$B01Sivilstatus, levels = koder)
      xAkseTxt <- 'Sivilstatus'
      tittel <- 'Sivilstatus'
}   

#enkelt (men feil?) kodesnutt for B01Sivilstatus:      
#if (valgtVar=='B01Sivilstatus') {                                                         #BRUKES I: Andeler
#      grtxt <- c('Enslig','Samboer','Gift','Skilt','Enke/\nenkemann','Annen')
#      RegData$VariabelGr <- factor(RegData$B01Sivilstatus, levels = koder)
#      tittel <- 'Sivilstatus'
#}   

if (valgtVar=='B02EgneBarn') {                                                                  #BRUKES I: Andeler
      
      #sortere bort ugyldige rader
      koder <- c(0:15)
      indDum <- which(RegData$B02EgneBarn %in% koder)#
      RegData <- RegData[indDum,] #velger de gyldige radene/
      
      grtxt <- c(0:4, '5+')	#c(names(AndelLand)[-length(gr)], '90+')
      gr <- c(0:5,15)
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
      
      xAkseTxt <- 'Antall barn'
      tittel <- 'Antall egne barn'
}      
      
if (valgtVar=='B03Bosituasjon') {                                                                #BRUKES I: Andeler
      #Alternativer: 1	Hos en av foreldrene, 2 Hos begge foreldre, 3 Bor alene, 4  Med partner, 
      #5 Med partner og barn, 6 Uten partner med barn, 9 Annen
      retn <- 'H'
      grtxt <- c ('Hos en av foreldrene', 'Hos begge foreldre', 'Bor alene', 'Med partner', 
                  'Med partner og barn', 'Uten partner med barn', 'Annen')
      #RegData$VariabelGr <- 99
      
      #Velge kun gyldige rader:      
      koder <- c (1:6,9) #definerer gyldige koder/kategorier, bukes til å lage indDum under, som videre brukes til å velge gyldige rader to linjer ned 
      indDum <- which(RegData$B03Bosituasjon %in% koder)#
      RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige 
      
      #lager en variabel kalt VariabelGr som skal brukes i NorSpisFigAndeler.R. Fordi vi har fjernet ugyldige rader i kodelinjene ovenfor, 
      #kan vi bare lage variabelen lik B03Bosituasjon. Vi fester variabelen til RegData. Deretter gjør vi den om til en faktor:
      RegData$VariabelGr <- RegData$B03Bosituasjon  #Endret fordi følgende gav feilmeld: RegData$VariabelGr[indDum] <- RegData$B03Bosituasjon[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder) #Komm. 2.mai -17: Denne var ikke med. Derfor feilmld
      #Trenger strengt tatt ikke levels, men har den med fordi den 
      #forsikrer oss om at kategorier som kan være tomme 
      #(e.g. ingen bodde alene, kategori 3) likevel kommer med i 
      #figur. 
      #Lage tittelen til diagrammet
      #xAkseTxt <- 'Bosituasjon'
      tittel <- 'Bosituasjon'
}
      
if (valgtVar %in% c('B04PabegyntUtd', 'B05FullfortUtd')) {                                       #BRUKES I: Andeler
      #OBS - se over/korrigere kategoriene
      retn <- 'H'
      grtxt <- switch(valgtVar, 
                      B04PabegyntUtd=c('Grunnskole','Videregående skole (1-3 år)',
                                       'Høgskole eller universitet, \nmindre enn 4 år', 
                                       'Høgskole eller universitet, \n4 år eller mer','Ukjent'),
                      B05FullfortUtd=c('Ikke fullført grunnskole','Grunnskole','Videregående skole (1-3 år)',
                                       'Høgskole eller universitet, \nmindre enn 4 år', 
                                       'Høgskole eller universitet, \n4 år eller mer','Ukjent'))

      #Velge kun gyldige rader:
      koder <- switch(valgtVar,
                        B04PabegyntUtd=c(1:4,9),
                        B05FullfortUtd=c(1:5,9))
      indDum <- which(RegData[ ,valgtVar] %in% koder)
      RegData <- RegData[indDum, ]
      
      RegData$VariabelGr <- RegData[ ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
      tittel <- switch(valgtVar,
                       B04PabegyntUtd ='Høyeste påbegynte utdanning',
                       B05FullfortUtd ='Høyeste fullførte utdanning')
}

      
if (valgtVar=='B06Hovedaktivitet') {                                                            #BRUKES I: Andeler
      # 1=Heltidsarbeid, 2=Deltidsarbeid, 3=På arbeidsmarkedstiltak, 4=Vernepliktig, 5=Skoleelev/lærling, 6=Student, 7=Sykemeldt, 8=Ufør, 9=Annen
      retn <- 'H'
      grtxt <- c('Heltidsarbeid', 'Deltidsarbeid', 'På arbeidsmarkedstiltak', 'Vernepliktig', 'Skoleelev/lærling', 'Student', 'Sykemeldt', 'Ufør', 'Annen')
      
      #Velge kun gyldige rader:
      koder <- c(1:9)
      #RegData$VariabelGr <- 99
      indDum <- which(RegData$B06Hovedaktivitet %in% koder)
      RegData <- RegData[indDum, ] 
      
      RegData$VariabelGr<- RegData$B06Hovedaktivitet #RegData$VariabelGr[indDum] <- RegData$B06Hovedaktivitet[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
      tittel <- 'Hovedaktivitet'
}
      
if (valgtVar=='B07Hovedinntekt') {                                                               #BRUKES I: Andeler
      retn <- 'H'
      grtxt <- c('Arbeidsinntekt', 'Sykepenger/trygd/pensjon', 'Blir forsørget', 'Sosialhjelp', 'Stipend/lån', 'Kursstønad/\nlønn i arbeidsmarkedstiltak', 'Andre inntekter')
      
      #Velge kun gyldige rader:
      koder <- c(1:6,9)
      #RegData$VariabelGr <- 99
      indDum <- which(RegData$B07Hovedinntekt %in% koder)
      
      RegData$VariabelGr <- RegData$B07Hovedinntekt #RegData$VariabelGr[indDum] <- RegData$B07Hovedinntekt[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6,9))
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

#if (valgtVar =='B08StartAldrProbl'){                                                      #BRUKES I: GjsnGrVar; 
#      #Velge gyldige observasjoner:
#      RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
#
#      RegData$Variabel <- RegData[ ,valgtVar]
#      deltittel <- 'alder ved start av problematikk'
#      xaksetxt <- 'Alder (år)'
#    
#}
      
#if (valgtVar %in% c('B08StartAldrProbl', 'B12cAldrForsteBeh')) {                                 #BRUKES I: Andeler
#      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
#      gr <- c(0,seq(5,30,5),150)
#      #indDum <- which(RegData[ ,valgtVar] %in% c(1:150))
#      #RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
#      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
#      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '30+')	#c(names(AndelLand)[-length(gr)], '90+')
#     xAkseTxt <- 'Aldersgruppe'
#      tittel <- switch(valgtVar,
#                      B08StartAldrProbl = 'Alder da problemene startet',
#                       B12cAldrForsteBeh = 'Tidligere behandling: Alder ved start av første behandling')
#}
      
if (valgtVar=='B11FamilieSF') {                                                                  #BRUKES I: Andeler
      retn <- 'V'
      grtxt <- c('Nei', 'Ja', 'Vet ikke')
      
      #Velge kun gyldige rader:
      koder <- c(0,1,9)
      #RegData$VariabelGr <- 99
      indDum <- which(RegData$B11FamilieSF %in% koder)
      RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData$B11FamilieSF[indDum]
      
      RegData$VariabelGr <- RegData$B11FamilieSF
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
      tittel <- 'Spiseforstyrrelse hos andre i familien?'
}
      
if (valgtVar=='B12TidlBehSF') {                                                                  #BRUKES I: Andeler
      grtxt <- c('Nei', 'Ja')
      
      #Velge kun gyldige rader:
      koder <- c(0,1)
      #RegData$VariabelGr <- 99
      indDum <- which(RegData$B12TidlBehSF %in% koder)
      RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData$B12TidlBehSF[indDum]
      
      RegData$VariabelGr <- RegData$B12TidlBehSF
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1))
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
      
      
#if (valgtVar =='B12cAldrForsteBeh'){                                                      #BRUKES I: GjsnGrVar; 
#      #Velge gyldige observasjoner:
#      RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
#      
#      RegData$Variabel <- RegData[ ,valgtVar]
#      deltittel <- 'alder ved første behandling'
#      xaksetxt <- 'Alder (år)'
#}
      
      

if (valgtVar %in% c('B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing', 
                    'B21SelvskadTidl', 'B22SelvskadSisteAr', 'B23SelvmordFTidl', 'B24SelvmordFSisteAr', 
                    'B25Avhengighet')) {                                                        #BRUKES I: Andeler
      retn <- 'V'
      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
      grtxt <- c('Nei', 'Ja', 'Ukjent')
      
      #Velge kun gyldige rader:
      koder <- c(0,1,9)
      #RegData$VariabelGr <- 99
      indDum <- which(RegData[ ,valgtVar] %in% koder)
      RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
      
      RegData$VariabelGr <- RegData[ ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
      tittel <- switch(valgtVar,
                       B17FysMishandl = 'Negativ hendelse: Tidligere fysisk mishandling',
                       B18PsykMishandl = 'Negativ hendelse: Tidligere psykisk mishandling',
                       B19Overgrep = 'Negativ hendelse: Tidligere misbruk/overgrep',
                       B20Mobbing = 'Negativ hendelse: Tidligere mobbing',
                       B21SelvskadTidl = 'Selvskading tidligere',
                       B22SelvskadSisteAr = 'Selvskading siste år',
                       B23SelvmordFTidl = 'Selvmordsforsøk tidligere',
                       B24SelvmordFSisteAr = 'Selvmordsforsøk siste år',
                       B25Avhengighet = 'Misbruk/avhengighet av alkohol, illegale rusmidler eller medikamenter')
}
      
if (valgtVar=='BehDodUnderBeh') {	                                                      #BRUKES I: AndelerGrVar
      #Velge bort ugyldige (NA) observasjoner
      koder <- c(0,1)
      RegData <- RegData[which(RegData[ ,valgtVar] %in% koder), ]
      #RegData <- RegData[which(RegData$BehDodUnderBeh>=0), ]    #tar bort eventuelle verdier som er <0
      #RegData$Variabel[which(RegData$BehDodUnderBeh==1)] <- 1
      
      RegData$Variabel <- RegData$BehDodUnderBeh
      tittel <- 'Dødsfall under behandling'
}      


if (valgtVar=='BehUtfallsvurdSamlet') {                                                          #BRUKES I: Andeler
      grtxt <- c('Ikke noe\n problem lenger', 'Klar bedring', 'Noe bedring', 'Uendret', 'Forverring', 'Ikke aktuelt')
      
      #Velge kun gyldige rader:
      koder <- c(1:6)
      #RegData$VariabelGr <- 99
      indDum <- which(RegData$BehUtfallsvurdSamlet %in% koder)
      RegData <- RegData[indDum, ]#RegData$VariabelGr[indDum] <- RegData$BehUtfallsvurdSamlet[indDum]
      
      RegData$VariabelGr <- RegData$BehUtfallsvurdSamlet
      
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6))
      tittel <- 'Behandlers samlede utfallsvurdering'
}
      
if (valgtVar=='BehVidereBeh') {                                                                  #BRUKES I: Andeler
      grtxt <- c('Nei', 'Ja')
      
      #Velge kun gyldige rader:
      koder <- c(0,1)
      #RegData$VariabelGr <- 99
      indDum <- which(RegData$BehVidereBeh %in% koder)
      RegData <- RegData [indDum, ]
      
      RegData$VariabelGr <- RegData$BehVidereBeh
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
      tittel <- 'Videre behandling annen instans?'
}
            
if (valgtVar=='DiagVDiabetes') {                                                          #BRUKES I: AndelerGrVar
      #Velge bort ugyldige (NA) observasjoner
      koder <- c(0,1)
      RegData <- RegData[which(RegData$DiagVDiabetes %in% koder), ]
      
      RegData$Variabel <- RegData$DiagVDiabetes
      tittel <- 'Diabetes'
      sortAvtagende <- FALSE
}
      
      
      

if (valgtVar=='DiagVSF') {                                                                       #BRUKES I: Andeler
      grtxt <- c('F500', 'F501', 'F502', 'F505', 'F509', 'Andre')  #FIKSE FIGUR: Mangler mulighet for at nye diagnosekoder kan komme til å bli brukt. Se e-post fra Lena 15.02.2017 for hvordan dette er løst i NGER 
      
      #Velge bortugyldige rader
      koder <- c('F500', 'F501', 'F502', 'F505', 'F509', 'Z004')
      indDum <- which(RegData$DiagVSF %in% koder)
      RegData <- RegData[indDum, ]
      
      
      RegData$DiagVSF <- as.character(RegData$DiagVSF)
      
      RegData$VariabelGr <-RegData$DiagVSF
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
      tittel <- 'Spiseforstyrrelsesdiagnose ICD-10'
}
      
      
      
if (valgtVar %in% c('EDEQ60GlobalScore',
                    'EDEQ60Restriksjon', 
                    'EDEQ60Kroppsform', 
                    'EDEQ60Spising', 
                    'EDEQ60Vekt'))
{                                                                                         #BRUKES I: GjsnGrVar;                                                                  
      #Velge gyldige observasjoner:
      RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
      
      RegData$Variabel <- RegData[ ,valgtVar]
      deltittel <- switch(valgtVar, 
                          EDEQ60GlobalScore = 'symptomtrykk: Global-skåre, EDE-Q 6.0',
                          EDEQ60Restriksjon = 'symptomtrykk: Restriksjon, EDE-Q 6.0',
                          EDEQ60Kroppsform = 'symptomtrykk: Kroppsform, EDE-Q 6.0',
                          EDEQ60Spising = 'symptomtrykk: Spising, EDE-Q 6.0',
                          EDEQ60Vekt = 'symptomtrykk: Vekt,EDE-Q 6.0')
      xaksetxt <- switch(valgtVar, 
                         EDEQ60GlobalScore = 'Global-skåre',
                         EDEQ60Restriksjon = 'Restriksjon',
                         EDEQ60Kroppsform = 'Kroppsform',
                         EDEQ60Spising = 'Spising',
                         EDEQ60Vekt = 'Vekt')
}
      


      
if (valgtVar %in% c('HCA01Atferd', 'HCA02Aktivitetsniva', 'HCA03Selvskade', 'HCA04Rusmisbruk', 'HCA05SkoleSprak',
                    'HCA06FysiskProblem', 'HCA07Hallusinasjoner', 'HCA08SomatiskSymp', 'HCA09EmosjonelleSymp','HCA10JevnaldrProbl', 
                    'HCA11Egenomsorg', 'HCA12FamilieProbl', 'HCASkoleframmote', 'HCA14ProblKunnskap', 'HCA15Mangelinfo', 'H01Atferd',
                    'H02Selvskade','H03Rusmisbruk', 'H04KognitiveProbl','H05FysiskeProbl', 'H06Hallusinasjoner','H07Stemningsleie',
                    'H08AndreProbl','H09ForhAndre','H10ADLProbl','H11BoligProbl','H12YrkeProbl')) {             
                                                                                                #BRUKES I: Andeler
      retn <- 'H'
      grtxt <- c('Ingen problem', 'Lite problem som ikke \n krever tiltak', 'Mildt problem, \n men klart tilstede',
                 'Moderat alvorlig problem', 'Alvorlig til svært alvorlig\n problem', 'Ukjent')
      
      #Velge kun gyldige rader:
      koder <- c(0:4,9)
      #RegData$VariabelGr <- 99
      indDum <- which(RegData[ ,valgtVar] %in% koder)
      RegData <- RegData[indDum, ]
      
      RegData$VariabelGr <- RegData[ ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
      tittel <- switch(valgtVar,
                       HCA01Atferd = 'HONOSCA: 1. Problemer med forstyrrende,\n antisosial eller aggressiv atferd',
                       HCA02Aktivitetsniva = c('HONOSCA: 2.Problemer med høyt aktivitetsnivå,', 'oppmerksomhet eller konsentrasjon'),
                       HCA03Selvskade = 'HONOSCA: 3.Selvskade som ikke skyldes uhell',
                       HCA04Rusmisbruk = 'HONOSCA: 4.Problemer med alkohol, stoff eller løsemiddelmisbruk',
                       HCA05SkoleSprak = 'HONOSCA: 5.Problemer med skole- eller språkferdigheter',
                       HCA06FysiskProblem = 'HONOSCA: 6. Problemer pga fysisk sykdom eller funksjonshemming',
                       HCA07Hallusinasjoner = 'HONOSCA: 7.Problemer knyttet til hallusinasjoner, \n vrangforestillinger eller unormale persepsjoner',
                       HCA08SomatiskSymp = 'HONOSCA: 8. Problemer med somatiske symptomer \n uten kjent organisk grunnlag',
                       HCA09EmosjonelleSymp = 'HONOSCA: 9. Problemer med emosjonelle \n og relaterte symptomer',
                       HCA10JevnaldrProbl = 'HONOSCA: 10. Problemer med forhold til jevnaldrende',
                       HCA11Egenomsorg = 'HONOSCA: 11.Problemer med egenomsorg og uavhengighet',
                       HCA12FamilieProbl = 'HONOSCA: 12.Problemer med familieliv og forhold til andre',
                       HCASkoleframmote = 'HONOSCA: 13.Dårlig skoleframmøte',
                       HCA14ProblKunnskap = 'HONOSCA: 14.Problemer med kunnskap eller forståelse av \n egenarten av barnets/ungdommens vanskeligheter ',
                       HCA15Mangelinfo = 'HONOSCA: 15.Problemer med mangel på informasjon om \n tilbud eller behandling av barnets/ungdommens vanskeligheter',
                       H01Atferd = 'HoNOS: 1.Overaktiv, aggressiv, forstyrrende eller agitert atferd',
                       H02Selvskade = 'HoNOS: 2.Selvskade som ikke skyldes uhell',
                       H03Rusmisbruk = 'HoNOS: 3.Problemdrikking eller bruk av rusmiddel',
                       H04KognitiveProbl = 'HoNOS: 4.Kognitive problemer',
                       H05FysiskeProbl = 'HoNOS: 5.Problemer med fysisk sykdom eller funksjonshemming',
                       H06Hallusinasjoner = 'HoNOS: 6.Problemer forbundet med \n hallusinasjoner og vrangforestillinger',
                       H07Stemningsleie = 'HoNOS: 7.Problem med senket stemningsleie',
                       H08AndreProbl = 'HoNOS: 8.Andre mentale eller atferdsmessige problem',
                       H09ForhAndre = 'HoNOS: 9.Problemer med forhold til andre',
                       H10ADLProbl = 'HoNOS: 10. Problemer med dagliglivets aktiviteter',
                       H11BoligProbl = 'HoNOS: 11. Problemer med boligforhold',
                       H12YrkeProbl = 'HoNOS: 12. Problemer med yrke og aktiviteter')
}
      
      
if (valgtVar == 'H08aVelgTypeProbl') {                                                          #BRUKES I: Andeler
      retn <- 'H'
      grtxt <- c('Fobi', 'Angst', 'Tvangslidelse', 'Mentalt stress/spenninger', 'Dissosiativ', 'Somatoform',	
                 'Spiseproblemer', 'Søvnvansker', 'Seksuelt problem', 'Annet problem (Spesifiser)')
      
      #Velge kun gyldige rader:
      koder <- c(1:9,99)
      indDum <- which(RegData[ ,valgtVar] %in% koder)
      RegData <- RegData[indDum, ]
      
      RegData$VariabelGr <- RegData[ ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
      tittel <- 'HoNOS: 8a Velg type problem'
}


if (valgtVar %in% c('MedAntidepressiva', 'MedBenzodiazepiner', 'MedNevroleptika', 'MedAnnenMedBeh')) {
                                                                                                #BRUKES I: Andeler
                                                                                                #FORBEDRE 2: Én søyle med "ja" er nok (trenger ikke egen søyle for "nei"). 
      
                                                                                                #FORBEDRE 3: Alle medisinene i én figur.  
      #FORBEDRET med riktig Teller 25.08.2017, ved å  først velge RegData med kun de som har svart 0 eller 1 på MedPsykofarmaka
      koder <- c(0,1)
      indDum1<- which(RegData[ ,'MedPsykofarmaka'] %in% koder)
      RegData <- RegData[indDum1, ]
      
      #for variablen, e.g. MedAntideptressiva har vi da (antagelig) verdiene NA, 0,1
      #kod da/erstatt NA med verdien 0 
      
      #RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ] #LENA? Hjelpeargument?
      grtxt <- c('Nei', 'Ja')
      
      #Velge kun gyldige rader:
      koder <- c(0,1)
      indDum2 <- which(RegData[ ,valgtVar] %in% koder)
      RegData <- RegData[indDum2, ]
      
      RegData$VariabelGr <- RegData[ ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
      tittel <- switch(valgtVar,
                       MedAntidepressiva = 'Psykofarmaka: Antidepressiva',
                       MedBenzodiazepiner = 'Psykofarmaka: Benzodiazepiner' ,
                       MedNevroleptika = 'Psykofarmaka: Nevroleptika',
                       MedAnnenMedBeh = 'Psykofarmaka: Annen medisinsk behandling')
}

      
if (valgtVar =='MedPsykofarmaka') {                                                             #BRUKES I: Andeler
      grtxt <- c('Nei', 'Ja')
      koder <- c(0,1)
      indDum <- which(RegData[, valgtVar] %in% koder)
      RegData <- RegData[indDum, ]
      RegData$VariabelGr <- RegData[ ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
      tittel <- '"Psykofarmakologisk behandling?"'
}      

if (valgtVar == 'MedPsykofarmakaAlle' ) {                                                                    #BRUKES I: Andeler
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer 
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      # Vi kan velge å sende tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
      # Eller vi kan gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen
      koder <- c(0,1)
      indDum <- which(RegData[, 'MedPsykofarmaka'] %in% koder)
      RegData <- RegData[indDum, ]
      flerevar <- 1
      #Omkorder variabelen MedPsykofarmaka for å bruke den til en søyle for de som ikke har fått noen medikamentell beh.
      RegData$MedPsykofarmaka <- ifelse(RegData$MedPsykofarmaka ==1, 0, 1)
      
      variable <- c('MedAntidepressiva', 'MedBenzodiazepiner', 'MedNevroleptika', 'MedAnnenMedBeh', 'MedPsykofarmaka')
      #Sjekk <- RegData[,variable]
      retn <- 'V'
      grtxt <- c('Antidepressiva', 'Benzodiazepiner', 'Nevroleptika', 'Annen', 'Ingen')
      ind01 <- which(RegData[ ,variable] < 2, arr.ind = T) #Alle ja/nei
      ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
      RegData[ ,variable] <- NA
      #RegData[,variable] <- 
      RegData[ ,variable][ind01] <- 0
      RegData[ ,variable][ind1] <- 1
      #Beregne direkte:
      #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
      tittel <- 'Medikamentell behandling'
}      
      
#if (valgtVar=='MedBMI') {                            #? Usikker på bakgrunnen for denne variabelen - sjekk om den eksiterer i datasett - BMI-variabler fremkommer jo under...
#      gr <- c(0, 18.5, 25, 30, 1000)
#      #RegData$VariabelGr <- -1
#      ind <- which(RegData$MedBMI>0)
#      RegData <- RegData[ind, ]
#      #RegData$VariabelGr[ind] <- RegData$MedBMI[ind]
#      RegData$VariabelGr <- cut(RegData$MedBMI, breaks=gr, include.lowest=TRUE, right=FALSE)
#      # RegData$VariabelGr[ind] <- cut(RegData[ind ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
#      # RegData$VariabelGr <- cut(RegData[,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
#      # grtxt <- c('', '<18,5', levels(RegData$VariabelGr)[3:(length(gr)-2)],'30+')
#      grtxt <- c( '<18,5', '18,5-25', '25-30','30+')
#      grtxt2 <- c('Undervekt', 'Normalvekt', 'Overvekt', 'Fedme')
#      xAkseTxt <- "Body Mass Index"
#      tittel <-  'Pasientenes BMI (Body Mass Index)'
#}
      
      
if (valgtVar  %in% c('MedBMIStart','MedIsoBMIBGSStart','MedIsoBMICDCStart')) {            #BRUKES I: Andeler 
                                                                                          #Merknad: Ikke kjørt den rutinemessige "velg gyldige rader, deretter sørg for at tomme kategorier kommer med", men ser ut til å fungere likevel
      RegData <- RegData[which(RegData$RegRegtype > 1 & RegData$RegRegtype <=4), ] #velger kun startregistreringer og utredninger (1-4) (1=Utredning voksen, 2=Utredning ungdom/barn, 3=Startregistrering voksen, 4=Startregistrering ungdom/barn)
      valgtVar <- sub("Start", "", valgtVar) #fjerner "Start" fra navnet til Valgtvar slik at kommandoen to linjer nedenfor blir riktig
      gr <- c(0,seq(10,30,2),150)
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest= TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '30+')
      xAkseTxt <- switch(valgtVar, 
                         MedBMI = 'BMI',
                         MedIsoBMIBGS = 'iso-BMI (BGS)',
                         MedIsoBMICDC = 'iso-BMI (CDC)')
      tittel <- switch(valgtVar, 
                       MedBMI = 'BMI-fordeling ved innkomst',
                       MedIsoBMIBGS = 'Fordeling iso-BMI ved innkomst (normdata: Bergen Growth Study (BGS))',
                       MedIsoBMICDC = 'Fordeling iso-BMI ved innkomst \n (normdata: Centers for Disease Control and Prevention (CDC))')
}
      
if (valgtVar  %in% c('MedBMISlutt','MedIsoBMIBGSSlutt','MedIsoBMICDCSlutt')) {            #BRUKES I: Andeler 
                                                                                          #Merknad: Ikke kjørt den rutinemessige "fjern gyldige rader, deretter sørg for at tomme kategorier kommer med", men ser ut til å fungere likevel
      RegData <- RegData[which(RegData$RegRegtype == 5 | RegData$RegRegtype ==6 | RegData$RegRegtype ==98 | RegData$RegRegtype ==99), ] #velger kun sluttregistreringer og avbrudd (5=Sluttregistrering voksen, 6=Sluttregistrering ungdom/barn, 98=Avbrutt behandling voksen, 99=Avbrutt behandling ungdom/barn).Merk: Kan strengt tatt fjerne 98 og 99 her siden BMI ikke måles ved avbrudd, men beholder de for tydelighetens skyld
      valgtVar <- sub("Slutt", "", valgtVar) #fjerner "Slut" fra navnet til Valgtvar slik at kommandoen to linjer nedenfor blir riktig
      gr <- c(0,seq(10,30,2),150)
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest= TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '30+')
      xAkseTxt <- switch(valgtVar, 
                         MedBMI = 'BMI',
                         MedIsoBMIBGS = 'iso-BMI (BGS)',
                         MedIsoBMICDC = 'iso-BMI (CDC)')
      tittel <- switch(valgtVar, 
                       MedBMI = 'BMI-fordeling ved utskriving',
                       MedIsoBMIBGS = 'Fordeling iso-BMI ved utskriving (normdata: Bergen Growth Study (BGS))',
                       MedIsoBMICDC = 'Fordeling iso-BMI ved utskriving \n (normdata: Centers for Disease Control and Prevention (CDC))')
}

if (valgtVar == 'NegHend' ) {                                                                    #BRUKES I: Andeler
                                                                                                #MERK: Ikke kontrollert figurens korrekthet enda. 
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer 
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      #Vi kan velge å sende tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
      #Eller vi kan gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen
      flerevar <- 1
      variable <- c('B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing')
      #Sjekk <- RegData[,variable]
      retn <- 'H'
      grtxt <- c('Fysisk mishandl.', 'Psykisk mishandl.', 'Overgrep', 'Mobbing')
      ind01 <- which(RegData[ ,variable] < 2, arr.ind = T) #Alle ja/nei
      ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
      RegData[ ,variable] <- NA
      #RegData[,variable] <- 
      RegData[ ,variable][ind01] <- 0
      RegData[ ,variable][ind1] <- 1
      #Beregne direkte:
      #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
      tittel <- 'Negative hendelser'
}
      
#UNDER UTARBEIDING: flere/antall negative hendelser

#if (valgtvar == 'AntallNegHend') {
#      flerevar <- 1
#      variable <- c('B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing')
#      retn <- 'V'
#      ind01 <- which(RegData[ ,variable] < 2, arr.ind = T) #Alle ja/nei
#      ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
#      RegData[ ,variable] <- NA
#     
#      RegData[ ,variable][ind01] <- 0
#      RegData[ ,variable][ind1] <- 1

#     grtxt <- c('0','1','2','3','4')
#      tittel <- 'Antall negative hendelser'
      
#}
      

if (valgtVar=='Norsktalende') {                                                                  #BRUKES I: Andeler
                                                                                                 #REPARERE (hvis vi ønsker denne variabelen): Kan være feil, og må sjekke om prod data har kodet denne numerisk (gamle testdata brukt når laget figur)
      #0=Nei, 1=Ja, 2= Delvis, 9=Ukjent
      grtxt <- c('Nei','Ja', 'Delvis', 'Ukjent')
      
      #Velge kun gyldige rader:
      koder <- c('Nei','Ja', 'Delvis', 'Ukjent')
      indDum <- which(RegData$Norsktalende %in% koder)
      RegData <- RegData[indDum, ]
      
      RegData$Norsktalende <- as.character(RegData$Norsktalende)
      
      RegData$VariabelGr <- RegData$Norsktalende
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = koder)
      tittel <- 'Norsktalende'
}

      
     
if (valgtVar == 'pasienttilfredshet') {                                                          #BRUKES I: Andeler
                                                                                                #IKKE KLAR. Startet på, men må endre til en figur med Pasienttilfredshet (flerevar=1) 
      #Her har vi ulik N for de ulike variablene. 
      flerevar <- 1 
      variable <- c('PT01OnsketInvolv', 'PT02BleInvolv', 'PT04KontaktBrukerorg', 
                    'PT05OrientertBrukerorg') 
      grtxt <- c('Ønske om andre nære i beh.', 'Nære involvert i beh.', 'Kontakt med brukerorg.?',
                 'Fått info om brukerorg.')
      indDum <- which(RegData[ ,valgtVar] %in% c(0,1))
      RegData <- RegData[indDum, ]
      RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = c(0,1))
      tittel <- 'Pasienttilfredshet' 
}

#Lage indekser for:PO01Forstod	PO02Tillit	PO03InfoDiagnose	PO04Tilpasset	PO05Involvert	
#PO06Organisert	PO07Tilfredsstillende	PO08Tilgjengelighet	PO09Utbytte	PO10Pasientsikkerhet

#Endre til en figur med Pasienttilfredshet (flerevar=1)      
if (valgtVar %in% c('PT01OnsketInvolv', 'PT02BleInvolv', 'PT04KontaktBrukerorg', 
                    'PT05OrientertBrukerorg')) {                                                #BRUKES I: Andeler
                                                                                                #Merknad: Ikke kjørt den rutinemessige "fjern gyldige rader, deretter sørg for at tomme kategorier kommer med"
      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
      grtxt <- c('Nei', 'Ja')
      indDum <- which(RegData[ ,valgtVar] %in% c(0,1))
      RegData <- RegData[indDum, ]
      RegData$VariabelGr <- factor(RegData[ ,valgtVar], levels = c(0,1))
      tittel <- switch(valgtVar,
                       PT01OnsketInvolv = 'Pasienttilfredshet: Ønske om involvering av andre nære i behandlingen',
                       PT02BleInvolv = 'Pasienttilfredshet: Ble nære involvert i behandlingen?',
                       PT04KontaktBrukerorg = 'Pasienttilfredshet: Noen gang kontak med brukerorganisasjoner?',
                       PT05OrientertBrukerorg = 'Pasienttilfredshet: Informasjon om brukerorganisasjoner ila. behandlingen?')
}


if (valgtVar=='PT03Utfallsvurd') {                                                   #BRUKES I: Andeler
                                                                                     #Merknad: Ikke kjørt den rutinemessige "fjern gyldige rader, deretter sørg for at tomme kategorier kommer med"
      grtxt <- c('Ikke noe problem lenger', 'Klar bedring', 'Noe bedring', 'Uendret', 'Forverring')
      # RegData$VariabelGr <- 99
      indDum <- which(RegData$PT03Utfallsvurd %in% c(1:5))
      RegData <- RegData[indDum, ]
      RegData$VariabelGr <- factor(RegData$PT03Utfallsvurd, levels = c(1:5))
      tittel <- 'Pasienttilfredshet: "Hvordan vurderer du utfallet av mottatt behandling?"'
}
      
if (valgtVar %in% c('RAND36FysFunk', 
                    'RAND36RollebegFys', 
                    'RAND36RollebegEmo', 
                    'RAND36Tretthet', 
                    'RAND36MentalHelse', 
                    'RAND36SosialFunk', 
                    'RAND36Smerte', 
                    'RAND36GenHelse', 
                    'RAND36EndringHelse'))
{                                                                                         #BRUKES I: GjsnGrVar; 
                                                                                          #Merknad: Ikke kjørt den rutinemessige "fjern ugyldige rader" først
      RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
      RegData$Variabel <- RegData[ ,valgtVar]
      deltittel <- switch(valgtVar, 
                          RAND36FysFunk = 'skåre: Global skåre, RAND-36',
                          RAND36RollebegFys = 'skåre: Rollefungering (fysisk), RAND-36',
                          RAND36RollebegEmo = 'skåre: Rollefungering (emosjonelt), RAND-36',
                          RAND36Tretthet = 'skåre: Vitalitet, RAND-36',
                          RAND36MentalHelse = 'skåre: Mental helse, RAND-36',
                          RAND36SosialFunk = 'skåre: Sosial fungering, RAND-36',
                          RAND36Smerte = 'skåre: Smerte, RAND-36',
                          RAND36GenHelse = 'skåre: Generell helse, RAND-36',
                          RAND36EndringHelse ='skåre: Endring i helse, RAND-36')
      xaksetxt <- switch(valgtVar, 
                         RAND36FysFunk = 'Global skåre',
                         RAND36RollebegFys = 'Rollefungering (fysisk)',
                         RAND36RollebegEmo = 'Rollefungering (emosjonelt)',
                         RAND36Tretthet = 'Vitalitet',
                         RAND36MentalHelse = 'Mental helse',
                         RAND36SosialFunk = 'Sosial fungering',
                         RAND36Smerte = 'Smerte',
                         RAND36GenHelse = 'Generell helse',
                         RAND36EndringHelse ='Endring i helse')
}
      
      
if (valgtVar == 'RCIEDEQ60GlobalScore') {
      retn <- 'V'
      grtxt <- c('Reliabel \nforverring', 'Sannsynlig \nforverring', 'Ingen endring', 'Sannsynlig\npositiv endring', 'Reliabel \npositiv endring')
            
      RegDataStart <- RegData[which(RegData$RegRegtype >= 3 & RegData$RegRegtype <=4), ] #lager dataramme med kun startregistreringer
      RegDataSlutt <- RegData[which(RegData$RegRegtype >= 5 & RegData$RegRegtype <=6), ] #lager dataramme med kun slttregistreinger ((5=Sluttregistrering voksen, 6=Sluttregistrering ungdom/barn, 98=Avbrutt behandling voksen, 99=Avbrutt behandling ungdom/barn).)
      RegData <- merge(RegDataStart, RegDataSlutt, by="PasientID", suffixes = c('','.y'))
            
      RegData$edeqtemp <- (RegData$EDEQ60GlobalScore.y - RegData$EDEQ60GlobalScore)/0.7405 #basert på Riksät-syntaks. Endre standard error of diff til norsk.
      RegData$edeqtemp[RegData$edeqtemp >=1.96] <- 1
      RegData$edeqtemp[RegData$edeqtemp <1.28 & RegData$edeqtemp >=-1.28] <- 2
      RegData$edeqtemp[RegData$edeqtemp < 1.96 & RegData$edeqtemp >-1.28] <- 3
      RegData$edeqtemp[RegData$edeqtemp <= -1.28 & RegData$edeqtemp > -1.96] <- 4
      RegData$edeqtemp[RegData$edeqtemp <= -1.96] <- 5
            
      #sortere bort ugyldige rader
      koder <- c(1:5)
      indDum <- which(RegData$edeqtemp %in% koder)#
      RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige 
            
      RegData$VariabelGr <- factor(RegData$edeqtemp, levels = koder)
            
      tittel <-'RCI EDEQ60GlobalScore'
}
      
if (valgtVar == 'RCICIA30GlobalScore') {
  retn <- 'V'
  grtxt <- c('Reliabel \nforverring', 'Sannsynlig \nforverring', 'Ingen endring', 'Sannsynlig\npositiv endring', 'Reliabel \npositiv endring')
            
      RegDataStart <- RegData[which(RegData$RegRegtype >= 3 & RegData$RegRegtype <=4), ] #lager dataramme med kun startregistreringer
      RegDataSlutt <- RegData[which(RegData$RegRegtype >= 5 & RegData$RegRegtype <=6), ] #lager dataramme med kun slttregistreinger ((5=Sluttregistrering voksen, 6=Sluttregistrering ungdom/barn, 98=Avbrutt behandling voksen, 99=Avbrutt behandling ungdom/barn).)
      RegData <- merge(RegDataStart, RegDataSlutt, by="PasientID", suffixes = c('','.y'))
            
      RegData$ciatemp <- (RegData$CIA30GlobalScore.y - RegData$CIA30GlobalScore)/4.9475 #basert på Riksät-syntaks. Endre standard error of diff til norsk.
      RegData$ciatemp[RegData$ciatemp >=1.96] <- 1
      RegData$ciatemp[RegData$ciatemp<1.28 & RegData$ciatemp >=-1.28] <- 2
      RegData$ciatemp[RegData$ciatemp < 1.96 & RegData$ciatemp >-1.28] <- 3
      RegData$ciatemp[RegData$ciatemp <= -1.28 & RegData$ciatemp > -1.96] <- 4
      RegData$ciatemp[RegData$ciatemp <= -1.96] <- 5
            
      #sortere bort ugyldige rader
      koder <- c(1:5)
      indDum <- which(RegData$ciatemp %in% koder)#
      RegData <- RegData[indDum,] #velger de gyldige radene/fjerner de ugyldige 
            
      RegData$VariabelGr <- factor(RegData$ciatemp, levels = koder)
            
      tittel <-'RCI CIA30GlobalScore'
}

if (valgtVar=='RegHenvInstans') { #1 Pasienten selv, 2 Fastlege/primærlege, 3	Øvrig primærhelsetjenste, 4 Spesialisthelsetjenesten,
      # 5	Barnehage / skolesektor/PPT, 6 Sosialtjeneste / barnevern, 7 Politi/fengsel/rettsvesen, 
      # 8 Rehabiliteringsinstitusjon/sykehjem, 9Privatpraktiserende spesialister, 99 Annet
                                                                                     #BRUKES I: Andeler
                                                                                     # MANGLER: (Rettet 28.06 slik at tom kat. kommer med, men...) kategoriene kommer ikke i den rekkefølgen de bør(slik de er listet i spørreskjemaene)
      retn <- 'H'
      grtxt <- c('Pasienten selv', 'Fastlege/primærlege', 'Øvrig primærhelsetjeneste', 'Spesialisthelsetjenesten', 
                 'Barnehage/skolesektor/PPT', 'Sosialtjeneste/barnevern', 'Politi/fengsel/rettsvesen', 'Rehabiliteringsinstitusjon/\nsykehjem', 'Privatpraktiserende \nspesialister', 'Annet')
      
      #sortere bort ugyldige rader:
      koder <- c(1:9,99)
      indDum <- which(RegData$RegHenvInstans %in% koder)
      RegData <- RegData[indDum,]

      RegData$VariabelGr <- RegData$RegHenvInstans      
      RegData$VariabelGr <- factor(RegData$RegHenvInstans,levels=koder)
      
      xAkseTxt <- 'Henvisende instans'
      tittel <- 'Henvisende instans'
}
      
if (valgtVar %in% c('SCL90TGSI',
                    'SCL90TSomatisering', 
                    'SCL90TTvang',
                    'SCL90TSensitivitet',
                    'SCL90TDepresjon',
                    'SCL90TAngst', 
                    'SCL90TFiendlighet', 
                    'SCL90TFobi', 
                    'SCL90TParanoia', 
                    'SCL90TPsykotisk'))                           
{                                                                                         #BRUKES I: GjsnGrVar; 
                                                                                          #Merknad: Ikke kjørt den rutinemessige "fjern ugyldige rader" først
      RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
      RegData$Variabel <- RegData[ ,valgtVar]
      deltittel <- switch(valgtVar, 
                          SCL90TGSI = 'symptomtrykk: Global Severity Index, SCL-90-R',
                          SCL90TSomatisering = 'symptomtrykk: Somatisering, SCL-90-R,',
                          SCL90TTvang = 'symptomtrykk: Tvang, SCL-90-R',
                          SCL90TSensitivitet = 'symptomtrykk: Sensitivitet, SCL-90-R',
                          SCL90TDepresjon = 'symptomtrykk: Depresjon, SCL-90-R',
                          SCL90TAngst = 'symptomtrykk: Angst, SCL-90-R',
                          SCL90TFiendlighet = 'symptomtrykk: Fiendtlighet, SCL-90-R',
                          SCL90TFobi = 'symptomtrykk: Fobi, SCL-90-R',
                          SCL90TParanoia = 'symptomtrykk: Paranoia, SCL90-R',
                          SCL90TPsykotisk = 'symptomtrykk: Psykotisisme, SCL90-R')
      xaksetxt <- switch(valgtVar, 
                         SCL90TGSI = 'Global Severity Index (T-skår; mean=50, std=10)',
                         SCL90TSomatisering = 'Somatisering (T-skår; mean=50, std=10)',
                         SCL90TTvang = 'Tvang (T-skår; mean=50, std=10)',
                         SCL90TSensitivitet = 'Sensitivitet (T-skår; mean=50, std=10)',
                         SCL90TDepresjon = 'Depresjon (T-skår; mean=50, std=10)',
                         SCL90TAngst = 'Angst (T-skår; mean=50, std=10)',
                         SCL90TFiendlighet = 'Fiendtlighet (T-skår; mean=50, std=10)',
                         SCL90TFobi = 'Fobi (T-skår; mean=50, std=10)',
                         SCL90TParanoia = 'Paranoia (T-skår; mean=50, std=10)',
                         SCL90TPsykotisk = 'Psykotisisme (T-skår; mean=50, std=10)')
}                                                                                          
      
if (valgtVar=='TidSykBehandling'){                                                              #BRUKES I: Andeler            #Kvalitetsindikator("hvor godt/tidlig fanger man opp de syke?"). Interessant med tidsserie og sammenligning mellom enheter.)
                                                                                                #Merknad: Ikke kjørt den rutinemessige "fjern ugyldige rader" først
      RegData$TidSykBehandling<-RegData$B12dArTilBehstart*12+RegData$B12dMndTilBehstart #lager variabel som gir år + måneder fra sykdom til behandlingsstart
      gr <- c(0,6,12,18,24,30,36,500)
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '36+')
      xAkseTxt <-'Måneder'
      tittel <- 'Tid fra sykdomsdebut til behandlingsstart (selvrapportert)' 
}

if (valgtVar=='VentetidKat') {                                                                  #BRUKES I: Andeler
                                                                                                #Merknad: Ikke kontrollert korrekthet og ikke kjørt den rutinemessige "fjern ugyldige rader" først
                                                                                                #Kvalitetsindikatorområdet ventetid?
      RegData$Ventetid <- difftime(strptime(RegData$RegHendelsesdato, format = "%Y-%m-%d"),     
                                   strptime(RegData$RegHenvMottattDato, format = "%Y-%m-%d"),units="weeks")    # tid fra henvisning til start av behandlings eller utredning - variabel kalkulert av Mads: Forskjellem "henvisning mottatt dato" og "hendelsesdato" 
      RegData$Ventetid <- as.numeric(RegData$Ventetid, units="weeks") #must make the atomic vector Ventetid numeric
      gr <- c(0,seq(2,16,2),150)
      RegData$VariabelGr <- cut(RegData$Ventetid, breaks=gr, include.lowest= TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '16+')
      xAkseTxt <- 'Uker'
      tittel <- 'Ventetid: Tid fra "henvisning mottatt" til utredningdato/behandlingsstart'
}

if (valgtVar=='VentetidOver2Uker') {                                                            #BRUKES I: AndelerGrVar       #Kvalitetsindikator
                                                                                                #Merknad: Ikke kjørt den rutinemessige "fjern ugyldige rader" først
      RegData$Ventetid <- difftime(strptime(RegData$RegHendelsesdato, format = "%Y-%m-%d"),
                                   strptime(RegData$RegHenvMottattDato, format = "%Y-%m-%d"),units="weeks")    # tid fra henvisning til start av behandlings eller utredning - variabel kalkulert av Mads: Forskjellem "henvisning mottatt dato" og "hendelsesdato" 
      RegData$Ventetid <- as.numeric(RegData$Ventetid, units="weeks") #must make the atomic vector Ventetid numeric
      RegData$Variabel <- 99
      RegData$Variabel[which(RegData$Ventetid>2)] <- 1 
      tittel <- 'Ventetid over 2 uker'
}


      

      
      
      

UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, retn=retn, #KImaal=KImaal, 
               tittel=tittel, deltittel=deltittel, variable= variable, flerevar=flerevar)  #, sortAvtagende=sortAvtagende
#RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
return(invisible(UtData)) 
      
}
      