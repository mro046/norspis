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
      variable <- 'Ingen'
      

      
#--------FigGjsnGrVar: Tilrettelegging av variabler til FigGjsnGrVar
if (valgtVar == 'AlderGjsn') { #endret fra alder fordi annen tiltretteligging heter alder.
      #Alle skal ha alder
      #Legeskjema.
      RegData <- RegData[which(RegData[ ,'Alder'] >0), ]
      RegData$Variabel <- RegData[ ,'Alder']
      deltittel <- 'alder'
      xaksetxt <- 'Alder (år)'
}
      
if (valgtVar =='B08StartAldrProbl'){
      RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
      RegData$Variabel <- RegData[ ,valgtVar]
      deltittel <- 'alder ved start av problematikk'
      xaksetxt <- 'Alder (år)'
}
      
if (valgtVar =='B12cAldrForsteBeh'){
      RegData <- RegData[which(RegData[ ,valgtVar] >0), ]
      RegData$Variabel <- RegData[ ,valgtVar]
      deltittel <- 'alder ved første behandling'
      xaksetxt <- 'Alder (år)'
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
{
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
      
if (valgtVar %in% c('EDEQ60GlobalScore',
                    'EDEQ60Restriksjon', 
                    'EDEQ60Kroppsform', 
                    'EDEQ60Spising', 
                    'EDEQ60Vekt'))
{
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
      
      
if (valgtVar %in% c('RAND36FysFunk', 
                    'RAND36RollebegFys', 
                    'RAND36RollebegEmo', 
                    'RAND36Tretthet', 
                    'RAND36MentalHelse', 
                    'RAND36SosialFunk', 
                    'RAND36Smerte', 
                    'RAND36GenHelse', 
                    'RAND36EndringHelse'))
{
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
    
      
      
      
#      
#     
#        
#--------FigAndelerGrVar: (Tilrettelegging av variabel som) brukes i figurtypen FigAndelerGrVar:
      
if (valgtVar=='alder_u18') {	#brukes i: NorspisFigAndelerGrVar
      RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
      RegData$Variabel[which(RegData$Alder<18)] <- 1 
      tittel <- 'Pasienter under 18 år'
}
      
if (valgtVar=='BehDodUnderBeh') {	#brukes i: NorspisFigAndelerGrVar
      RegData <- RegData[which(RegData$BehDodUnderBeh>=0), ]    #tar bort eventuelle verdier som er <0
      RegData$Variabel[which(RegData$BehDodUnderBeh==1)] <- 1 
      tittel <- 'Mortalitet'
}      
      
if (valgtVar=='DiagVDiabetes') {    #brukes i: NorspisFigAndelerGrVar
      RegData$Variabel <- RegData$DiagVDiabetes
      tittel <- 'Diabetes'
      sortAvtagende <- FALSE
}
      

#avsluttet her24.03.17 - fungerer ikke 
if (valgtVar=='VentetidOver2Uker') { #brukes i: NorspisFigAndelerGrVar
      RegData$Ventetid <- difftime(strptime(RegData$RegHendelsesdato, format = "%Y-%m-%d"),
                                   strptime(RegData$RegHenvMottattDato, format = "%Y-%m-%d"),units="weeks")    # tid fra henvisning til start av behandlings eller utredning - variabel kalkulert av Mads: Forskjellem "henvisning mottatt dato" og "hendelsesdato" 
      RegData$Variabel[which(RegData$Ventetid>2)] <- 1 
      
      tittel <- 'Ventetid over 2 uker'
}

  

#--------FigAndeler: (Tilrettelegging av variabel som) brukes i figurtypen FigAndeler:

if (valgtVar=='Alder') {
gr <- c(0,seq(10,50,10),150)
RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
#grtxt <- c('0-11', '11-12', '13-14', '15-16', '17-18', '19-20', '21-25', '26-30', '31-35', '36-40', '41-45', '46-50', '51-60, '61+')
grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '50+')	#c(names(AndelLand)[-length(gr)], '90+')
xAkseTxt <- 'Aldersgruppe'
tittel <- 'Aldersfordeling'

}


if (valgtVar  %in% c('MedBMIStart','MedIsoBMIBGSStart','MedIsoBMICDCStart')) {
      RegData <- RegData[which(RegData$RegRegtype >= 1 & RegData$RegRegtype <=4), ] #velger kun startregistreringer og utredninger (1=Utredning voksen, 2=Utredning ungdom/barn, 3=Startregistrering voksen, 4=Startregistrering ungdom/barn)
      valgtVar <- sub("Start", "", valgtVar) #fjerner "Start" fra navnet til Valgtvar slik at kommandoen to linjer nedenfor blir riktig
      gr <- c(0,seq(10,30,2.5),150)
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest= TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '30+')
      xAkseTxt <- switch(valgtVar, 
                         MedBMI = 'BMI',
                         MedIsoBMIBGS = 'iso-BMI (BGS)',
                         MedIsoBMICDC = 'iso-BMI (CDC)')
      tittel <- switch(valgtVar, 
                       MedBMI = 'BMI-fordeling ved innkomst',
                       MedIsoBMIBGS = 'Fordeling iso-BMI ved innkomst (normdata: Bergen Growth Study (BGS))',
                       MedIsoBMICDC = 'Fordeling iso-BMI ved innkomst (normdata: Centers for Disease Control and Prevention (CDC))')
}

if (valgtVar  %in% c('MedBMISlutt','MedIsoBMIBGSSlutt','MedIsoBMICDCSlutt')) {
      RegData <- RegData[which(RegData$RegRegtype == 5 | RegData$RegRegtype ==6 | RegData$RegRegtype ==98 | RegData$RegRegtype ==99), ] #velger kun sluttregistreringer og avbrudd (5=Sluttregistrering voksen, 6=Sluttregistrering ungdom/barn, 98=Avbrutt behandling voksen, 99=Avbrutt behandling ungdom/barn).Merk: Kan streng tatt fjerne 98 og 99 her siden BMI ikke måles ved avbrudd
      valgtVar <- sub("Slutt", "", valgtVar) #fjerner "Slut" fra navnet til Valgtvar slik at kommandoen to linjer nedenfor blir riktig
      gr <- c(0,seq(10,30,2.5),150)
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest= TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '30+')
      xAkseTxt <- switch(valgtVar, 
                         MedBMI = 'BMI',
                         MedIsoBMIBGS = 'iso-BMI (BGS)',
                         MedIsoBMICDC = 'iso-BMI (CDC)')
      tittel <- switch(valgtVar, 
                       MedBMI = 'BMI-fordeling ved utskriving',
                       MedIsoBMIBGS = 'Fordeling iso-BMI ved utskriving (normdata: Bergen Growth Study (BGS))',
                       MedIsoBMICDC = 'Fordeling iso-BMI ved utskriving (normdata: Centers for Disease Control and Prevention (CDC))')
}
      
      
#if (valgtVar=='MedBMI') {
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

      
if (valgtVar=='VentetidKat') {
      RegData$Ventetid <- difftime(strptime(RegData$RegHendelsesdato, format = "%Y-%m-%d"),
                                   strptime(RegData$RegHenvMottattDato, format = "%Y-%m-%d"),units="weeks")    # tid fra henvisning til start av behandlings eller utredning - variabel kalkulert av Mads: Forskjellem "henvisning mottatt dato" og "hendelsesdato" 
      RegData$Ventetid <- as.numeric(RegData$Ventetid, units="weeks") #must make the atomic vector Ventetid numeric
      gr <- c(0,seq(2,16,2),150)
      RegData$VariabelGr <- cut(RegData$Ventetid, breaks=gr, include.lowest= TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '16+')
      xAkseTxt <- 'Uker'
      tittel <- 'Ventetid'
}

if (valgtVar=='RegHenvInstans') { #1 Pasienten selv, 2 Fastlege/primærlege, 3	Øvrig primærhelsetjenste, 4 Spesialisthelsetjenesten,
      # 5	Barnehage / skolesektor/PPT, 6 Sosialtjeneste / barnevern, 7 Politi/fengsel/rettsvesen, 
      # 8 Rehabiliteringsinstitusjon/sykehjem, 9Privatpraktiserende spesialister, 99 Annet
      retn <- 'H'
      RegData$VariabelGr <- RegData$RegHenvInstans
      grtxt <- c('Pasienten selv', 'Fastlege/primærlege', 'Øvrig primærhelsetjeneste', 'Spesialisthelsetjenesten', 
                 'Barnehage/skolesektor/PPT', 'Sosialtjeneste/barnevern', 'Politi/fengsel/rettsvesen', 'Annet')
      xAkseTxt <- 'Henvisende instans'
      tittel <- 'Henvisende instans'
}

if (valgtVar=='B02EgneBarn') {
      gr <- c(0:5,15)
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
      grtxt <- c(0:4, '5+')	#c(names(AndelLand)[-length(gr)], '90+')
      xAkseTxt <- 'Barn'
      tittel <- 'Antall egne barn'
}      

if (valgtVar=='B03Bosituasjon') {
      #Alternativer: 1	Hos en av foreldrene, 2 Hos begge foreldre, 3 Bor alene, 4  Med partner, 
      #5 Med partner og barn, 6 Uten partner med barn, 9 Annen
      retn <- 'H'
      grtxt <- c ('Hos en av foreldrene', 'Hos begge foreldre', 'Bor alene', 'Med partner', 
                  'Med partner og barn', 'Uten partner med barn', 'Annen')
      #RegData$VariabelGr <- 99
      indDum <- which(RegData$B03Bosituasjon %in% c(1:6,9))
      RegData$VariabelGr[indDum] <- RegData$B03Bosituasjon[indDum]
      #RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:6,9))
      #xAkseTxt <- 'Bosituasjon'
      tittel <- 'Bosituasjon'
}

      
      

if (valgtVar %in% c('B04PabegyntUtd', 'B05FullfortUtd')) {
      #OBS - se over/korrigere kategoriene
      retn <- 'H'
      grtxt <- switch(valgtVar, 
                      B04PabegyntUtd=c('Grunnskole','Videregående skole (1-3 år)',
                                       'Høgskole eller universitet, \nmindre enn 4 år', 
                                       'Høgskole eller universitet, \n4 år eller mer','Ukjent'),
                      B05FullfortUtd=c('Ikke fullført grunnskole','Grunnskole','Videregående skole (1-3 år)',
                                       'Høgskole eller universitet, \nmindre enn 4 år', 
                                       'Høgskole eller universitet, \n4 år eller mer','Ukjent'))
      verdier <- switch(valgtVar,
                        B04PabegyntUtd=c(1:4,9),
                        B05FullfortUtd=c(1:5,9))
      indDum <- which(RegData[ ,valgtVar] %in% verdier)
      RegData <- RegData[indDum, ]
      RegData$VariabelGr <- RegData[ ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = verdier)
      tittel <- 'Høyeste påbegynte utdanning'
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
      

if (valgtVar %in% c('B08StartAldrProbl', 'B12cAldrForsteBeh')) {
      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
      gr <- c(0,seq(5,30,5),150)
      #indDum <- which(RegData[ ,valgtVar] %in% c(1:150))
      #RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '30+')	#c(names(AndelLand)[-length(gr)], '90+')
      xAkseTxt <- 'Aldersgruppe'
      tittel <- switch(valgtVar,
                       B08StartAldrProbl = 'Alder da problemene startet',
                       B12cAldrForsteBeh = 'Tidligere behandling: Alder ved start av første behandling')
}

if (valgtVar=='TidSykBehandling'){
      RegData$TidSykBehandling<-RegData$B12dArTilBehstart*12+RegData$B12dMndTilBehstart #lager variabel som gir år + måneder fra sykdom til behandlingsstart
      gr <- c(0,6,12,18,24,30,36,500)
      RegData$VariabelGr <- cut(RegData[ ,valgtVar], breaks=gr, include.lowest=TRUE, right=FALSE)
      grtxt <- c(levels(RegData$VariabelGr)[-(length(gr)-1)], '36+')
      xAkseTxt <-'Måneder'
      tittel <- 'Tid fra sykdomsdebut til behandlingsstart' 
}

if (valgtVar=='B11FamilieSF') {
      retn <- 'V'
      grtxt <- c('Nei', 'Ja', 'Vet ikke')
      #RegData$VariabelGr <- 99
      indDum <- which(RegData$B11FamilieSF %in% c(0,1,9))
      RegData$VariabelGr[indDum] <- RegData$B11FamilieSF[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9))
      tittel <- 'Spiseforstyrrelse hos andre i familien?'
}

if (valgtVar=='B12TidlBehSF') {
      grtxt <- c('Nei', 'Ja', 'Ukjent')
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
      retn <- 'V'
      #  RegData <- RegData[which(RegData$ErOppflg == 0), ] #LENA? Hjelpeargument?
      grtxt <- c('Nei', 'Ja', 'Ukjent', 'Ikke reg.')
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
      

if (valgtVar=='Norsktalende') {
      #0=Nei, 1=Ja, 2= Delvis, 9=Ukjent
      grtxt <- c('Nei','Ja', 'Delvis', 'Ukjent')
      indDum <- which(as.character(RegData$Norsktalende) %in% grtxt[-1])
      RegData$VariabelGr[indDum] <- RegData$Norsktalende[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:2,9))
      tittel <- 'Norsktalende'
}



if (valgtVar == 'NegHend' ) {
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
      
      
      
#Endre til en figur med Pasienttilfredshet (flerevar=1)      
if (valgtVar %in% c('PT01OnsketInvolv', 'PT02BleInvolv', 'PT04KontaktBrukerorg', 
                    'PT05OrientertBrukerorg')) {
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
#STARTET Endre til en figur med Pasienttilfredshet (flerevar=1)      
if (valgtVar == 'pasienttilfredshet') {
      #IKKE KLAR!!
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

if (valgtVar=='DiagVSF') {
      grtxt <- c('F500', 'F509')
      RegData$DiagVSF <- as.character(RegData$DiagVSF)
      RegData$VariabelGr <- 'Andre Diag'
      indDum <- which(RegData$DiagVSF %in% grtxt)
      RegData$VariabelGr[indDum] <- RegData$DiagVSF[indDum]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(grtxt, 'Andre Diag'))
      tittel <- 'Pasienttilfredshet: "Hvordan vurderer du utfallet av mottatt behandling?"'
}

            if (valgtVar=='PT03Utfallsvurd') {
            grtxt <- c('Ikke noe problem lenger', 'Klar bedring', 'Noe bedring', 'Uendret', 'Forverring')
           # RegData$VariabelGr <- 99
            indDum <- which(RegData$PT03Utfallsvurd %in% c(1:5))
            RegData <- RegData[indDum, ]
            RegData$VariabelGr <- factor(RegData$PT03Utfallsvurd, levels = c(1:5))
            tittel <- 'Pasienttilfredshet: "Hvordan vurderer du utfallet av mottatt behandling?"'
      }
      

            

      
#if (MedPsykofarmaka == 1) Hvordan legge inn betingelsen?
if (valgtVar %in% c('MedAntidepressiva', 'MedBenzodiazepiner', 'MedNevroleptika', 'MedAnnenMedBeh')) {
      #RegData <- RegData[which(RegData$PasientSkjemaStatus == 1), ] #LENA? Hjelpeargument?
      grtxt <- c('Nei', 'Ja','Ukjent')
      RegData$VariabelGr <- 99
      indDum <- which(RegData[ ,valgtVar] %in% 0,1)
      RegData$VariabelGr[indDum] <- RegData[indDum ,valgtVar]
      RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,99))
      tittel <- switch(valgtVar,
                       MedAntidepressiva = 'Antidepressiva',
                       MedBenzodiazepiner = 'Benzodiazepiner' ,
                       MedNevroleptika = 'Nevroleptika',
                       MedAnnenMedBeh = 'Annen medisinsk behandling')
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

if (valgtVar=='B01Sivilstatus') {
      grtxt <- c('Enslig','Samboer','Gift','Skilt','Enke/enkemann','Annen')
      RegData$VariabelGr <- factor(RegData$B01Sivilstatus, levels = c(1:5,9))
      tittel <- 'Sivilstatus'
}
      
#Lage indekser for:PO01Forstod	PO02Tillit	PO03InfoDiagnose	PO04Tilpasset	PO05Involvert	
#PO06Organisert	PO07Tilfredsstillende	PO08Tilgjengelighet	PO09Utbytte	PO10Pasientsikkerhet


      
UtData <- list(RegData=RegData, grtxt=grtxt, xAkseTxt=xAkseTxt, retn=retn, #KImaal=KImaal, 
               tittel=tittel, variable= variable, flerevar=flerevar)  #, sortAvtagende=sortAvtagende
#RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
return(invisible(UtData)) 
      
}