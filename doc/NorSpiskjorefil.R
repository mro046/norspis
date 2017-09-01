#library(rapbase)


#-------------------------Hente data-------------------------------
rm(list=ls())

library(norspis)
NorSpisForlop <- read.table('C:/Users/spa-ressp-2/NorSpisTestData/ForlopsOversikt2017-02-01.csv', sep=';',
                            header=T, encoding = 'UTF-8')
NorSpisEnkeltledd <- read.table('C:/Users/spa-ressp-2/NorSpisTestData/EnkeltLeddNum2017-02-01.csv', sep=';', 
                                header=T, encoding = 'UTF-8') 
NorSpisAlleScorer <- read.table('C:/Users/spa-ressp-2/NorSpisTestData/AlleScorer2017-02-01.csv', sep=';', header=T, encoding = 'UTF-8') #,


#-------------------------Merge data-------------------------------
ForlAlleSc <- merge(NorSpisForlop, NorSpisAlleScorer, suffixes = c('','X'), by = "ForlopsID", all = FALSE)
NorSpisData <- merge(ForlAlleSc, NorSpisEnkeltledd, suffixes = c('','X'), by = "ForlopsID", all = FALSE)
#NorSpisData <- merge(NorSpisForlop, suffixes = c('','X'), by = c("ForlopsID" ), NorSpisEnkeltledd, 
#                     all = FALSE)  #by.x = "ForlopsID", by.y = "ForlopsID",#"SykehusNavn", "AvdRESH"

#------------------------Endre navn-------------------------------
RegData <- NorSpisData

#------------------------setwd-----------------------------------
setwd('C:/Users/spa-ressp-2/Documents/norspis/')

#------------------------Definere parametre
# Inndata til funksjon:
reshID <- 'TESTNO'  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 200	#alder, til og med
erMann <- ''
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2017-12-31'
preprosess <- 1
hentData <- 0
#? tittel=1  
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
                  #		6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten

outfile <-'' #paste(valgtVar, '_ford.png', sep='')#Navn angis av Jasper #tom=lagres ikke men kommer opp på skjermen

#I--------------------teste enkeltvariabler


#1--------------------Teste NorSpisFigAndeler

valgtVar <- 'SCL90TGSI'	#Må velge... B03Bosituasjon'B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing', 
#'B21SelvskadTidl', 'B22SelvskadSisteAr', 'B23SelvmordFTidl', 'B24SelvmordFSisteAr', 
#'B25Avhengighet

NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, minald=minald, maxald=maxald)

#2-------------------- TESTE FIGURER HVOR ET MÅL (gjsn, median, andeler) ER GRUPPERT PÅ BEHANDLINGSENHETER (evt. annnen grupperingsvariabel)

grVar <- 'SykehusNavn'             #sette variablen man ønsker å gruppere på


#2.1------------------ Teste NorSpisFigAndelerGrVar (andeler per enhet)

#setwd('C:/Users/spa-ressp-2/Documents/norspis/R')           # "#source" NorSpisFigAndelerGrVar, hvis det er behov for det
#source('NorSpisFigAndelerGrVar.R', encoding = 'UTF-8')      # "#source" NorSpisFigAndelerGrVar, hvis det er behov for det

valgtVar <- 'BehDodUnderBeh'

NorSpisFigAndelerGrVar(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
                       grVar=grVar, outfile=outfile, 
                       minald=minald, maxald=maxald) #reshID=reshID, enhetsUtvalg=enhetsUtvalg, 

#2.2------------------ Teste NorSpisFigGjsnGrVar (andeler per enhet)

valgtMaal='Gjsn'   #evt. endre til 'Med' hvis vil ha medianen. 

#source funksjonen hvis den (ved en feil) ikke kommer inn når man bygger pakken
#getwd()
#setwd('C:/Users/spa-ressp-2/Documents/norspis/R')
#source('NorSpisFigGjsnGrVar.R', encoding = 'UTF-8')

valgtVar <- 'SCL90TGSI'

NorSpisFigGjsnGrVar(RegData=NorSpisData, valgtVar=valgtVar, grVar=grVar, valgtMaal=valgtMaal, datoFra=datoFra, datoTil=datoTil, 
                    minald=minald, maxald=maxald, erMann=erMann, outfile=outfile)

#II--------------------teste flere/alle variabler


#1--------------------Teste FLERE for NorSpisFigAndeler
variableNY <- c("Alder",
              "B01Sivilstatus",
              "B02EgneBarn", 
              "B03Bosituasjon",
              "B04PabegyntUtd", 
              "B05FullfortUtd", 
              "B06Hovedaktivitet",
              "B07Hovedinntekt",
              "B08StartAldrProbl",
              "B11FamilieSF",
              "B12TidlBehSF",
              "B12cAldrForsteBeh",
              "B17FysMishandl",
              "B18PsykMishandl",
              "B19Overgrep",
              "B20Mobbing",
              "B21SelvskadTidl",
              "B22SelvskadSisteAr",
              "B23SelvmordFTidl",
              "B24SelvmordFSisteAr",
              "B25Avhengighet",
              "BehUtfallsvurdSamlet",
              "BehVidereBeh",
              "HCA01Atferd",
              "HCA02Aktivitetsniva",
              "HCA03Selvskade",
              "HCA04Rusmisbruk",
              "HCA05SkoleSprak",
              "HCA06FysiskProblem",
              "HCA07Hallusinasjoner",
              "HCA08SomatiskSymp",
              "HCA09EmosjonelleSymp",
              "HCA10JevnaldrProbl", 
              "HCA11Egenomsorg",
              "HCA12FamilieProbl",
              "HCASkoleframmote",
              "HCA14ProblKunnskap",
              "HCA15Mangelinfo",
              "H01Atferd",
              "H02Selvskade",
              "H03Rusmisbruk",
              "H04KognitiveProbl",
              "H05FysiskeProbl",
              "H06Hallusinasjoner",
              "H07Stemningsleie",
              "H08AndreProbl",
              "H09ForhAndre",
              "H10ADLProbl",
              "H11BoligProbl",
              "H12YrkeProbl",
              "MedAntidepressiva",
              "MedBenzodiazepiner",
              "MedBMISlutt",
              "MedBMIStart",
              "MedIsoBMIBGSSlutt",
              "MedIsoBMIBGSStart", 
              "MedIsoBMICDCSlutt",
              "MedIsoBMICDCStart",
              "MedNevroleptika",
              "NegHend",
              "Norsktalende",
              "PT01OnsketInvolv",
              "PT02BleInvolv",
              "PT03Utfallsvurd",
              "PT04KontaktBrukerorg",
              "PT05OrientertBrukerorg",
              "RegHenvInstans",
              "TidSykBehandling",
              "VentetidKat")                            #sortert alfabetisk

#Hvis legger til nye variable, sende nye til Lena: 
#variableNYE <- setdiff(variableNY,variable) #Send til Lena når lagt til nye valgtVar


for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '_ford.png')
      setwd('C:/Users/spa-ressp-2/Documents/norspis/testfigurer')
      NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
                        reshID=reshID, outfile=outfile, minald=minald, maxald=maxald)
}

#2.1--------------------Teste FLERE for NorSpisFigAndelerGrVar andeler per enhet (evt. annen grupperingsvariabel) --------------------------
for (valgtVar in variable) {
      outfile <- paste(valgtVar, '.png', sep='')
      NorSpisFigAndelerGrVar(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
      grVar=grVar, outfile=outfile, 
      minald=minald, maxald=maxald ) #reshID=reshID, enhetsUtvalg=enhetsUtvalg, 
}

for (valgtVar in variable) {
      outfile <- paste(valgtVar, '.png', sep='')
      FigAndelerGrVar(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
                      datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                      reshID=reshID, libkat=libkat, outfile=outfile)
}

#2.2--------------------Teste FLERE for NorSpisFigGjsnGrVar (gjennomsnitt per enhet)

variable <- c("AlderGjsn",
              "B08StartAldrProbl",
              "B12cAldrForsteBeh", 
              "EDEQ60GlobalScore",
              "EDEQ60Kroppsform",
              "EDEQ60Restriksjon",
              "EDEQ60Spising",
              "EDEQ60Vekt",
              "RAND36EndringHelse",
              "RAND36FysFunk", 
              "RAND36GenHelse", 
              "RAND36MentalHelse", 
              "RAND36RollebegEmo", 
              "RAND36RollebegFys", 
              "RAND36Smerte", 
              "RAND36SosialFunk", 
              "RAND36Tretthet", 
              "SCL90TAngst",
              "SCL90TDepresjon",
              "SCL90TFiendlighet",
              "SCL90TFobi",
              "SCL90TGSI",
              "SCL90TParanoia",
              "SCL90TPsykotisk",
              "SCL90TSensitivitet",
              "SCL90TSomatisering",
              "SCL90TTvang")                       #  sortert alfabetisk


for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '_ford.png')
      setwd('C:/Users/spa-ressp-2/Documents/norspis/testfigurer')
      
      NorSpisFigGjsnGrVar(RegData=NorSpisData, valgtVar=valgtVar, grVar=grVar, valgtMaal=valgtMaal, datoFra=datoFra, datoTil=datoTil, 
                          minald=minald, maxald=maxald, erMann=erMann, outfile=outfile)      
}