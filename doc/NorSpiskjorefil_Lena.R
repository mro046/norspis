#library(rapbase) #Fjerne når rapbase/figtype permanent del av NorSpis?



#-------------------------Hente data-------------------------------
rm(list=ls())
library(norspis)
NorSpisAlleScorer <- read.table('C:/Registre/NorSpis/AlleScorer2017-02-01.csv', sep=';', header=T, encoding = 'UTF-8') #,
NorSpisEnkeltledd <- read.table('C:/Registre/NorSpis/EnkeltLeddNum2017-02-01.csv', sep=';', header=T, encoding = 'UTF-8') #,
NorSpisForlop <- read.table('C:/Registre/NorSpis/ForlopsOversikt2017-02-01.csv', sep=';', header=T, encoding = 'UTF-8')
#NorSpisData <- merge(NorSpisForlop, suffixes = c('','X'), by = c("ForlopsID" ), NorSpisEnkeltledd, all = FALSE)  #by.x = "ForlopsID", by.y = "ForlopsID",
ForlAlleSc <- merge(NorSpisForlop, NorSpisAlleScorer, suffixes = c('','X'),
                by = "ForlopsID", all = FALSE)
NorSpisData <- merge(ForlAlleSc, NorSpisEnkeltledd, suffixes = c('','X'),
                     by = "ForlopsID", all = FALSE)

RegData <- NorSpisData

#ForlopTab <- NorSpisForlop[ ,c('AvdRESH', 'erMann','PasientAlder','ForlopsID','HovedDato','SykehusNavn')]
#EnkeltleddTab <- NorSpisEnkeltledd[,c('ForlopsID','DiagVSF')]
#ELForl <- merge(ForlopTab, EnkeltleddTab,
#                     by = "ForlopsID", all = FALSE)
#AlleScorerELForl <- merge(ELForl, NorSpisAlleScorer,
#                by = "ForlopsID", all = FALSE)


#------------------------Definere parametre

setwd('C:/ResultattjenesteGIT/norspis/')

# Inndata til funksjon:
reshID <- 'TESTNO'  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
erMann <- ''
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
preprosess <- 1
hentData <- 0
enhetsUtvalg <- 1 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
grVar <- 'ShNavn'

#------------------------------ Andeler flere var --------------------------

valgtVar <- 'B12TidlBehSF'	#Må velge... NegHend, PT03Utfallsvurd,BehUtfallsvurdSamlet, MedBenzodiazepiner, 
            #MedAntidepressiva,MedNevroleptika, PT01OnsketInvolv,PT02BleInvolv, PT04KontaktBrukerorg, 
            #PT05OrientertBrukerorg, Alder,B08StartAldrProbl, B12dAldrForsteBeh, B04PabegyntUtd, 
            #Norsktalende, B05FullfortUtd, MedBMI, B06Hovedaktivitet, B07Hovedinntekt, B12TidlBehSF, 
            #B17FysMishandl, B18PsykMishandl, B19Overgrep, B20Mobbing, 

outfile <-'' #paste(valgtVar, '_ford.png', sep='')#Navn angis av Jasper

NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, minald=minald, maxald=maxald )

 
variable <- c('PT03Utfallsvurd','BehUtfallsvurdSamlet', 'MedBenzodiazepiner', 
              'MedAntidepressiva','MedNevroleptika', 'PT01OnsketInvolv','PT02BleInvolv', 'PT04KontaktBrukerorg', 
              'PT05OrientertBrukerorg', 'Alder','B08StartAldrProbl', 'B12cAldrForsteBeh', 'B04PabegyntUtd', 
              'Norsktalende', 'B05FullfortUtd','MedBMI', 'B06Hovedaktivitet', 'B07Hovedinntekt', 'B12TidlBehSF', 
              'B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing')

 for (valgtVar in variable) {
	outfile <- paste0(valgtVar, '_ford.png')
	NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, outfile=outfile, minald=minald, maxald=maxald)
 }

#------------------------------ Andeler per enhet (evt. annen grupperingsvariabel) --------------------------

valgtVar <- 'alder_u18'
outfile <- paste(valgtVar, '_ford.png', sep='')#Navn angis av Jasper
NorSpisFigAndelerGrVar(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
                  grVar=grVar, outfile=outfile, 
                  minald=minald, maxald=maxald ) #reshID=reshID, enhetsUtvalg=enhetsUtvalg, 

#(Mads-fjerne alfanum når NorSpis-pakke OK) #Teste variables
#(Mads-fjerne alfanum når NorSpis-pakke OK) variable <- c('Alder', 'Education')


