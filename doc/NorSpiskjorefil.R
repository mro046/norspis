
library(rapbase)


#--------------------------------------------------------
#------------------------------ Andeler flere var --------------------------
rm(list=ls())
library(norspis)
NorSpisEnkeltledd <- read.table('C:/Users/spa-ressp-2/Documents/norspis/data/EnkeltLeddNum2017-02-01.csv', sep=';', header=T, encoding = 'UTF-8') #,
NorSpisForlop <- read.table('C:/Users/spa-ressp-2/Documents/norspis/data/ForlopsOversikt2017-02-01.csv', sep=';', header=T, encoding = 'UTF-8')
NorSpisData <- merge(NorSpisForlop, suffixes = c('','X'), by = c("ForlopsID" ), NorSpisEnkeltledd, all = FALSE)  #by.x = "ForlopsID", by.y = "ForlopsID",#"SykehusNavn", "AvdRESH"
NorSpisForlop <- read.table('C:/Users/spa-ressp-2/Documents/norspis/data/ForlopsOversikt2017-02-01.csv', sep=';', header=T, encoding = 'UTF-8')

setwd('C:/Users/spa-ressp-2/Documents/norspis/')
RegData <- NorSpisData

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
valgtVar <- 'B01Sivilstatus'	#Må velge... AlderPT03Utfallsvurd,BehUtfallsvurdSamlet, MedBenzodiazepiner,
#MedAntidepressiva,MedNevroleptika, PT01OnsketInvolv,PT02BleInvolv, PT04KontaktBrukerorg, PT05OrientertBrukerorg, 
#Alder,B08StartAldrProbl, B12dAldrForsteBeh, B04PabegyntUtd, Norsktalende, B05FullfortUtd, MedBMI,
#B06Hovedaktivitet, B07Hovedinntekt, B12TidlBehSF, B17FysMishandl, B18PsykMishandl, B19Overgrep, B20Mobbing, 

outfile <-'' #paste(valgtVar, '_ford.png', sep='')#Navn angis av Jasper

NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, minald=minald, maxald=maxald)

#Teste variabler
variable <- c('PT03Utfallsvurd','BehUtfallsvurdSamlet', 'MedBenzodiazepiner', 
              'MedAntidepressiva','MedNevroleptika', 'PT01OnsketInvolv','PT02BleInvolv', 'PT04KontaktBrukerorg', 
              'PT05OrientertBrukerorg', 'Alder','B08StartAldrProbl', 'B12cAldrForsteBeh', 'B04PabegyntUtd', 
              'Norsktalende', 'B05FullfortUtd','MedBMI', 'B06Hovedaktivitet', 'B07Hovedinntekt', 'B12TidlBehSF', 
              'B17FysMishandl', 'B18PsykMishandl', 'B19Overgrep', 'B20Mobbing', 'B01Sivilstatus')

for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '_ford.png')
      NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
                        reshID=reshID, outfile=outfile, minald=minald, maxald=maxald)
}




















#------------------------------ Andeler per enhet (evt. annen grupperingsvariabel) --------------------------
source('NorSpisFigAndelerGrVar.R', encoding = 'UTF-8')
NorSpisFigAndelerGrVar(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
                       reshID=reshID, enhetsUtvalg=enhetsUtvalg, grVar=grVar, outfile=outfile, 
                       minald=minald, maxald=maxald)




#------------------------------ Andel, per enhet --------------------------
#-----------------------------------------------------------------------------------
rm(list=ls())
NorSpisEnkeltledd <- read.table('C:/Users/spa-ressp-2/Documents/norspis/data/EnkeltLedd2017-02-01.csv', sep=';', header=T, encoding = 'UTF-8') #,
NorSpisForlop <- read.table('C:/Users/spa-ressp-2/Documents/norspis/data/ForlopsOversikt2016-08-16.csv', sep=';', header=T, encoding = 'UTF-8')
NorSpisData <- merge(NorSpisForlop, suffixes = c('','X'), by = c("ForlopsID" ), NorSpisEnkeltledd, all = FALSE)  #by.x = "ForlopsID", by.y = "ForlopsID",
#"SykehusNavn", "AvdRESH"
RegData <- NorSpisData
#LENA setwd("C:/ResultattjenesteGIT/Nakke/")

# Inndata til funksjon:
reshID <- 13 #De tre med flest reg:
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2015-12-31'
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
tittel=1
enhetsUtvalg <- 1	#1-Eget sykehus mot resten (standard), 0-Hele landet, 2-Eget sykehus

valgtVar <- ''	#Må velge... 

outfile <- '' #paste(valgtVar, '_ShusSyn.png', sep='')	#Navn angis av Jasper


NorSpisFigAndelerGrVar(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar,
                datoTil=datoTil, minald=minald, maxald=maxald, #erMann=erMann,
                reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile) #Mads:Påse at riktige innparametre...(sammenlign med de variablene funksjonen bygger på (se filaNorSpisFigAndelerGrVar.R)).  

##Teste variables
#variable <- c('Alder','Utdanning')

#for (valgtVar in variable) {
#      outfile <- paste(valgtVar, '.png', sep='')
#      FigAndelerGrVar(RegData=NakkeData, datoFra=datoFra, valgtVar=valgtVar,
#                      datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
#                      reshID=reshID, libkat=libkat, outfile=outfile)
#}