

#--------------------------------------------------------
#------------------------------ Andeler flere var --------------------------
rm(list=ls())
library(norspis)
NorSpisEnkeltledd <- read.table('C:/ResultattjenesteGIT/norspis/data/EnkeltLeddNum2016-08-16.csv', sep=';', header=T, encoding = 'UTF-8') #,
NorSpisForlop <- read.table('C:/ResultattjenesteGIT/norspis/data/ForlopsOversikt2016-08-16.csv', sep=';', header=T, encoding = 'UTF-8')
NorSpisData <- merge(NorSpisForlop, suffixes = c('','X'), by = c("ForlopsID" ), NorSpisEnkeltledd, all = FALSE)  #by.x = "ForlopsID", by.y = "ForlopsID", 
#"SykehusNavn", "AvdRESH"

setwd('C:/ResultattjenesteGIT/norspis/')
RegData <- NorSpisData

# Inndata til funksjon:
reshID <- 12  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
erMann <- ''
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
preprosess <- 1
hentData <- 0
enhetsUtvalg <- 1 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'B05FullfortUtd'	#Må velge... Alder, , B04PabegyntUtd, Norsktalende 

outfile <- paste(valgtVar, '_ford.png', sep='')	#Navn angis av Jasper
setwd("C:/ResultattjenesteGIT/NorSpis/")

 NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, minald=minald, maxald=maxald )


#Teste variable
variable <- c('Alder', 'Education')


for (valgtVar in variable) {
	outfile <- paste(valgtVar, '_ford.png', sep='')
	NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, outfile=outfile, minald=minald, maxald=maxald)
}

