

#--------------------------------------------------------
#------------------------------ Andeler flere var --------------------------
rm(list=ls())
library(norspis)
NorSpisEnkeltledd <- read.table('C:/ResultattjenesteGIT/norspis/data/EnkeltLeddNum2016-08-16.csv', sep=';', header=T, encoding = 'UTF-8') #,
NorSpisForlop <- read.table('C:/ResultattjenesteGIT/norspis/data/ForlopsOversikt2016-08-16.csv', sep=';', header=T, encoding = 'UTF-8')
NorSpisData <- merge(NorSpisForlop, by = "ForlopsID", NorSpisEnkeltledd, all = FALSE)  #by.x = "ForlopsID", by.y = "ForlopsID", 


reshID <- ..  	#Må sendes med til funksjon
setwd('C:/ResultattjenesteGIT/norspis/')
RegData <- NorSpisData

# Inndata til funksjon:
reshID <- 110734 # 110734 (Tønsberg)  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
erMann <- ''
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
preprosess <- TRUE
hentData <- 0
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
valgtVar <- 'BMI'	#Må velge... Alder, Education, FollowupSeriousness, HypCompleteness, KomplPost, KomplPostUtd, KomplReopUtd,

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

