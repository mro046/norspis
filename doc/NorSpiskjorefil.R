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


#------------------------Definere parametre (som sendes inn til funksjonen)
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




outfile <-'' #paste(valgtVar, '_ford.png', sep='')#Navn angis av Jasper


#--------------------Teste NorSpisFigAndeler

valgtVar <- 'Alder'	#Må velge... Alder, PT03Utfallsvurd,BehUtfallsvurdSamlet, BehVidereBeh, MedBenzodiazepiner,
#MedAntidepressiva,MedNevroleptika, PT01OnsketInvolv,PT02BleInvolv, PT04KontaktBrukerorg, PT05OrientertBrukerorg, 
#Alder,B08StartAldrProbl, B12dAldrForsteBeh, B04PabegyntUtd, NegHend, Norsktalende, B05FullfortUtd, MedBMI,
#B06Hovedaktivitet, B07Hovedinntekt, B12TidlBehSF, B17FysMishandl, B18PsykMishandl, B19Overgrep, B20Mobbing, 


NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, minald=minald, maxald=maxald)

      #--------------------teste flere/alle variabler
variable <- c( "Alder", "B04PabegyntUtd", "B05FullfortUtd", "B06Hovedaktivitet",
               "B07Hovedinntekt", "B08StartAldrProbl", "B12cAldrForsteBeh", "B12TidlBehSF", 
               "B17FysMishandl", "B18PsykMishandl", "B19Overgrep", "B20Mobbing",   
               "BehUtfallsvurdSamlet", "MedAntidepressiva", "MedBenzodiazepiner", "MedBMI",       
               "MedNevroleptika", "NegHend", "Norsktalende", "PT01OnsketInvolv", "PT02BleInvolv",
               "PT03Utfallsvurd", "PT04KontaktBrukerorg", "PT05OrientertBrukerorg")                #sortert alfabetisk


for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '_ford.png')
      setwd('C:/Users/spa-ressp-2/Documents/norspis/testfigurer')
      NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
                        reshID=reshID, outfile=outfile, minald=minald, maxald=maxald)
}

#-------------------- Teste NorSpisFigAndelerGrVar (andeler per enhet (evt. annen grupperingsvariabel)) --------------------------
valgtVar <- 'DiagVDiabetes'             #valg: alder_u18, BehDodUnderBeh, DiagVDiabetes 
grVar <- 'SykehusNavn'             #variablen man ønsker å gruppere på

#outfile <- paste(valgtVar, '_ford.png', sep='')#Navn angis av Jasper

setwd('C:/Users/spa-ressp-2/Documents/norspis/R')
source('NorSpisFigAndelerGrVar.R', encoding = 'UTF-8')


NorSpisFigAndelerGrVar(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
                       grVar=grVar, outfile=outfile, 
                       minald=minald, maxald=maxald ) #reshID=reshID, enhetsUtvalg=enhetsUtvalg, 



#------------------Teste FigGjsnGrVar(gjennomsnitt per enhet)

valgtVar <- 'AldersGjsn' #AldersGjsn, B08StartAldrProbl, B12cAldrForsteBeh, SCL90TDepresjon, SCL90TGSI, SCL90TSensitivitet, SCL90TSomatisering, SCL90TTvang,
grVar <- 'SykehusNavn'
valgtMaal='Gjsn'   #evt. endre til 'Med' hvis vil ha medianen. 

#source funksjonen hvis den (ved en feil) ikke kommer inn når man bygger pakken
#getwd()
setwd('C:/Users/spa-ressp-2/Documents/norspis/R')

source('NorSpisFigGjsnGrVar.R', encoding = 'UTF-8')
NorSpisFigGjsnGrVar(RegData=NorSpisData, valgtVar=valgtVar, grVar=grVar, valgtMaal=valgtMaal, datoFra=datoFra, datoTil=datoTil, 
                    minald=minald, maxald=maxald, erMann=erMann, outfile=outfile)


      #--------------------teste flere
variable <- c("EDEQ60GlobalScore",
            "EDEQ60Restriksjon",
            "EDEQ60Kroppsform",
            "EDEQ60Spising",
            "EDEQ60Vekt",
            "EDEQ60GlobalScore",
            "EDEQ60Restriksjon",
            "EDEQ60Kroppsform",
            "EDEQ60Spising",
            "EDEQ60Vekt",
            "RAND36FysFunk", 
            "RAND36RollebegFys", 
            "RAND36RollebegEmo", 
            "RAND36Tretthet", 
            "RAND36MentalHelse", 
            "RAND36SosialFunk", 
            "RAND36Smerte", 
            "RAND36GenHelse", 
            "RAND36EndringHelse",
            "SCL90TGSI",
            "SCL90TSomatisering",
            "SCL90TTvang",
            "SCL90TSensitivitet",
            "SCL90TDepresjon",
            "SCL90TAngst",
            "SCL90TFiendlighet",
            "SCL90TFobi",
            "SCL90TParanoia",
            "SCL90TPsykotisk")                       #ikke sortert alfabetisk enda

for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '_ford.png')
      setwd('C:/Users/spa-ressp-2/Documents/norspis/testfigurer')
      
      NorSpisFigGjsnGrVar(RegData=NorSpisData, valgtVar=valgtVar, grVar=grVar, valgtMaal=valgtMaal, datoFra=datoFra, datoTil=datoTil, 
                          minald=minald, maxald=maxald, erMann=erMann, outfile=outfile)      
}




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