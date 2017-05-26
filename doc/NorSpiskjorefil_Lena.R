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
library(norspis)
# Inndata til funksjon:
reshID <- 'TESTNO'  	#Må sendes med til funksjon
minald <- 0	#alder, fra og med
maxald <- 130	#alder, til og med
erMann <- ''
datoFra <- '2013-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2017-12-31'
preprosess <- 1
hentData <- 0
enhetsUtvalg <- 0 #		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
grVar <- 'EnhNavn'

#------------------------------ Andeler flere var --------------------------

valgtVar <- 'B03Bosituasjon'	#Må velge... NegHend, PT03Utfallsvurd,BehUtfallsvurdSamlet, MedBenzodiazepiner, 
            #MedAntidepressiva,MedNevroleptika, PT01OnsketInvolv,PT02BleInvolv, PT04KontaktBrukerorg, 
            #PT05OrientertBrukerorg, Alder,B08StartAldrProbl, B12dAldrForsteBeh, B04PabegyntUtd, 
            #Norsktalende, B05FullfortUtd, MedBMI, B06Hovedaktivitet, B07Hovedinntekt, B12TidlBehSF, 
            #B17FysMishandl, B18PsykMishandl, B19Overgrep, B20Mobbing, 

outfile <- '' #paste(valgtVar, '_ford.png', sep='')#Navn angis av Jasper

NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
	reshID=reshID, enhetsUtvalg=enhetsUtvalg, outfile=outfile, minald=minald, maxald=maxald )

 
variable <- c("Alder", "B02EgneBarn", "B03Bosituasjon", "B04PabegyntUtd", "B05FullfortUtd", "B06Hovedaktivitet",
               "B07Hovedinntekt", "B08StartAldrProbl",'B11FamilieSF', "B12TidlBehSF",'B12cAldrForsteBeh',
               "B17FysMishandl", "B18PsykMishandl", "B19Overgrep", "B20Mobbing", "B21SelvskadTidl",
               "B22SelvskadSisteAr", "B23SelvmordFTidl", "B24SelvmordFSisteAr", "B25Avhengighet",    
               "BehUtfallsvurdSamlet", "MedAntidepressiva", "MedBenzodiazepiner","MedBMISlutt","MedBMIStart",
               "MedNevroleptika", "B25Avhengighet","NegHend", "Norsktalende", "MedIsoBMIBGSSlutt", "MedIsoBMIBGSStart", 
               "MedIsoBMICDCSlutt", "MedIsoBMICDCStart","PT01OnsketInvolv","PT02BleInvolv","PT03Utfallsvurd", 
               "PT04KontaktBrukerorg", "PT05OrientertBrukerorg", "RegHenvInstans", "TidSykBehandling","VentetidKat" )
variableNY <- c("Alder", 
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
              "VentetidKat")                  

variableNYE <- c("TidSykBehandling",
                 "VentetidKat") #setdiff(variableNY,variable)
      
 for (valgtVar in variableNYE) {
	outfile <- paste0(valgtVar, '_ford.png')
	NorSpisFigAndeler(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
		reshID=reshID, outfile=outfile, minald=minald, maxald=maxald)
 }

#------------------------------ Andeler per enhet (evt. annen grupperingsvariabel) --------------------------

valgtVar <- 'alder_u18'
outfile <- paste0(valgtVar, '_AndEnh.png')#Navn angis av Jasper
NorSpisFigAndelerGrVar(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
                  grVar=grVar, outfile=outfile, 
                  minald=minald, maxald=maxald ) #reshID=reshID, enhetsUtvalg=enhetsUtvalg, 
variable <- c('alder_u18','BehDodUnderBeh', 'DiagVDiabetes', 'VentetidOver2Uker')
for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '_andGrVar.png')
      NorSpisFigAndelerGrVar(RegData=NorSpisData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil, #erMann=erMann,
                             grVar=grVar, outfile=outfile, minald=minald, maxald=maxald)
}

#------------------------------ Gjennomsnitt/median per enhet (evt. annen grupperingsvariabel) --------------------------

grVar <- 'EnhNavn'
valgtMaal='Gjsn'   #evt. endre til 'Med' hvis vil ha medianen. 
valgtVar <- 'EDEQ60GlobalScore' #Alder, B08StartAldrProbl, B12cAldrForsteBeh, SCL90TDepresjon, SCL90TGSI, 
      #SCL90TSensitivitet, SCL90TSomatisering, SCL90TTvang,
outfile <- '' #paste0(valgtVar, 'GjMed.png')

#source('NorSpisFigGjsnGrVar.R', encoding = 'UTF-8')
NorSpisFigGjsnGrVar(RegData=NorSpisData, valgtVar=valgtVar, grVar=grVar, valgtMaal=valgtMaal, datoFra=datoFra, datoTil=datoTil, 
                    minald=minald, maxald=maxald, erMann=erMann, outfile=outfile)

variable <- c("AlderGjsn",
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
              "SCL90TTvang")                       #ikke sortert alfabetisk enda
#Sortert

for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '_gjsn.png')
      NorSpisFigGjsnGrVar(RegData=NorSpisData, valgtVar=valgtVar, grVar=grVar, valgtMaal=valgtMaal, datoFra=datoFra, datoTil=datoTil, 
                          minald=minald, maxald=maxald, erMann=erMann, outfile=outfile)      
}

