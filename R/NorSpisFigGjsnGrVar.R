#'Søylediagram som viser sentralmål (gj.sn./median) for hvert sykehus
#'
#' Funksjon som genererer en figur med gjennomsnitt/median
#' for hvert sykehus og kan ta inn ulike numeriske variable.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' @inheritParams NorSpisFigAndeler
#' @param valgtMaal Sentralmål 'Med' gir median, alt annet gir gjennomsnitt
#' @param valgtVar Variabelen det skal vises resultat for.
#'             B08StartAldrProbl: alder (år)
#'
#' @return Figur med...
#'
#' @export

NorSpisFigGjsnGrVar <- function(RegData, valgtVar, valgtMaal='Gjsn', datoFra='2012-04-01', datoTil='2050-12-31',
                         minald=0, maxald=130, erMann='', outfile='', hentData=0, preprosess=1, grVar='')
      
                        # reshID ,        
                        #spmLena: skal "lagFig=1" inn (i NorSpisFigAndelerGrVar er den med, men ikke i NakkeFikAndelerGrVar)?
                        #spmLena: tok inn "grVar=''", men var usikker på om den skulle være med...den er med i NorSpisFigAndelerGrVar, men ikke i NakkeFikAndelerGrVar) 


{

#------ Hente data
      if (hentData == 1) {
            RegData <- NorSpisELAlleScorData(datoFra, datoTil) #RegData <- NakkeRegDataSQL()	#RegData <- NakkeLoadRegDataMinimal()
      }
      
#------ Preprosessere data
      if (preprosess){                                #SpmLena: ikke preprosess==1 her (som over)?
            RegData <- NorSpisPreprosess(RegData=RegData)  #RegData <- NakkePreprosess(RegData=RegData)
      }
      
#------- Tilrettelegge variable
NorSpisVarSpes <- NorSpisVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)
 #-------og hente inn RegData og parametre fra tilretteleggingen
RegData <- NorSpisVarSpes$RegData
deltittel <- NorSpisVarSpes$deltittel
xaksetxt <- NorSpisVarSpes$tittel

#------- Gjøre utvalg
      NorSpisUtvalg <- NorSpisUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar, minald=minald, maxald=maxald, 
                                     erMann=erMann, enhetsUtvalg=enhetsUtvalg, reshID=reshID)  #SpmLena: Trenger vi alle disse innparamentrene (kokt fra NorSpisFigAndlerGrVar. 
                                                                                                #NakkeUtvalg (nedenfor har færre parametre))
                                                                                                #NakkeUtvalg <- NakkeLibUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann)	#, tidlOp=tidlOp
      RegData <- NorSpisUtvalg$RegData
      utvalgTxt <- NorSpisUtvalg$utvalgTxt

      
      if (dim(RegData)[1] >= 0) {
            RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
            RegData[ ,grVar] <- as.factor(RegData[ ,grVar])	#RegData$AvdID <- as.factor(RegData$AvdID)
            Ngr <- table(RegData[ ,grVar])
      } else {
            Ngr <- 0}
      
      
#-------Gjøre beregninger (og først sette noen parametre)

      Ngrense <- 10		#Minste antall registreringer for at ei gruppe skal bli vist
      N <- dim(RegData)[1]
      
      if(N > 0) {
            Ngr <- table(RegData[ ,grVar])
      } else {
            Ngr <- 0}
      
      AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))  #MULIGENS OVERFLØDIG - KOMMER LENGER NED...
      
      
      
#FØLGENDE KOMMER FRA NakkeFigGjsnGrVar.R:
      #Ngrtxt <- paste(', N=', as.character(Ngr), sep='') #paste('N=', as.character(Ngr), sep='')
      Ngrtxt <- paste0('N=', as.character(Ngr)) #paste('N=', as.character(Ngr), sep='')
      indGrUt <- as.numeric(which(Ngr < Ngrense))
      if (length(indGrUt)==0) { indGrUt <- 0}
      Ngrtxt[indGrUt] <- paste0(' (<', Ngrense,')')	#paste('N<', Ngrense,sep='')
      
      
      if (valgtMaal=='Med') {
            t1 <- 'Median'
            tleg <- t1} else {
            t1 <- 'Gjennomsnittlig'
            tleg <- 'Gjennomsnitt'}
      
      tittel <- paste(t1, deltittel, sep=' ')
      
      
      
#-----------Figur---------------------------------------
      
if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
      FigTypUt <- figtype(outfile)
      farger <- FigTypUt$farger
      plot.new()
      if (dim(RegData)[1]>0) {
            tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
      } else {tekst <- 'Ingen registrerte data for dette utvalget'}
      title(main=tittel, cex=0.95)	#line=-8,
      text(0.5, 0.6, tekst, cex=1.2)
      #text(0.5, 0.3, , cex=1.2)
      legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
      if ( outfile != '') {dev.off()}
} else {
            
      #------------------------------------------beregninger---------
      dummy0 <- -0.001                                                        #SpmLena: ?
      #Kommer ut ferdig sortert!
      if (valgtMaal=='Med') {
            MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
            MedIQR$stats[ ,indGrUt] <- dummy0
            MedIQR$conf[ ,indGrUt] <- dummy0
            sortInd <- order( MedIQR$stats[3,], decreasing=TRUE)
            Midt <- as.numeric(MedIQR$stats[3, sortInd])
            KIned <- MedIQR$conf[1, sortInd]
            KIopp <- MedIQR$conf[2, sortInd]
            MedIQRHele <-  boxplot.stats(RegData$Variabel, do.conf = TRUE)
            MidtHele <- as.numeric(MedIQRHele$stats[3])	#median(RegData$Variabel)
            KIHele <- MedIQRHele$conf
            #The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).)
            #They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared,
            #and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give
            #roughly a 95% confidence interval for the difference in two medians.
                  
      } else {	#Gjennomsnitt blir standard.
            Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
            SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
            Gjsn[indGrUt] <- dummy0
            SE[indGrUt] <- 0
            sortInd <- order(Gjsn, decreasing=TRUE)
            Midt <- as.numeric(Gjsn[sortInd])
            KIned <- Gjsn[sortInd] - 2*SE[sortInd]
            KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
            MidtHele <- round(mean(RegData$Variabel),1)
            KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)
      }
            
      #GrNavnSort <- paste(names(Ngr)[sortInd], Ngrtxt[sortInd], sep='')
      GrNavnSort <- names(Ngr)[sortInd] #, Ngrtxt[sortInd])
      AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
      soyletxt <- c(sprintf('%.1f',Midt[1:AntGr]), rep('',length(Ngr)-AntGr))	#	#round(Midt[1:AntGr],1)       #spmLena:'%.1f'?
      xmax <-  min(1.1*max(c(Midt, KIned, KIopp)), 1.4*max(Midt))
      cexGrNavn <- 1
      cexSoyletxt <- 1
            
#--------------------------FIGUR---------------------------------------------------
      FigTypUt <- figtype(outfile, height=3*800, fargepalett=NorSpisUtvalg$fargepalett) #, res=96
      farger <- FigTypUt$farger
      #Tilpasse marger for å kunne skrive utvalgsteksten
      NutvTxt <- length(utvalgTxt)
      vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexGrNavn)*0.7)
      #NB: strwidth oppfører seg ulikt avh. av device...
      par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med    #spmLena?
            
#første del av "tegningen av figuren" begynner herfra...
      pos <- barplot(Midt, horiz=T, border=NA, col=farger[3],
                  xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='', las=1, cex.names=cexGrNavn)
      indGrUtPlot <- AntGr+(1:length(indGrUt))
      posKI <- pos[1:AntGr]
      ybunn <- 0
      ytopp <- max(posKI)*1.2	 #endret fra "1.03"  #min(posKI) 
      #lager 95%KI for hele landet
      polygon( c(rep(KIHele[1],2), rep(KIHele[2],2)), c(ybunn, ytopp, ytopp, ybunn),col=farger[4], border=farger[4])   #spmLena: dette plottet virker overflødig -tar det derfor ut (med #) Hva er tanken bak det?
      lines(x=rep(MidtHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)      #gjøres også noen linjer nedenfor, kan derfor antagelig fjerne herfra... 
      legend('top', fill=c('white', farger[4]),  border='white', lwd=2,
                 col=c(farger[2], farger[4]), seg.len=0.6, merge=TRUE, bty='n',
                 c(paste(tleg, ', alle: ', sprintf('%.1f', MidtHele), ', N=', N, sep=''),
                 paste('95% konf.int., alle (',
                 sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')', sep='')))
            
            
      barplot(Midt, horiz=T, border=NA, col=farger[3], xlim=c(0, xmax), add=TRUE,
                    font.main=1, xlab = xaksetxt, las=1) 	#xlim=c(0,ymax), #, cex.names=0.5         #dette plottet legges til det opprinnelig (add=TRUE)
      title(tittel, font.main=1)
      title('med 95% konfidensintervall', line=0.5, font.main=1, cex.main=0.95)
      mtext(at=pos+0.1, GrNavnSort, side=2, las=1, cex=cexGrNavn, adj=1, line=0.25)	#Sykehusnavn
      mtext(at=pos-0.1, Ngrtxt[sortInd], side=2, las=1, cex=cexGrNavn, adj=1, line=0.25)	#Sykehusnavn
            
      text(x=1.1*max(strwidth(soyletxt, units='user', cex=cexSoyletxt)), y=pos,	#y=pos+0.1,
                    soyletxt, las=1, cex=cexSoyletxt, adj=1, col=farger[4])	#Tekst på søylene (verdi,e.g.gjennomsnittsalderen for sykehuset/søylen)
      #OK	text(x=xmax/20, y=pos+0.1, soyletxt, las=1, cex=0.75, adj=1, col=farger[1])	#Tekst på søylene (verdi)
            
      #Tekst som angir hvilket utvalg som er gjort ("Registreringsperiode: XXXX-XX-XX til YYYY-YY-YY")
      mtext(utvalgTxt, side=3, las=1, cex=cexGrNavn*0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
            
      options(warn = -1)	#Unngå melding om KI med lengde 0. Fungerer av en eller annen grunn ikke i pdf.
      #øvre KI
      arrows(x0=Midt[-indGrUtPlot]*0.999, y0=posKI, x1=KIopp[-indGrUtPlot], y1=posKI,
            length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
      #nedre KI
      arrows(x0=Midt[-indGrUtPlot]*1.001, y0=posKI, x1=KIned[-indGrUtPlot], y1=posKI,
            length=0.5/max(pos), code=2, angle=90, lwd=1.5, col=farger[1])
      par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}
#----------------------------------------------------------------------------------
      }
}
