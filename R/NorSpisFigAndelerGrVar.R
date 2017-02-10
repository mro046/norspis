#' Søylediagram med AggVerdier for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med AggVerdier av en gitt variabel for ei valgt gruppering, 
#' f.eks. enheter. I øyeblikket benytter funksjonen bare 'ShNavn' som grupperingsvariabel, men 
#' andre valg kan lett inkluderes. 
#'
#' Figurtypen som genereres er avhengig av valgtVar. Ved valgtVar='InnMaate', får man ei oversikt oversikt
#' over andel av innkomsttype ved hver enhet, dvs. ei fordeling av de tre innkomsttypene for hver enhet.
#' For andre "valgtVar" viser figuren andel av den valgte variabelen for hver enhet.
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder_u18: Pasienter under 18 år 
#'     \item alder_over80: Pasienter over 80 år (>=80)
#'     \item dod30d: Pasienter som dør innen 30 dager etter innleggelse
#'     \item dodeIntensiv: Pasienter som dør på intensivavdelinga. 
#'     \item innMaate: Hastegrad inn på intensiv (Elektivt, Akutt medisinsk, Akutt kirurgisk)
#'		Dette valget viser en annen figurtype.
#'     \item respStotte: Pasienter som har fått respiratorstøtte
#'     \item reinn: Andel reinnlagte (kun hvor dette er registrert, dvs. fjerner ukjente)
#'    }
#' Funksjonen benytter funksjonene: NorSpisRegDataSQL, NorSpisPreprosess, NorSpisVarTilrettelegg, NorSpisUtvalg
#' og NIRFigSoyler
#'
#' @inheritParams NorSpisFigAndeler
#' 
#' 
#' @return Søylediagram med AggVerdier av valgt variabel for hvert sykehus
#'
#' @export
NorSpisFigAndelerGrVar <- function(RegData, valgtVar, datoFra=0, datoTil=0, 
                            minald=0, maxald=130, grVar='', erMann='', hentData=0, preprosess=1, 
                            outfile='', lagFig=1) 
      
      #aar=0,InnMaate=99,grType=99, 
                              
{
      #NB: Tomme grVar fjernes så vurder om dette kan være standard...
      if (hentData == 1) {		
            RegData <- NorSpisRegDataSQL(datoFra, datoTil)
      }
      
      # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
      if (preprosess){
            RegData <- NorSpisPreprosess(RegData=RegData)	#, reshID=reshID)
      }
      
      #------- Tilrettelegge variable
#      NorSpisVarSpes <- NorSpisVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)
#      RegData <- NorSpisVarSpes$RegData

      if (valgtVar=='alder_u18') {	#endret fra under18
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel[which(RegData$Alder<18)] <- 1 
            tittel <- 'Pasienter under 18 år'
      }

            if (valgtVar=='diabetes') { #AndelerGrVar
       RegData$Variabel <- RegData$Dod30
       tittel <- 'Opphold der pasienten døde innen 30 dager etter innleggelse'
       sortAvtagende <- FALSE
 }

	  
      #------- Gjøre utvalg
      NorSpisUtvalg <- NorSpisUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar, minald=minald, maxald=maxald, 
                                      erMann=erMann, enhetsUtvalg=enhetsUtvalg, reshID=reshID)
            
      
      RegData <- NorSpisUtvalg$RegData
      utvalgTxt <- NorSpisUtvalg$utvalgTxt

      
      if (dim(RegData)[1] >= 0) {
            RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
            RegData[ ,grVar] <- as.factor(RegData[ ,grVar])	#RegData$AvdID <- as.factor(RegData$AvdID)
            Ngr <- table(RegData[ ,grVar])
      } else {
            Ngr <- 0}

#------------Gjøre beregninger      
      Ngrense <- 10	
      N <- dim(RegData)[1]
      if(N > 0) {Ngr <- table(RegData[ ,grVar])} else {Ngr <- 0}
      AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
      AndelerGr <- as.vector(table(RegData[which(RegData$Variabel==1) , grVar])/Ngr*100)	#round(100*Nvar/Ngr,2)
      
      if (sum(which(Ngr < Ngrense))>0) {indGrUt <- as.numeric(which(Ngr<Ngrense))} else {indGrUt <- 0}
      AndelerGr[indGrUt] <- NA #-0.0001
      sortInd <- order(as.numeric(AndelerGr),na.last = FALSE) #decreasing=NorSpisVarSpes$sortAvtagende
      
      AndelerGrSort <- AndelerGr[sortInd]
      AndelHele <- sum(RegData$Variabel==1)/N*100	
      Ngrtxt <- as.character(Ngr)	#
      Ngrtxt[indGrUt] <- paste0('<', Ngrense) 
      GrNavnSort <- paste0(names(Ngr)[sortInd], ' (',Ngrtxt[sortInd], ')')
      grtxt <- GrNavnSort
      
      andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), ' %') 	
      andeltxtUsort[indGrUt] <- ''
      andeltxt <- andeltxtUsort[sortInd]
      soyletxt <- andeltxt
      
      N = list(Hoved=N, Rest=0)
      Ngr = list(Hoved=Ngr, Rest=0)
      AggVerdier = list(Hoved=AndelerGrSort, Rest=0)
      xAkseTxt <- "Andel pasienter (%)"	#Denne kan avhenge av figurtype
      
      #Se NorSpisFigSoyler for forklaring av innhold i AndelerGrVarData
#      AndelerGrVarData <- list(AggVerdier=AggVerdier, 
#                               AggTot=AndelHele, 
#                               N=N, 
#                               Ngr=Ngr,
#                               grtxt2='', 
#                               soyletxt=andeltxt,
#                               grtxt=GrNavnSort,
#                               #tittel=NorSpisVarSpes$tittel, 
#                               #yAkseTxt=yAkseTxt, 
#                               #retn='H', 
#                               xAkseTxt=xAkseTxt, #NorSpisVarSpes$xAkseTxt,
#                               #KImaal = NorSpisVarSpes$KImaal,
#                               grTypeTxt=NorSpisUtvalg$grTypeTxt,			 
#                               utvalgTxt=NorSpisUtvalg$utvalgTxt, 
#                               fargepalett=NorSpisUtvalg$fargepalett, 
#                               medSml=NorSpisUtvalg$medSml, 
#                               smltxt=NorSpisUtvalg$smltxt)
      
      #Lagre beregnede data
      #if (hentData==1) {
#      save(AndelerGrVarData, file='data/AndelerGrVarData.RData')
      #}
      
      #FigDataParam skal inn som enkeltparametre i funksjonskallet
	  lagFig=0
	  cexgr <- 1-ifelse(AntGr>20, 0.25*AntGr/60, 0)
	  
      if (lagFig == 1) {
            NIRFigSoyler(RegData, AggVerdier=AggVerdier, AggTot=AndelHele, Ngr=Ngr,N=N, cexgr=cexgr, 
                         tittel=NorSpisVarSpes$tittel, 
                         smltxt=NorSpisUtvalg$smltxt, utvalgTxt=NorSpisUtvalg$utvalgTxt, #yAkseTxt=yAkseTxt,
                         grTypeTxt=NorSpisUtvalg$grTypeTxt,  fargepalett=NorSpisUtvalg$fargepalett, grtxt=GrNavnSort, 
                         soyletxt=andeltxt,grVar=grVar, KImaal = NorSpisVarSpes$KImaal, #medKI = medKI,
                         medSml=NorSpisUtvalg$medSml, xAkseTxt=xAkseTxt, outfile=outfile)
      }
      #---------------------------------------FRA FIGANDELER, FigGjsnGrVar og FigAndelGrVar--------------------------
#Hvis for få observasjoner..

if (dim(RegData)[1] < 10 | 
		(grVar=='' & length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg %in% c(1,3))) {
	#-----------Figur---------------------------------------
      FigTypUt <-figtype(outfile)  #FigTypUt <- figtype(outfile)
	farger <- FigTypUt$farger
	plot.new()
	title(tittel)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	tekst <- 'For få registreringer'
	text(0.5, 0.6, tekst, cex=1.2)
	if ( outfile != '') {dev.off()}
	
} else {
	

	#Plottspesifikke parametre:
	#Høyde må avhenge av antall grupper
	hoyde <- ifelse(length(AggVerdier$Hoved)>20, 3*800, 3*600)
	FigTypUt <- figtype(outfile, height=hoyde, fargepalett=NorSpisUtvalg$fargepalett)	
	#Tilpasse marger for å kunne skrive utvalgsteksten
	NutvTxt <- length(utvalgTxt)
	vmarg <- max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75)
	#NB: strwidth oppfører seg ulikt avh. av device...
	par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
	
	
	farger <- FigTypUt$farger
	fargeHoved <- ifelse(grVar %in% c('ShNavn'), farger[4], farger[1])
	fargeRest <- farger[3]
	antGr <- length(grtxt)
	lwdRest <- 3	#tykkelse på linja som repr. landet
	cexleg <- 0.9	#Størrelse på legendtekst
	
	
	#Definerer disse i beregningsfunksjonen?  
      xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.2
      xmax <- min(xmax, 100) 	#100 som maks bare hvis andelsfigur..
	  ymin <- 0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
	  ymax <- 0.2+1.2*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt))

	  #Må def. pos først for å få strek for hele gruppa bak søylene
	  ### reverserer for å slippe å gjøre det på konf.int
	  pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=T, xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
	                     xlab=xAkseTxt, border=NA, col.axis='white', col='white'))
	  indOK <- which(AggVerdier$Hoved>=0)
	  posOK <- pos[indOK]
	  posOver <- max(pos)+0.35*log(max(pos))
	  posDiff <- 1.2*(pos[1]-pos[2])
	  posOK <- pos[indOK]
	  minpos <- min(posOK)-0.7
	  maxpos <- max(posOK)+0.7
	  

	if (grVar %in% c('ShNavn')) {	#Må si noe om den "gamle figurtypen"
	      #grtxt <- rev(grtxt)
	      mtext(at=posOver, paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25)
	      #Linje for hele landet/utvalget:
	      lines(x=rep(AndelHele, 2), y=c(minpos, maxpos), col=farger[1], lwd=2.5) #y=c(0, max(pos)+0.55), 
	      #Linje for kvalitetsindikatormål:
		      barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, add=TRUE,
	              col=fargeHoved, border=NA, cex.names=cexgr) #, xlim=c(0, xmax), ylim=c(ymin,ymax)
	      soyleXpos <- 1.1*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
	      text(x=soyleXpos, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[1])	#AggVerdier, hvert sykehus
	      }


	#--------------------------------------


      #Legge på gruppe/søylenavn
      mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25) 
      
      #Fordelingsfigurer:
      #if (grVar == '') {
      	if (medSml == 1) { #Legge på prikker for sammenlikning
      		  points(as.numeric(rev(AggVerdier$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
      	}
      	
	
	title(tittel, line=1.5) #cex.main=1.3)

	#Tekst som angir hvilket utvalg som er gjort
	avst <- 0.8
	utvpos <- 3	#Startlinje for teksten
	mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
	
	par('fig'=c(0, 1, 0, 1)) 
	if ( outfile != '') {dev.off()}
	
}  #Figur
} #Funksjon


#      return(invisible(AndelerGrVarData))
      

