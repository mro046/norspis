#' Søylediagram med AggVerdier for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med AggVerdier av en gitt variabel for ei valgt gruppering, 
#' f.eks. enheter. I øyeblikket benytter funksjonen bare 'EnhNavn' som grupperingsvariabel, men 
#' andre valg kan lett inkluderes. 
#'
#' Figurtypen som genereres er avhengig av valgtVar. Ved valgtVar='InnMaate', får man ei oversikt oversikt
#' over andel av innkomsttype ved hver enhet, dvs. ei fordeling av de tre innkomsttypene for hver enhet.
#' For andre "valgtVar" viser figuren andel av den valgte variabelen for hver enhet.
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder_u18: Pasienter under 18 år
#'     \item BehDodUnderBeh: Andel pasienter som døde i behandling
#'     \item DiagVDiabetes: Andel pasienter med diabetes
#'     \item VentetidOver2Uker: Andel med ventetid over 2 uker, beregnet fra dato for mottatt henvisning til hendelsesdato.
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
NorSpisFigAndelerGrVar <- function(RegData, valgtVar, datoFra=0, datoTil=0, minald=0, maxald=130, 
                                   grVar='EnhNavn', erMann='', hentData=0, preprosess=1, outfile='', lagFig=1)                                   


{

#------ Hente data      
       #NB: Tomme grVar fjernes så vurder om dette kan være standard...
      if (hentData == 1) {		
            RegData <- NorSpisELAlleScorData(datoFra=datoFra, datoTil=datoTil)
      }

#------ Preprosessere data
      # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
      if (preprosess){
            RegData <- NorSpisPreprosess(RegData=RegData)	#, reshID=reshID)
      }
      

#------- Tilrettelegge variable
NorSpisVarSpes <- NorSpisVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)
 #--------og hente inn RegData og parametre fra tilretteleggingen
RegData <- NorSpisVarSpes$RegData
tittel <- NorSpisVarSpes$tittel
	  
#------- Gjøre utvalg
NorSpisUtvalg <- NorSpisUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar, minald=minald, 
                               maxald=maxald, erMann=erMann, reshID=reshID)
            
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
      
      if(N > 0) {
            Ngr <- table(RegData[ ,grVar])      #lager en tabell med antall(N) (e.g. antall under 18 år) i hver gruppe(e.g. på hvert sykehus)
      } else {
            Ngr <- 0}
      
      AntGr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
      AndelerGr <- as.vector(table(RegData[which(RegData$Variabel==1) , grVar])/Ngr*100)	#round(100*Nvar/Ngr,2) #beregner størrelsen på andelene i hver gruppe, e.g. andeler pasienter under 18 år på ulike sykehus
      
      
      if (sum(which(Ngr < Ngrense))>0) {
            indGrUt <- as.numeric(which(Ngr<Ngrense))
      } else {
            indGrUt <- 0}
      
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

if (dim(RegData)[1] < 10 ) {

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
	fargeHoved <- ifelse(grVar %in% c('EnhNavn'), farger[4], farger[1])
	fargeRest <- farger[3]
	antGr <- length(grtxt)
	lwdRest <- 3	#tykkelse på linja som repr. landet
	cexleg <- 0.9	#Størrelse på legendtekst
	
	
	#Definerer disse i beregningsfunksjonen?  
      xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.2
      xmax <- min(xmax, 100) 	#100 som maks bare hvis andelsfigur..
	  ymin <- 0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
	  ymax <- 0.2+1.2*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt))

	  
	  #HERFRA BEGYNNER SELVE TEGNINGEN AV FIGUREN
	  #Må def. pos først for å få strek for hele gruppa bak søylene
	  ### reverserer for å slippe å gjøre det på konf.int
	  pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=T, xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
	                     xlab=xAkseTxt, border=NA, col.axis='white', col='white'))
	  indOK <- which(AggVerdier$Hoved>=0)
	  posOver <- max(pos)+0.35*log(max(pos))
	  #posDiff <- 1.2*(pos[1]-pos[2])
	  minpos <- min(pos[indOK])-0.7
	  maxpos <- max(pos[indOK])+0.7
	  
      #grtxt <- rev(grtxt)
      mtext(at=posOver, paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25) #legger til en "(N)"
      #Linje for hele landet/utvalget:
      lines(x=rep(AndelHele, 2), y=c(minpos, maxpos), col=farger[1], lwd=2.5) #y=c(0, max(pos)+0.55), #mads-denne skal vel lage en vertikal linje som representerer hele landet, men fungerer ikke på nåværende tidspunkt. 
      #Linje for kvalitetsindikatormål:
	      barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, add=TRUE,
              col=farger[3], border=NA, cex.names=cexgr) #, xlim=c(0, xmax), ylim=c(ymin,ymax), col=fargeHoved
      soyleXpos <- 1.5*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr #mads endret fra 1.1 til 1.5
      text(x=soyleXpos, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[1])	#AggVerdier, hvert sykehus

      #Legge på gruppe/søylenavn
      mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25) 
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
      

