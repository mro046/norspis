#' Søylediagram, horisontalt eller vertikalt, som viser andeler av valgt variabel.
#'
#' Søylediagrammet viser fordelinga til den valgte variabelen. Søylene er horisontale eller vertikale
#' avhengig av hvor stor plass kategorinavnet til søyla tar.
#'
#' #' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Aldersfordeling, 5-årige grupper
#'     \item BMI: Pasientenes BMI (Body Mass Index)
#'     \item Norsktalende: Snakker pasienten norsk
#'     \item Utdanning: Utdanningsnivå
#'    }
#'
#'
#' @param RegData Dataramme med alle nødvendige variable fra registeret
#' @param outfile Navn på fil figuren skrives ned til
#' @param reshID Avdelingsid (reshID) for egen avdeling,
#' @param hentData Angir om funksjonen skal kjøre spørring for å hente data eller ikke.
#'					0: ikke kjør (standard)
#'					1: kjør
#' @param preprosess Skal data preprosesseres, dvs. gjøre standard omregning av variable og beregne nye.
#'						TRUE (standard) / FALSE
#' @param datoFra Hoveddato, fra og med. Standard: '2012-01-01'
#' @param datoTil Hoveddato, til og med. Standard: '3000-01-01' (siste registreringsdato)
#' @param minald Alder, fra og med
#' @param maxald Alder, til og med
#' @param erMann Kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#' @param enhetsUtvalg Sammenlikning eller ikke: 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.
#'
#' Detajer...:
#'
#' @return En figur med søylediagram (fordeling) av ønsket variabel
#'
#' @export

NorSpisFigAndeler  <- function(RegData, valgtVar, datoFra='2016-01-01', datoTil='3000-12-31',
		minald=0, maxald=130, erMann='', outfile='', hentData=0, preprosess=1,
		reshID, enhetsUtvalg=1)
{

	if (hentData == 1) {
		RegData <- NorSpisELAlleScorData(datoFra=datoFra, datoTil=datoTil)
	  }

# Preprosessere data
     if (preprosess==1){
       RegData <- NorSpisPreprosess(RegData=RegData)
     }
      


#FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG. EKSEMPLER FOR ULIKE SITUASJONER.
#DENNE DELEN VIL VI TRENGE FOR F.EKS. "HONOS, ENKELTLEDD"

#Vi har en "interessekonflikt" for figurer satt sammen av fler variable:
#Utvalg må kjøres etter all variabelfiltrering for å få riktig utvalgstekst i figuren.
#Vi må kjøre utvalg før variabeldefinisjon for å få riktige indekser - 
#Løsning: Må tilrettelegge generelle variable som representerer de sammensatte variablene.

#--------------- Definere variable ------------------------------

NorSpisVarSpes <- NorSpisVarTilrettelegg(RegData=RegData, valgtVar=valgtVar)
RegData <- NorSpisVarSpes$RegData
#Flere av variablene fra NorSpisVarTilrettelegg hentet ut lengre ned.


NorSpisUtvalg <- NorSpisUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, 
                               maxald=maxald, erMann=erMann, enhetsUtvalg=enhetsUtvalg, reshID=reshID)
RegData <- NorSpisUtvalg$RegData
ind <- NorSpisUtvalg$ind
medSml <- NorSpisUtvalg$medSml
hovedgrTxt <- NorSpisUtvalg$hovedgrTxt
smltxt <- NorSpisUtvalg$smltxt
utvalgTxt <- NorSpisUtvalg$utvalgTxt
fargepalett=NorSpisUtvalg$fargepalett
medSml=NorSpisUtvalg$medSml



#----------------------BEREGNINGER
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.

      Andeler <- list(Hoved = 0, Rest =0)
      N <- list(Hoved = 0, Rest =0)
      Ngr <- list(Hoved = 0, Rest =0)
      ind <- NorSpisUtvalg$ind
      
      Ngr$Hoved <- switch(as.character(NorSpisVarSpes$flerevar), 
                          '0' = table(RegData$VariabelGr[ind$Hoved]),
                          '1' = colSums(sapply(RegData[ind$Hoved ,variable], as.numeric), na.rm=T))
      #N$ gjelder nå hvis samme totalutvalg for alle
      N$Hoved <- switch(as.character(NorSpisVarSpes$flerevar), 
                        '0' = sum(Ngr$Hoved),	#length(ind$Hoved)- Kan inneholde NA
                        '1' = length(ind$Hoved))
      Andeler$Hoved <- 100*Ngr$Hoved/N$Hoved
      
      if (NorSpisUtvalg$medSml==1) {
            Ngr$Rest <- switch(as.character(NorSpisVarSpes$flerevar), 
                               '0' = table(RegData$VariabelGr[ind$Rest]),
                               '1' = colSums(sapply(RegData[ind$Rest ,variable], as.numeric), na.rm=T))
            N$Rest <- switch(as.character(NorSpisVarSpes$flerevar), 
                             '0' = sum(Ngr$Rest),	#length(ind$Rest)- Kan inneholde NA
                             '1' = length(ind$Rest))
            Andeler$Rest <- 100*Ngr$Rest/N$Rest
      }
      
      
      grtxt2 <- paste0('(', sprintf('%.1f',Andeler$Hoved), '%)')
      yAkseTxt='Andel pasienter (%)'
      
      FigDataParam <- list(AggVerdier=Andeler, N=N, 
                           Ngr=Ngr,	
                           KImaal <- NorSpisVarSpes$KImaal,
                           #soyletxt=soyletxt,
                           grtxt2=grtxt2, 
                           grtxt=NorSpisVarSpes$grtxt,
                           tittel=NorSpisVarSpes$tittel, 
                           retn=NorSpisVarSpes$retn, 
                           xAkseTxt=NorSpisVarSpes$xAkseTxt,
                           yAkseTxt=yAkseTxt,
                           utvalgTxt=NorSpisUtvalg$utvalgTxt, 
                           fargepalett=NorSpisUtvalg$fargepalett, 
                           medSml=NorSpisUtvalg$medSml,
                           hovedgrTxt=NorSpisUtvalg$hovedgrTxt,
                           smltxt=NorSpisUtvalg$smltxt)
 
#Definerer opp variable siden vi fortsatt genererer figuren i denne funksjonen.     
grtxt=NorSpisVarSpes$grtxt
tittel=NorSpisVarSpes$tittel
retn=NorSpisVarSpes$retn
xAkseTxt=NorSpisVarSpes$xAkseTxt
hovedgrTxt=NorSpisUtvalg$hovedgrTxt

lagFig <- 0      
      if (lagFig == 1) {
            #cexgr <- 1-ifelse(AntGr>20, 0.25*AntGr/60, 0)
            NorSpisFigSoyler(RegData, AggVerdier, Ngr, tittel=NorSpisVarSpes$tittel, hovedgrTxt=NorSpisUtvalg$hovedgrTxt, 
                         smltxt=NorSpisUtvalg$smltxt, Ngr = Ngr, KImaal <- NorSpisVarSpes$KImaal,
                         N=N, retn='V', utvalgTxt, grtxt=NorSpisVarSpes$grtxt, grtxt2=grtxt2, medSml=NorSpisUtvalg$medSml, 
                         xAkseTxt=NorSpisVarSpes$xAkseTxt, yAkseTxt=yAkseTxt, 
                         outfile=outfile)	
            #ENDRE så figurparametrene skrives fullt ut i parameterkallet
      }
      


#-----------Figur---------------------------------------
#Hvis for få observasjoner..
if (dim(RegData)[1] < 0 | (N$Hoved<0 )) {
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
	plot.new()
	title(tittel)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	text(0.5, 0.6, 'For få registreringer', cex=1.2)
	if ( outfile != '') {dev.off()}

} else {

#-----------Figur---------------------------------------
#Innparametre: xAkseTxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr
cexgr <- 1	#Kan endres for enkeltvariable
antDes <- 1


#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=NorSpisUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
antDesTxt <- paste0('%.', antDes, 'f')
grtxtpst <- paste0(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)')
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

farger <- FigTypUt$farger
fargeHoved <- farger[1]
fargeRest <- farger[3]
antGr <- length(grtxt)
lwdRest <- 1	#tykkelse på linja som repr. landet
cexleg <- 1	#Størrelse på legendtekst

#Horisontale søyler
if (retn == 'H') {
	xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel,
		col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
	if (N$Hoved>0) {mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)}

	if (medSml == 1) {
		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
		legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'),
						paste0(smltxt, ' (N=', N$Rest,')')),
			border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
			lty=NA, ncol=1,lwd=lwdRest, cex=cexleg) #
		} else {
		legend('top', paste0(hovedgrTxt, ' (N=', N$Hoved,')'),
			border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		}
}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	if (length(grtxt2) == 1) {grtxt2 <- paste0('(', sprintf(antDesTxt, Andeler$Hoved), '%)')}
	ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
		xlab=xAkseTxt, col=fargeHoved, border='white', ylim=c(0, ymax))	
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
if (medSml == 1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
	legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')),
		border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
		lwd=lwdRest, ncol=2, cex=cexleg)	#
	} else {
	legend('top', paste0(hovedgrTxt, ' (N=', N$Hoved,')'),
		border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
	}
}

title(tittel, line=1, font.main=1)

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}
}

#Beregninger som returneres fra funksjonen.
AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
rownames(AndelerUt) <- c('Hoved', 'Rest')
AntallUt <- rbind(N$Hoved, N$Rest)
rownames(AntallUt) <- c('Hoved', 'Rest')

UtData <- list(paste0(toString(tittel),'.'), AndelerUt, AntallUt, grtxt )
names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
return(invisible(UtData))

}
