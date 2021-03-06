% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NorSpisFigGjsnGrVar.R
\name{NorSpisFigGjsnGrVar}
\alias{NorSpisFigGjsnGrVar}
\title{Søylediagram som viser sentralmål (gj.sn./median) for hvert sykehus}
\usage{
NorSpisFigGjsnGrVar(RegData, valgtVar, valgtMaal = "Gjsn",
  datoFra = "2012-04-01", datoTil = "2050-12-31", minald = 0,
  maxald = 130, erMann = "", outfile = "", hentData = 0,
  preprosess = 1, grVar = "")
}
\arguments{
\item{RegData}{Dataramme med alle nødvendige variable fra registeret}

\item{valgtVar}{Variabelen det skal vises resultat for.
B08StartAldrProbl: alder (år)}

\item{valgtMaal}{Sentralmål 'Med' gir median, alt annet gir gjennomsnitt}

\item{datoFra}{Hoveddato, fra og med. Standard: '2012-01-01'}

\item{datoTil}{Hoveddato, til og med. Standard: '3000-01-01' (siste registreringsdato)}

\item{minald}{Alder, fra og med}

\item{maxald}{Alder, til og med}

\item{erMann}{Kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge}

\item{outfile}{Navn på fil figuren skrives ned til. outfile lik '' skriver til skjerm.}

\item{hentData}{Angir om funksjonen skal kjøre spørring for å hente data eller ikke.
0: ikke kjør (standard)
1: kjør}

\item{preprosess}{Skal data preprosesseres, dvs. gjøre standard omregning av variable og beregne nye.
TRUE (standard) / FALSE}
}
\value{
Figur med...
}
\description{
Funksjon som genererer en figur med gjennomsnitt/median
for hvert sykehus og kan ta inn ulike numeriske variable.
Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
}
