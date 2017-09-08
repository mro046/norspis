#' Hente data fra enkeltspørsmål (enkeltledd), NorSpis
#'
#' Henter data fra staging
#'
#' @inheritParams NorSpisFigAndeler
#'
#' @return RegData data frame
#' @export
#'
NorSpisELAlleScorData <- function(datoFra = '2015-01-01', datoTil = '2099-01-01') {
      
      registryName <- "norspis"
      dbType <- "mysql"
      
      query <- paste0('SELECT  
 PasientAlder, 
   B01Sivilstatus,
   B02EgneBarn, 
   B03Bosituasjon, 
   B04PabegyntUtd,
   B05FullfortUtd, 
   B06Hovedaktivitet,
   B06Hovedaktivitet,
   B07Hovedinntekt,
   B08StartAldrProbl,
   B11FamilieSF,
   B12dArTilBehstart,
   B12dMndTilBehstart,
   B12cAldrForsteBeh,
   B12TidlBehSF,
   B17FysMishandl,
   B18PsykMishandl,
   B19Overgrep,
   B20Mobbing,
   B21SelvskadTidl,
   B22SelvskadSisteAr,
   B23SelvmordFTidl,
   B24SelvmordFSisteAr,
   B25Avhengighet,
   BehDodUnderBeh,
   BehUtfallsvurdSamlet,
   BehVidereBeh,
   CIA30GlobalScore,
   DiagVDiabetes,
   DiagVSF,
   ErMann,
   EDEQ60GlobalScore,
   EDEQ60Kroppsform,
   EDEQ60Restriksjon,
   EDEQ60Spising,
   EDEQ60Vekt,
   H01Atferd,
   H02Selvskade,
   H03Rusmisbruk,
   H04KognitiveProbl,
   H05FysiskeProbl,
   H06Hallusinasjoner,
   H07Stemningsleie,
   H08AndreProbl,
   H09ForhAndre,
   H10ADLProbl,
   H11BoligProbl,
   H12YrkeProbl,
   HCA01Atferd,
   HCA02Aktivitetsniva,
   HCA03Selvskade,
   HCA04Rusmisbruk,
   HCA05SkoleSprak,
   HCA06FysiskProblem,
   HCA07Hallusinasjoner,
   HCA08SomatiskSymp,
   HCA09EmosjonelleSymp,
   HCA10JevnaldrProbl, 
   HCA11Egenomsorg,
   HCA12FamilieProbl,
   HCA14ProblKunnskap,
   HCA15Mangelinfo,
   HCASkoleframmote,
   MedAnnenMedBeh,
   MedAntidepressiva,
   MedBenzodiazepiner,
   MedBMI,
   MedIsoBMIBGS,
   MedNevroleptika,
   MedPsykofarmaka,
   Norsktalende,
   PasientAlder,
   PO01Forstod,
   PO02Tillit,
   PO03InfoDiagnose,
   PO04Tilpasset,
   PO05Involvert,
   PO06Organisert,
   PO07Tilfredsstillende,
   PO08Tilgjengelighet,
   PO09Utbytte,
   PO10Pasientsikkerhet,
   PT01OnsketInvolv,
   PT01OnsketInvolv,
   PT02BleInvolv,
   PT02BleInvolv,
   PT03Utfallsvurd,
   PT03Utfallsvurd,
   PT04KontaktBrukerorg,
   PT05OrientertBrukerorg,
   RAND36EndringHelse,
   RAND36FysFunk, 
   RAND36GenHelse, 
   RAND36MentalHelse, 
   RAND36RollebegEmo, 
   RAND36RollebegFys, 
   RAND36Smerte, 
   RAND36SosialFunk, 
   RAND36Tretthet, 
   RegHendelsesdato,
   RegHenvMottattDato,
   RegHenvInstans,
   RegRegtype,
   SCL90TAngst,
   SCL90TDepresjon,
   SCL90TFiendlighet,
   SCL90TFobi,
   SCL90TGSI,
   SCL90TParanoia,
   SCL90TPsykotisk,
   SCL90TSensitivitet,
   SCL90TSomatisering,
   SCL90TTvang,
   ForlopsOversikt.AvdRESH,
   ForlopsOversikt.BasisRegStatus,
   ForlopsOversikt.HovedDato,
   ForlopsOversikt.OppflgRegStatus,
   ForlopsOversikt.OppflgStatus,
   ForlopsOversikt.SykehusNavn
   FROM EnkeltLeddNum 
   INNER JOIN ForlopsOversikt 
   ON EnkeltLeddNum.ForlopsID = ForlopsOversikt.ForlopsID
   LEFT JOIN AlleScorer  
   ON ForlopsOversikt.ForlopsID = AlleScorer.ForlopsID
WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')
      
      #  FROM AlleVarNum
      #  INNER JOIN ForlopsOversikt
      #  ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID
      #  LEFT JOIN FollowupsNum
      #  ON ForlopsOversikt.ForlopsID = FollowupsNum.ForlopsID
      # WHERE HovedDato >= \'', datoFra, '\' AND HovedDato <= \'', datoTil, '\'')
      
      
      RegData <- rapbase::LoadRegData(registryName, query, dbType)
      
      
      return(RegData)
}



