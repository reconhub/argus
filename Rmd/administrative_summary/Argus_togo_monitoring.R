
############
### JG: To review code below and put in a R Markdown format
############











#####################################
##### Donnees 
# Tables MySQL du pilote d'Argus au Togo
##### Etapes
# Connexion à la base de données et export des données d'intérêts
# Monitorer les CS n'envoyant pas les rapports
# Monitorer les dépassements de seuils (lorsque présents sur le fullreport)
# Monitorer les districts, régions, central ne validant pas les rapports
# ## Resultats
# Monitoring du pilote
#######################################

#### Monitorer le pilote de Argus au Togo

library(RMySQL)
library(RSQLite)
library(Hmisc)
library(lubridate)

dateBaseline <- as.Date("27/06/16", format="%d/%m/%y")
dateEstimate <- as.Date("19/06/17", format="%d/%m/%y")


pathFilesInput <- "D:/travaux/National support/Togo/Argus pilot test/monitoring/"

pathFilesOutput <- "D:/travaux/National support/Togo/Argus pilot test/monitoring/"

baseSql <- dbConnect(MySQL(),user = 'root',password = '0000',host = 'localhost',dbname = 'frontlinesms2')

############
### Monitoring
#################

# période d'analyse

############################## METTRE dateBaseline et dateEstimate si non lancé à partir de source()

# dateBaseline <- as.Date("23/05/16", format="%d/%m/%y")
# dateEstimate <- as.Date("22/08/16", format="%d/%m/%y")
# dateEstimate <- today()

# Connexion à la base de données et export des données d'intérêts

# userData <- ses_data[which(as.Date(ses_data$periodStart) >= dateBaseline & as.Date(ses_data$periodStart) <= dateEstimate & ses_data$disease!="ALERT"), c("path","contactPhoneNumber", "periodStart", "disease", "value1")]

userData <- dbGetQuery(baseSql, paste("SELECT path, reception, contactPhoneNumber, periodStart, disease, value1 FROM ses_data WHERE disease <> 'ALERT' AND periodStart >=", paste("'",dateBaseline," 00:00:00'", sep=""),"AND periodStart <=",paste("'",dateEstimate," 23:59:59'", sep=""),";", sep=" "))

# fullreport <- sesdashboard_fullreport[which(as.Date(sesdashboard_fullreport$startDate) >= dateBaseline & as.Date(sesdashboard_fullreport$startDate) <= dateEstimate ), c("id","FK_SiteId","startDate","status","statusModifiedDate")]

fullreport <- dbGetQuery(baseSql, paste("SELECT id, FK_SiteId, startDate, status, aggregate, createdDate, firstValidationDate, firstRejectionDate, statusModifiedDate FROM sesdashboard_fullreport WHERE startDate >=", paste("'",dateBaseline," 00:00:00'", sep=""),"AND startDate <=",paste("'",dateEstimate," 23:59:59'", sep=""),";", sep=" "))

# sites1 <- sesdashboard_sites[, c("id","name","level","path", "parentSiteReference")]

sites <- dbGetQuery(baseSql, "SELECT id, name, level, path, parentSiteReference FROM sesdashboard_sites")

# siteConnect1 <- sesdashboard_user[,c("username","last_login")]

siteConnect <- dbGetQuery(baseSql, "SELECT username, last_login FROM sesdashboard_user")

# partreport1 <- sesdashboard_partreport[which(sesdashboard_partreport$FK_FullReportId %in% fullreport$id),c("id","status","FK_FullReportId")]

partreport <- dbGetQuery(baseSql, paste("SELECT id, status, FK_FullReportId, aggregate, createdDate, statusModifiedDate, firstValidationDate, firstRejectionDate FROM sesdashboard_partreport WHERE FK_FullReportId IN (",paste(fullreport$id,collapse =","),");"))

# report1 <- sesdashboard_report[which(sesdashboard_report$FK_PartReportId %in% partreport$id),c("id","disease","FK_PartReportId")]

report <- dbGetQuery(baseSql, paste("SELECT id, disease, isArchived, FK_PartReportId FROM sesdashboard_report WHERE FK_PartReportId IN (",paste(partreport$id,collapse =","),");"))

# reportvalues1 <- sesdashboard_reportvalues[which(sesdashboard_reportvalues$FK_ReportId %in% report$id),c("Key","Value","FK_ReportId")]

reportvalues <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_reportvalues WHERE FK_ReportId IN (",paste(report$id,collapse =","),");"))

sesGateway <- dbGetQuery(baseSql,"SELECT * FROM ses_gateway_queue WHERE sent IS NULL")

sesGatewayFull <- dbGetQuery(baseSql,"SELECT * FROM ses_gateway_queue")

dbDisconnect(baseSql)

### nettoyage des données

gateway <- read.csv(paste(pathFilesInput,"gateway.csv", sep=""),colClasses = "character")

gateway$name2 <-  gsub(pattern = "_", replacement = "", x = gateway$name2)
gateway$name2 <-  gsub(pattern = "-", replacement = "", x = gateway$name2)
gateway$name2[which(gateway$name2=="esperance")] <- "hopitalesperance"
gateway$name2[which(gateway$name2=="dominique")] <- "saintdominique"
gateway$name2[which(gateway$name2=="kwampit")] <- "kwampitbong"
gateway$name2[which(gateway$name2=="josephbogou")] <- "hopstjosephbogou"
gateway$name2[which(gateway$name2=="donorione")] <- "donorionebombouaka"
gateway$name2[which(gateway$name2=="pmi")] <- "korpmi"
gateway$name2[which(gateway$name2=="kountong")] <- "kountongbong"
gateway$name2[which(gateway$name2=="tantigou")] <- "pediatrietantigou"
gateway$name2[which(gateway$name2=="polyclinique")] <- "polycliniquedelome"
gateway$name2[which(gateway$name2=="voirie")] <- "dstvoirie"
gateway$name2[which(gateway$name2=="infpp")] <- "infinfpp"
gateway$name2[which(gateway$name2=="cslome")] <- "csdelome"
gateway$name2[which(gateway$name2=="camillelellis")] <- "stcamilledelellis"
gateway$name2[which(gateway$name2=="loisel")] <- "saintloisel"
gateway$name2[which(gateway$name2=="bernardin")] <- "saintbernardin"
gateway$name2[which(gateway$name2=="misericorde")] <- "nddelamisericorde"
gateway$name2[which(gateway$name2=="diagnostic")] <- "lediagnostic"
gateway$name2[which(gateway$name2=="messie")] <- "lemessie"
gateway$name2[which(gateway$name2=="chrlome")] <- "chrlomecommune"
gateway$name2[which(gateway$name2=="carmel")] <- "montcarmel"
gateway$name2[which(gateway$name2=="mariereine")] <- "saintemariereine"
gateway$name2[which(gateway$name2=="attikoume")] <- "beattikoume"
gateway$name2[which(gateway$name2=="cstt")] <- "musacstt"
gateway$name2[which(gateway$name2=="louganda")] <- "saintmartyrdelouganda"
gateway$name2[which(gateway$name2=="cinkasse")] <- "hopcinkasse"
gateway$name2[which(gateway$name2=="district1admin")] <- "district1"
gateway$name2[which(gateway$name2=="district2admin")] <- "district2"
gateway$name2[which(gateway$name2=="district3admin")] <- "district3"
gateway$name2[which(gateway$name2=="district4admin")] <- "district4"
gateway$name2[which(gateway$name2=="district5admin")] <- "district5"
gateway$name2[which(gateway$name2=="cinkasseadmin")] <- "cinkasse"
gateway$name2[which(gateway$name2=="kpendjaladmin")] <- "kpendjal"
gateway$name2[which(gateway$name2=="otiadmin")] <- "oti"
gateway$name2[which(gateway$name2=="tandjoareadmin")] <- "tandjaore"
gateway$name2[which(gateway$name2=="toneadmin")] <- "tone"
gateway$name2[which(gateway$name2=="lomeadmin")] <- "lomecommune"
gateway$name2[which(gateway$name2=="savanesadmin")] <- "savanes"
gateway$name2[which(gateway$name2=="togoadmin")] <- "togo"


userData$week <- week(userData$periodStart)
fullreport$week <- week(fullreport$startDate)

siteConnect$name2 <- tolower(siteConnect$username)
siteConnect$name2 <-  gsub(pattern = "_", replacement = "", x = siteConnect$name2)
siteConnect$name2 <-  gsub(pattern = "-", replacement = "", x = siteConnect$name2)
siteConnect$name2[which(siteConnect$name2=="esperance")] <- "hopitalesperance"
siteConnect$name2[which(siteConnect$name2=="dominique")] <- "saintdominique"
siteConnect$name2[which(siteConnect$name2=="kwampit")] <- "kwampitbong"
siteConnect$name2[which(siteConnect$name2=="josephbogou")] <- "hopstjosephbogou"
siteConnect$name2[which(siteConnect$name2=="donorione")] <- "donorionebombouaka"
siteConnect$name2[which(siteConnect$name2=="pmi")] <- "korpmi"
siteConnect$name2[which(siteConnect$name2=="kountong")] <- "kountongbong"
siteConnect$name2[which(siteConnect$name2=="tantigou")] <- "pediatrietantigou"
siteConnect$name2[which(siteConnect$name2=="polyclinique")] <- "polycliniquedelome"
siteConnect$name2[which(siteConnect$name2=="voirie")] <- "dstvoirie"
siteConnect$name2[which(siteConnect$name2=="infpp")] <- "infinfpp"
siteConnect$name2[which(siteConnect$name2=="cslome")] <- "csdelome"
siteConnect$name2[which(siteConnect$name2=="camillelellis")] <- "stcamilledelellis"
siteConnect$name2[which(siteConnect$name2=="loisel")] <- "saintloisel"
siteConnect$name2[which(siteConnect$name2=="bernardin")] <- "saintbernardin"
siteConnect$name2[which(siteConnect$name2=="misericorde")] <- "nddelamisericorde"
siteConnect$name2[which(siteConnect$name2=="diagnostic")] <- "lediagnostic"
siteConnect$name2[which(siteConnect$name2=="messie")] <- "lemessie"
siteConnect$name2[which(siteConnect$name2=="chrlome")] <- "chrlomecommune"
siteConnect$name2[which(siteConnect$name2=="carmel")] <- "montcarmel"
siteConnect$name2[which(siteConnect$name2=="mariereine")] <- "saintemariereine"
siteConnect$name2[which(siteConnect$name2=="attikoume")] <- "beattikoume"
siteConnect$name2[which(siteConnect$name2=="cstt")] <- "musacstt"
siteConnect$name2[which(siteConnect$name2=="louganda")] <- "saintmartyrdelouganda"
siteConnect$name2[which(siteConnect$name2=="cinkasse")] <- "hopcinkasse"
siteConnect$name2[which(siteConnect$name2=="district1admin")] <- "district1"
siteConnect$name2[which(siteConnect$name2=="district2admin")] <- "district2"
siteConnect$name2[which(siteConnect$name2=="district3admin")] <- "district3"
siteConnect$name2[which(siteConnect$name2=="district4admin")] <- "district4"
siteConnect$name2[which(siteConnect$name2=="district5admin")] <- "district5"
siteConnect$name2[which(siteConnect$name2=="cinkasseadmin")] <- "cinkasse"
siteConnect$name2[which(siteConnect$name2=="kpendjaladmin")] <- "kpendjal"
siteConnect$name2[which(siteConnect$name2=="otiadmin")] <- "oti"
siteConnect$name2[which(siteConnect$name2=="tandjoareadmin")] <- "tandjaore"
siteConnect$name2[which(siteConnect$name2=="toneadmin")] <- "tone"
siteConnect$name2[which(siteConnect$name2=="lomeadmin")] <- "lomecommune"
siteConnect$name2[which(siteConnect$name2=="savanesadmin")] <- "savanes"
siteConnect$name2[which(siteConnect$name2=="togoadmin")] <- "togo"

sites$name2 <- tolower(sites$name)
sites$name2 <-  gsub(pattern = "_", replacement = "", x = sites$name2)

userData$name2 <- userData$path

listZone <- sites[which(sites$level %in% c(1,2,3)),c("name","level","name2")]
listZone[which(duplicated(listZone$name))]
listZone$name[which(listZone$name=="Lome_Commune")] <- "LomeCom"

for (i in 1:length(listZone$name)) {
  userData$name2 <- gsub(pattern = listZone$name[i], replacement = "", x = userData$name2)
}
userData$name2 <-  gsub(pattern = "SitesRoot", replacement = "", x = userData$name2)
userData$name2 <-  gsub(pattern = "/", replacement = "", x = userData$name2)
userData$name2 <-  gsub(pattern = "_", replacement = "", x = userData$name2)
userData$name2 <-  tolower(userData$name2)

userData$name2[which(userData$name2=="hop")] <- "hopcinkasse"
userData$name2[which(userData$name2 %nin% sites$name2)]
# sites$name2[which(sites$name2 %nin% userData$name2)]
# siteConnect$name2[which(siteConnect$name2 %nin% sites$name2)]
sites$name2[which(sites$name2 %nin% siteConnect$name2)]

listZone <- sites[which(sites$level %in% c(1,2,3)),c("name","level","name2")]
listZone[which(duplicated(listZone$name))]
listZone$name[which(listZone$name=="Lome_Commune")] <- "LomeCom"

reportvalues <- reportvalues[,-c(1,5,6)]

### imputation des données dans table fullreport à partir de la table userData du fait du changement de structure de la base de données en cours de pilote

userData$reception <- as.POSIXct(userData$reception)

# createdDate

fullreportOld <- which(is.na(fullreport$createdDate))

for (i in fullreportOld) {
  temp <-NA
  temp <- which(userData$name2==sites$name2[which(sites$id==fullreport$FK_SiteId[i])] & userData$week==fullreport$week[i])
  fullreport$createdDate[i] <- ifelse(length(temp)==0,NA,as.character(min(userData$reception[temp])))
}
fullreport$createdDate <- as.POSIXct(fullreport$createdDate)

# firstValidationDate

fullreport$firstValidationDate <- ifelse(fullreport$status=="VALIDATED" & is.na(fullreport$firstValidationDate),fullreport$statusModifiedDate,fullreport$firstValidationDate)

# firstRejectionDate

fullreport$firstRejectionDate <- ifelse(fullreport$status=="REJECTED" & is.na(fullreport$firstRejectionDate),fullreport$statusModifiedDate,fullreport$firstRejectionDate)

  # création matrice de monitoring

# retirer sites avec téléphones non distribués
sites2 <- sites[-which(sites$name %in% c( "Lowossou","Meilleure_Sante","Veritas","Le_Diagnostic","Lotus","Novissi","Gendarmerie","Jerusalem","FAT","ATBEF","GADH","MUSA_CSTT","Saint_martyr_de_louganda")),]

# numSem <- seq(from=week(dateBaseline), to=week(dateEstimate), by=1)
numSem <- c(seq(from=week(dateEstimate), to=1, by=-1),seq(from=52, to=week(dateBaseline), by=-1))

monitorPilot <- matrix(c("semaine","zone","nb rapports envoyes","% de rapports envoyés","nb rapports envoyes a temps","% rapports envoyés à temps" ,"nb rapports validés" ,"% rapports validés","% rapports statut validés","derniere connexion admin site"),nrow = 1 )

fullreport$startDate <- as.POSIXct(fullreport$startDate)
fullreport$firstValidationDate <- as.POSIXct(fullreport$firstValidationDate)
fullreport$firstRejectionDate <- as.POSIXct(fullreport$firstRejectionDate)

for (i in 1:length(numSem)) {
  temp <- matrix(NA, nrow=1, ncol = 10)
  temp[1,1] <- paste("semaine", numSem[i])
  monitorPilot <- rbind(monitorPilot,temp)

  for (j in 1:length(listZone$name)) {
    temp <- matrix(NA, nrow=1, ncol = 10)
    temp[1,1] <- numSem[i]
    temp[1,2] <- listZone$name[j]
    
    temp2 <- NA
    temp2 <- sites2[grep(x=sites2$path,pattern=listZone$name[j]),]
    temp2 <- temp2$id[which(temp2$level==4)]
      
    temp3 <- NA
    temp3 <- fullreport$id[which(fullreport$FK_SiteId %in% temp2 & fullreport$week==numSem[i])]
    
    temp[1,3] <- paste("n=",length(temp3),"/",length(temp2))
    temp[1,4] <- paste(round(length(temp3)/length(temp2),3)*100,"%")
    
    temp4 <- NA
    temp4 <- fullreport$id[which(fullreport$FK_SiteId %in% temp2 & fullreport$week==numSem[i] & fullreport$createdDate < (fullreport$startDate+662400))]
    temp[1,5] <- paste("n=",length(temp4),"/",length(temp2))
    temp[1,6] <- paste(round(length(temp4)/length(temp2),3)*100,"%")
    
    temp5 <- NA
    temp5 <- sites2[grep(x=sites2$path,pattern=listZone$name[j]),]
    temp5 <- temp5$id[which(temp5$level==(listZone$level[j]+1))]
    temp6 <- NA
    temp6 <- fullreport$id[which(fullreport$FK_SiteId %in% temp5 & fullreport$week==numSem[i] & fullreport$status!="PENDING")]
    
    temp7 <- NA
    temp7 <- fullreport$id[which(fullreport$FK_SiteId %in% temp5 & fullreport$week==numSem[i])]
    
    temp[1,7] <- paste("n=",length(temp6),"/",length(temp7))
    temp[1,8] <- paste(round(length(temp6) / length(temp7),3)*100,"%") 
    
    temp8 <- NA
    temp8 <- fullreport$id[which(fullreport$FK_SiteId %in% temp5 & fullreport$week==numSem[i] & fullreport$status=="VALIDATED")]
    
    temp[1,9] <- paste(round(length(temp8) / length(temp7),3)*100,"%")  
    temp[1,10] <- siteConnect$last_login[which(siteConnect$name2==listZone$name2[j])]
    monitorPilot <- rbind(monitorPilot,temp)
  }
}

monitorPilot <- data.frame(monitorPilot, stringsAsFactors = F)
colnames(monitorPilot) <- as.character(monitorPilot[1,])
monitorPilot <- monitorPilot[-1,]

write.csv(monitorPilot,paste(pathFilesOutput,"monitorPilot.csv", sep=""),na="",row.names=F, quote = T)


# Création de matrice de monitoring en fonction des gateways

sites2$gateway <- NA
for (i in 1:length(sites2$name2)) {
  sites2$gateway[i] <- ifelse(sites2$level[i]==4,gateway$gateway[which(gateway$name2==sites2$name2[i])],NA)
}

listGateway <- unique(sites2$gateway[which(!is.na(sites2$gateway))])

corrListNum <- data.frame(nom=c("Gateway 1","Gateway 2","Gateway 3","Gateway 4","Gateway 5","Gateway 6","Gateway 7","Gateway 8"),num=c("+22893299870","+22893310359","+22899068027","+22898410436","+22898416909","+22898684266","+22893299832","+22893299848"), stringsAsFactors = F)

monitorPilotGateway <- matrix(c("semaine","gateway","nb rapports envoyes","% de rapports envoyés","nb rapports envoyes a temps","% rapports envoyés à temps","numéro","nb messages pending","date plus vieux message pending"),nrow = 1 )

sesGateway$creationDate <- as.POSIXct(sesGateway$creationDate)
corrListNum$dateOld <- NA
corrListNum$dateOld <- as.POSIXct(corrListNum$dateOld)

for (i in 1:length(corrListNum$num)) {
  corrListNum$pending[i] <- length(which(sesGateway$gatewayId==corrListNum$num[i]))
  corrListNum$dateOld[i] <- if(corrListNum$pending[i]==0) {NA} else {min(sesGateway$creationDate[which(sesGateway$gatewayId==corrListNum$num[i])])}
}

corrListNum$dateOld <- as.character(corrListNum$dateOld)

for (i in 1:length(numSem)) {
  temp <- matrix(NA, nrow=1, ncol = 9)
  temp[1,1] <- paste("semaine", numSem[i])
  monitorPilotGateway <- rbind(monitorPilotGateway,temp)

  for (j in 1:length(listGateway)) {
    temp <- matrix(NA, nrow=1, ncol = 9)
    temp[1,1] <- numSem[i]
    temp[1,2] <- listGateway[j]
    
    temp2 <- NA
    temp2 <- sites2$id[which(sites2$gateway==listGateway[j])]
    
    temp3 <- NA
    temp3 <- fullreport$id[which(fullreport$FK_SiteId %in% temp2 & fullreport$week==numSem[i])]
    
    temp[1,3] <- paste("n=",length(temp3),"/",length(temp2))
    temp[1,4] <- paste(round(length(temp3)/length(temp2),3)*100,"%")
    
    temp4 <- NA
    temp4 <- fullreport$id[which(fullreport$FK_SiteId %in% temp2 & fullreport$week==numSem[i] & fullreport$createdDate < (fullreport$startDate+662400))]
    temp[1,5] <- paste("n=",length(temp4),"/",length(temp2))
    temp[1,6] <- paste(round(length(temp4)/length(temp2),3)*100,"%")
    
    temp[1,7] <- corrListNum$num[which(corrListNum$nom==listGateway[j])]
    
    temp[1,8] <- corrListNum$pending[which(corrListNum$nom==listGateway[j])]
    
    temp[1,9] <- corrListNum$dateOld[which(corrListNum$nom==listGateway[j])]
    
    monitorPilotGateway <- rbind(monitorPilotGateway,temp)
  }
}

monitorPilotGateway <- data.frame(monitorPilotGateway, stringsAsFactors = F)
colnames(monitorPilotGateway) <- as.character(monitorPilotGateway[1,])
monitorPilotGateway <- monitorPilotGateway[-1,]

write.csv(monitorPilotGateway,paste(pathFilesOutput,"monitorPilotGateway.csv",sep=""),na="",row.names=F, quote = T)


# Monitorer les CS n'envoyant pas les rapports ou avec rapports incomplets

monitorNoReport <- matrix(c("periode zone","site","nb rapports manquants","semaines rapport manquant","nb rapports incomplets","semaines rapports incomplets"),nrow = 1 )

listSitePeriph <- sites[which(sites$level==4),]
temp <- matrix(NA, nrow=1, ncol = 6)
temp[1,1] <- paste(as.character(dateBaseline)," S",numSem[length(numSem)], " /", as.character(dateEstimate)," S",numSem[1],sep="")
monitorNoReport <- rbind(monitorNoReport,temp)

for (i in 1:length(listSitePeriph$id)) {
  temp <- matrix(NA, nrow=1, ncol = 6)
  temp[1,1] <- listSitePeriph$parentSiteReference[i]
  temp[1,2] <- listSitePeriph$name[i]
  temp2 <- NA
  temp2 <- userData[which(userData$name2==listSitePeriph$name2[i]),]
  temp3 <- NA
  temp3 <- table(temp2$week)
  temp[1,3] <- paste("n=",length(numSem) - length(temp3),"/",length(numSem))
  temp[1,4] <- paste(numSem[which(as.character(numSem) %nin% names(temp3))],sep="", collapse ="-")
  temp4<- NA
  temp4<- by(data=temp2$disease, INDICES=temp2$week, FUN=unique)
    lgLev <- function(x) length(levels(as.factor(as.character(x))))
  temp4 <- t(as.matrix(lapply(temp4, lgLev)))  
  temp[1,5] <- paste("n=",ifelse(any(temp4<13),length(which(temp4<13)),0),"/",length(numSem))
  temp[1,6] <- paste(colnames(temp4)[which(temp4<13)],sep="", collapse ="-")
  monitorNoReport <- rbind(monitorNoReport,temp)
}

monitorNoReport <- data.frame(monitorNoReport, stringsAsFactors = F)
colnames(monitorNoReport) <- as.character(monitorNoReport[1,])
monitorNoReport <- monitorNoReport[-1,]

write.csv(monitorNoReport,"D:/travaux/National support/Togo/Argus pilot test/monitoring/monitorNoReport.csv",na="",row.names=F, quote = T)

# Monitorer les dépassements de seuils qui ont été validés

userDataSeuil <- userData[which((userData$disease %in% c("cholera","fievre_jaune","tetanos_neonatal","PFA","charbon_humain","rage","sd_FHA") & as.numeric(userData$value1)>=1) | (userData$disease=="rougeole" & as.numeric(userData$value1)>=3) | (userData$disease=="meningite" & as.numeric(userData$value1)>=2)),c("disease","week","name2")]

userDataSeuil <- unique(userDataSeuil)
userDataSeuil$fullId <- NA
userDataSeuil$partId <- NA
userDataSeuil$reportId <- NA
userDataSeuil$value <- NA
userDataSeuil$status <- NA
userDataSeuil$statusModifiedDate <- NA
userDataSeuil$zone <- NA
userDataSeuil$name <- NA

for (i in 1:length(userDataSeuil$name2)) {
  userDataSeuil$fullId[i] <- fullreport$id[which(fullreport$FK_SiteId==sites$id[which(sites$name2==userDataSeuil$name2[i])] & fullreport$week==userDataSeuil$week[i])]
  temp <- NA
  temp <-  partreport$id[which(partreport$FK_FullReportId==userDataSeuil$fullId[i] & partreport$status=="VALIDATED")]
  userDataSeuil$partId[i] <- ifelse(length(temp)==0,NA,temp)
  userDataSeuil$reportId[i] <- ifelse(length(temp)==0,NA,report$id[which(report$disease==userDataSeuil$disease[i] & report$FK_PartReportId==userDataSeuil$partId[i] & report$isArchived==0)])
  userDataSeuil$value[i] <- ifelse(length(temp)==0,NA,reportvalues$Value[which(reportvalues$Key=="cas" & reportvalues$FK_ReportId==userDataSeuil$reportId[i])])
  userDataSeuil[i,c("status","statusModifiedDate")] <- fullreport[which(fullreport$id==userDataSeuil$fullId[i]),c("status","statusModifiedDate")]
  userDataSeuil$zone[i] <- sites$parentSiteReference[which(sites$name2==userDataSeuil$name2[i])]
  userDataSeuil$name[i] <- sites$name[which(sites$name2==userDataSeuil$name2[i])]
}

monitorSeuil <- userDataSeuil[,c("week","zone","name","disease","value","status","statusModifiedDate")]
monitorSeuil <- monitorSeuil[-which(monitorSeuil$status!="VALIDATED" | (monitorSeuil$disease %in% c("cholera","fievre_jaune","tetanos_neonatal","PFA","charbon_humain","rage","sd_FHA") & as.numeric(monitorSeuil$value)==0) | (monitorSeuil$disease=="rougeole" & as.numeric(monitorSeuil$value)<3) | (monitorSeuil$disease=="meningite" & as.numeric(monitorSeuil$value)<2)),]

monitorSeuil$status <- tolower(monitorSeuil$status)
monitorSeuil <- monitorSeuil[order(monitorSeuil$week),]
  
colnames(monitorSeuil) <- c("semaine","zone","site","maladie","nb de cas", "statut rapport","date de dernier statut")

write.csv(monitorSeuil,"D:/travaux/National support/Togo/Argus pilot test/monitoring/monitorSeuil.csv",na="",row.names=F, quote = T)

