####
## JG: to review and put in R markdown format
####



#### Monitorer le pilote de Argus au Togo

library(RMySQL)
library(RSQLite)
library(Hmisc)
library(lubridate)
library(ggplot2)
library(grid)
library(gtools)

########################
##*************************** revoir le script avec les nouvelles variables de la base de donn�es, prendre exemple sur le script monitoring
#*****************************************************************

baseSql <- dbConnect(MySQL(),user = 'root',password = '0000',host = 'localhost',dbname = 'frontlinesms2')

# période d'analyse

# depuis le début du pilote
dateBaseline <- as.Date("23/05/16", format="%d/%m/%y")
# période d'interet pour le rapport
dateBaseline2 <- as.Date("30/01/17", format="%d/%m/%y")
dateEstimate <- as.Date("20/02/17", format="%d/%m/%y")

numSem <- seq(from=week(dateBaseline), to=week(dateEstimate), by=1)
numSem2 <- seq(from=week(dateBaseline2), to=week(dateEstimate), by=1)

# chemin pour les fichiers:
path <- "D:/travaux/National support/Togo/Argus pilot test/rapport_mensuel/results_february/"

# Création des bases de données pour les analyses

userData <- dbGetQuery(baseSql, paste("SELECT path, reception, contactPhoneNumber, periodStart, disease, value1 FROM ses_data WHERE disease <> 'ALERT' AND periodStart >=", paste("'",dateBaseline," 00:00:00'", sep=""),"AND periodStart <=",paste("'",dateEstimate," 23:59:59'", sep=""),";", sep=" "))

fullreport <- dbGetQuery(baseSql, paste("SELECT id, FK_SiteId, startDate, status, aggregate, createdDate, firstValidationDate, firstRejectionDate, statusModifiedDate FROM sesdashboard_fullreport WHERE startDate >=", paste("'",dateBaseline," 00:00:00'", sep=""),"AND startDate <=",paste("'",dateEstimate," 23:59:59'", sep=""),";", sep=" "))

sites <- dbGetQuery(baseSql, "SELECT id, name, level, path, parentSiteReference FROM sesdashboard_sites")

partreport <- dbGetQuery(baseSql, paste("SELECT id, status, FK_FullReportId, aggregate, createdDate, statusModifiedDate, firstValidationDate, firstRejectionDate FROM sesdashboard_partreport WHERE FK_FullReportId IN (",paste(fullreport$id,collapse =","),");"))

report <- dbGetQuery(baseSql, paste("SELECT id, disease, status, isArchived, FK_PartReportId, receptionDate FROM sesdashboard_report WHERE FK_PartReportId IN (",paste(partreport$id,collapse =","),");"))

reportvalues <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_reportvalues WHERE FK_ReportId IN (",paste(report$id,collapse =","),");"))

dbDisconnect(baseSql)
