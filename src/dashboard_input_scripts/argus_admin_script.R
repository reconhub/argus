#####################################
#### Objective
# Create the data tables needed for the Argus dashboards

#### Prerequisites
# None

#### Data 
# MySQL database of Argus

#### Steps
# Set the variables of interest
# Set the periods of interest
# Retrieve raw data of interest from SQL database
#
#
#

#### Results
# Data tables with all data needed to create the Argus dashboards
# reportingValues_YR: table with overall indicators of interest since the beginning of the year (week 1) for each level from the first intermediate level:
# .	Id_Site: the id of the site in the sites_id table.
# .	level: the level of the site (1: central level ; max: leafsite)
# .	FK_ParentId: the id of the parent site in the sites_id table.
# .	reference: name of the site.
# .	weeklyTimelinessMinutes: deadline in minutes from Sunday midnight to be on-time (you should not need it).
# .	nbExpected: nb of expected reports
# .	nbReceived: nb of received reports
# .	nbReceivedValidated: nb of reports received and validated at least once (taken into account in the analysis related with the diseases)
# 
# reportingValues_W12:  table with indicators of interest for each of the 12 previous weeks for each level from the first intermediate level:
#   .	Id_Site: the id of the site in the sites_id table.
# .	level: the level of the site (1: central level ; max: leafsite)
# .	FK_ParentId: the id of the parent site in the sites_id table.
# .	reference: name of the site.
# .	weeklyTimelinessMinutes: deadline in minutes from Sunday midnight to be on-time (you should not need it).
# .	week: number of the week
# .	nbExpected: nb of expected reports
# .	nbReceived: nb of received reports from leaf sites
# .	nbReceivedBelow: nb of received reports from below level
# .	nbReceivedValidated: nb of reports received and validated at least once (taken into account in the analysis related with the diseases)
# .	nbRecTime: nb of received reports from leaf sites on time
# .	nbRecTimeBelow: nb of received reports on time from below level on time
# .	nbReviewed: nb of reviewed reports
# .	nbRevTime: nb of reviewed reports on time
# .	compReport: completeness of data reporting for the period
# .	timeReport: timeliness of data reporting for the period
# .	compReview: completeness of data review for the period
# .	timeReview: timeliness of data review for the period
# .	ids_fullreport_recVal: id of the fullreports that has been validated at least once (you should not need it, will be used to create the tables with the data related to diseases)
#
#reportingValues_W12_overall: same table as reportingValues_W12 with aggregated data for the whole period for each site.
#
#
#######################################

library(RMySQL)
library(RSQLite)
library(Hmisc)
library(lubridate)
library(ggplot2)
#library(purrr)
#library(grid)
#library(gtools)

########################################## 
#### Set the variables of interest
##########################################
db_config <- "db/db_config"

get_config <- function(config) {
  config <- utils::read.table(config, sep = "=",
                              col.names = c("key", "value"),
                              as.is = c(1, 2),
                              comment.char = "")
  split(config$value, config$key)
}

config_list <- get_config(db_config)

# first day of the week

weekFirstDay <- 1 # either 1 for Monday or 7 for Sunday

# name of the variables used for the number of cases and the number of deaths
nbCase_label <- 
  
nbDeath_label <- 

##########################################   
#### Set the periods of interest
########################################## 

# function to correct week() based on the first day of the week, if Monday use isoweek(), if Sunday use epiweek()

weekFunction <- function(x) {
  if(weekFirstDay==1) {
    return(isoweek(x))
  } else {
    if(weekFirstDay==7) {
      return(epiweek(x))
    } else {
      return("Error first day week, modify the variable weekFirstDay")
    }
  }
}

# Start date of interest: the first day of the period of interest: first day of week 1 or first day of the 12th previous week (calculation also of first day of the 8th and 3rd previous weeks)

dateStart_W12 <- floor_date(today()-weeks(12),unit="week", week_start = weekFirstDay)
dateStart_W8 <- floor_date(today()-weeks(8),unit="week", week_start = weekFirstDay)
dateStart_W3 <- floor_date(today()-weeks(3),unit="week", week_start = weekFirstDay)

if(weekFunction(floor_date(today(),unit = "year"))==1) {
  dateStart_YR <- floor_date(floor_date(today(),unit = "year"),unit="week",week_start = weekFirstDay)
} else {
  dateStart_YR <- floor_date(floor_date(today(),unit = "year")+7,unit="week",week_start = weekFirstDay)
} 

dateStart <- min(dateStart_W12,dateStart_YR)

# End date of interest for the weekly reports: first day of the last week to be reported

dateEnd <- floor_date(today()-weeks(1),unit="week", week_start = weekFirstDay)

# Creation of a vector including the Weeks of interest

weekStart_W12 <- weekFunction(dateStart_W12)
weekStart_W8 <- weekFunction(dateStart_W8)
weekStart_W3 <- weekFunction(dateStart_W3)

weekEnd <- weekFunction(today()-weeks(1))

nbWeeksYear <- ifelse(weekFunction(floor_date(today(),unit = "year"))==1,weekFunction(floor_date(today(),unit = "year")-weeks(1)),weekFunction(floor_date(today(),unit = "year"))) # to know if 52 or 53 weeks in the previous year.

numSem_W12 <- NA # vector with the 12 previous weeks

if(weekEnd>weekStart_W12){
  numSem_W12 <- seq(from=weekStart_W12, to=weekEnd, by=1)
} else {
  numSem_W12 <- c(seq(from=weekStart_W12, to=nbWeeksYear, by=1), seq(from=1, to=weekEnd, by=1))
}

numSem_W8 <- NA # vector with the 12 previous weeks

if(weekEnd>weekStart_W8){
  numSem_W8 <- seq(from=weekStart_W8, to=weekEnd, by=1)
} else {
  numSem_W8 <- c(seq(from=weekStart_W8, to=nbWeeksYear, by=1), seq(from=1, to=weekEnd, by=1))
}

numSem_W3 <- NA # vector with the 12 previous weeks

if(weekEnd>weekStart_W3){
  numSem_W3 <- seq(from=weekStart_W3, to=weekEnd, by=1)
} else {
  numSem_W3 <- c(seq(from=weekStart_W3, to=nbWeeksYear, by=1), seq(from=1, to=weekEnd, by=1))
}

numSem_YR <- seq(from=1, to=weekEnd, by=1) # vector with the weeks since first week 1

########################################## 
#### Retrieve raw data of interest from the MySQL database
########################################## 

baseSql <- dbConnect(MySQL(), user = config_list$DB_USER,
                     password = config_list$DB_PASSWORD,
                     host = config_list$DB_HOST,
                     dbname = config_list$DB_NAME)

sites_id <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_sites")

sites_contact <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_contacts")

sites_relationships <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_sites_relationship")

sites_dimdates <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_indicatordimdate WHERE fullDate >=", paste("'",dateStart," 00:00:00'", sep=""),"AND fullDate <=",paste("'",today()," 23:59:59'", sep=""),";", sep=" "))

diseases_name <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_diseases")

diseases_variable <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_diseasevalues")

diseases_threshold <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_thresholds")

fullreport <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_fullreport WHERE startDate >=", paste("'",dateStart," 00:00:00'", sep=""),"AND startDate <=",paste("'",today()," 23:59:59'", sep=""),";", sep=" "))

partreport <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_partreport WHERE FK_FullReportId IN (",paste(fullreport$id,collapse =","),");"))

report <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_report WHERE FK_PartReportId IN (",paste(partreport$id,collapse =","),");"))

report_values <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_reportvalues WHERE FK_ReportId IN (",paste(report$id,collapse =","),");"))

alerts <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_alert WHERE receptionDate >=", paste("'",dateStart," 00:00:00'", sep=""),"AND receptionDate <=",paste("'",today()," 23:59:59'", sep=""),";", sep=" "))

dbDisconnect(baseSql)
rm(baseSql)

########################################## 
#### Modification of the format of some variables
########################################## 

# sites_relationships

sites_relationships$FK_DimDateFromId <- ymd(sites_relationships$FK_DimDateFromId) # put as date
sites_relationships$FK_DimDateToId <- ymd(sites_relationships$FK_DimDateToId) # put as date

# fullreport

fullreport$startDate <- ymd_hms(fullreport$startDate)
fullreport$createdDate <- ymd_hms(fullreport$createdDate)


########################################## 
#### Creation of tables of interest for the dashboards
########################################## 

###############
## Preparatory tables
###############

# Stucture of the system

levPeriph <- max(sites_relationships$level)

levCentral <- 1

levFirstInter <- levPeriph-1

levLastInter <- levCentral +1

deadlinePeriph <- unique(sites_id$weeklyTimelinessMinutes[which(sites_id$id %in% sites_relationships$FK_SiteId[which(sites_relationships$level==levPeriph)])])

# Dataframe with one row per parent site

parentSites <- sites_relationships[-which(sites_relationships$level==levPeriph),c("FK_SiteId","level","FK_ParentId")]
parentSites <- unique(parentSites)
parentSites <- parentSites[-which(parentSites$level==0),]
parentSites <- merge(parentSites,sites_id[,c("id","reference","weeklyTimelinessMinutes")], by.x = "FK_SiteId", by.y="id")
colnames(parentSites)[which(colnames(parentSites)=="FK_SiteId")] <- "Id_Site"


###############
# Dataframes with sites active for specific periods
###############


# Dataframe with the sites active for the whole period of interest

## FK_DimDateFromID: the site is expected to start reporting the week after for the reports of the week including FK_DimDateFromID
## FK_DimDateToID: the site is expected to report for the last time the week that includes (FK_DimDateToID - 1 day) for the reports of the week before the one including (FK_DimDateToID - 1 day)

temp <- NA # to compute the list of sites active for the whole period of interest since week 1
temp <- which(sites_relationships$FK_DimDateFromId <= dateStart_YR + 6 & (is.na(sites_relationships$FK_DimDateToId) | sites_relationships$FK_DimDateToId -1 >= dateEnd +7))

sites_wholePeriod_YR <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]

temp <- NA # to compute the list of sites active for the 12 previous weeks
temp <- which(sites_relationships$FK_DimDateFromId <= dateStart_W12 +6 & (is.na(sites_relationships$FK_DimDateToId) | sites_relationships$FK_DimDateToId -1 >= dateEnd +7))

sites_wholePeriod_W12 <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]

temp <- NA # to compute the list of sites active for the 8 previous weeks
temp <- which(sites_relationships$FK_DimDateFromId <= dateStart_W8 + 6 & (is.na(sites_relationships$FK_DimDateToId) | sites_relationships$FK_DimDateToId -1 >= dateEnd +7))

sites_wholePeriod_W8 <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]

temp <- NA # to compute the list of sites active for the 3 previous weeks
temp <- which(sites_relationships$FK_DimDateFromId <= dateStart_W3 +6 & (is.na(sites_relationships$FK_DimDateToId) | sites_relationships$FK_DimDateToId -1 >= dateEnd +7))

sites_wholePeriod_W3 <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]


# Dataframe with the sites active only for specific weeks in the period of interest

## Since week 1
temp <- NA # to compute the sites active only for specific weeks since week 1
temp <- which((sites_relationships$FK_DimDateFromId > dateStart_YR +6 & sites_relationships$FK_DimDateFromId <= dateEnd +7) | (sites_relationships$FK_DimDateToId -1 < dateEnd +7 & sites_relationships$FK_DimDateToId -1 >= dateStart_YR +7))

sites_specificPeriod_YR <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]

sites_specificPeriod_YR$weekStart <- ifelse(sites_specificPeriod_YR$FK_DimDateFromId > dateStart_YR +6, weekFunction(sites_specificPeriod_YR$FK_DimDateFromId), 1)

sites_specificPeriod_YR$weekEnd <- ifelse((sites_specificPeriod_YR$FK_DimDateToId -1 < dateEnd +7) & !is.na(sites_specificPeriod_YR$FK_DimDateToId), weekFunction(sites_specificPeriod_YR$FK_DimDateToId-8), weekEnd)

sites_specificPeriod_YR$duration <- NA # nb of weeks for which site active
sites_specificPeriod_YR$duration <- sites_specificPeriod_YR$weekEnd - sites_specificPeriod_YR$weekStart +1

## 12 previous weeks
temp <- NA # to compute the list of sites active for the 12 previous weeks
temp <- which((sites_relationships$FK_DimDateFromId > dateStart_W12 +6 & sites_relationships$FK_DimDateFromId <= dateEnd +7) | (sites_relationships$FK_DimDateToId -1 < dateEnd +7 & sites_relationships$FK_DimDateToId -1 >= dateStart_W12 +7))

sites_specificPeriod_W12 <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]

sites_specificPeriod_W12$weekStart <- ifelse(sites_specificPeriod_W12$FK_DimDateFromId > dateStart_W12 +6, weekFunction(sites_specificPeriod_W12$FK_DimDateFromId), weekStart_W12)

sites_specificPeriod_W12$weekEnd <- ifelse((sites_specificPeriod_W12$FK_DimDateToId -1 < dateEnd +7) & !is.na(sites_specificPeriod_W12$FK_DimDateToId), weekFunction(sites_specificPeriod_W12$FK_DimDateToId-8), weekEnd)

sites_specificPeriod_W12$duration <- NA # nb of weeks for which site active
sites_specificPeriod_W12$duration <- ifelse(sites_specificPeriod_W12$weekEnd >= sites_specificPeriod_W12$weekStart, sites_specificPeriod_W12$weekEnd - sites_specificPeriod_W12$weekStart +1, nbWeeksYear - sites_specificPeriod_W12$weekStart +1 + sites_specificPeriod_W12$weekEnd)

#############
## reportingValues_YR: table with variables of interest since the beginning of the year (week 1) for each site from the first intermediate level:
#############

reportingValues_YR <- parentSites # Since week 1

reportingValues_YR$nbExpected <- NA # Nb of expected reports
reportingValues_YR$nbReceived <- NA # Nb of reports received
reportingValues_YR$ids_fullreport_recVal <- # ids of full reports received and validated
  reportingValues_YR$nbReceivedValidated <- NA # Nb of reports received and validated (taken into account in the analyses)

Id_Site <- NA

for (Id_Site in reportingValues_YR$Id_Site[which(reportingValues_YR$level==levFirstInter)]) {
  
  # Nb of expected reports
  
  expectedSites_whole_YR <- NA # relationships IDs of reporting sites for the whole period
  expectedSites_whole_YR <- sites_wholePeriod_YR$id[which(sites_wholePeriod_YR$FK_ParentId==Id_Site)]
  
  expectedReports_specific_YR <- NA # expected number of reports from sites reporting only during a specific period (sum of weekly reporting duration for each site)
  expectedReports_specific_YR <- sum(sites_specificPeriod_YR$duration[which(sites_specificPeriod_YR$FK_ParentId==Id_Site)])
  
  reportingValues_YR$nbExpected[which(reportingValues_YR$Id_Site==Id_Site)] <- length(expectedSites_whole_YR)*length(numSem_YR) + expectedReports_specific_YR
  
  # Nb of received reports
  
  receivedReports_whole_YR <- NA # number of reports received from sites active the whole period
  receivedReports_whole_YR <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_YR & fullreport$weekNumber %in% numSem_YR)])
  
  receivedReports_specific_YR <- NA #  number of reports from sites reporting only during a specific period, vector with one value per reporting site for a specific period
  
  list_Id_SpecificSite <- NA
  list_Id_SpecificSite <- sites_specificPeriod_YR$id[which(sites_specificPeriod_YR$FK_ParentId==Id_Site)]
  
  if(length(list_Id_SpecificSite)==0) {
    receivedReports_specific_YR <- 0
    
  }else{
    
    Id_SpecificSite <- NA
    for (Id_SpecificSite in list_Id_SpecificSite) { # in case there is reporting sites for a specific period
      counter <- 1
      numSemTemp <- NA
      rowLineId <- NA
      rowLineId <- which(sites_specificPeriod_YR$id==Id_SpecificSite)
      
      if(sites_specificPeriod_YR$weekEnd[rowLineId] >= sites_specificPeriod_YR$weekStart[rowLineId]){
        numSemTemp <- seq(from=sites_specificPeriod_YR$weekStart[rowLineId], to=sites_specificPeriod_YR$weekEnd[rowLineId], by=1)
      } else {
        numSemTemp <- c(seq(from=sites_specificPeriod_YR$weekStart[rowLineId], to=nbWeeksYear, by=1), seq(from=1, to=sites_specificPeriod_YR$weekEnd[rowLineId], by=1))
      }
      
      receivedReports_specific_YR[counter] <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId==Id_SpecificSite & fullreport$weekNumber %in% numSemTemp)])
      counter <- counter+1
    }
  }
  
  reportingValues_YR$nbReceived[which(reportingValues_YR$Id_Site==Id_Site)] <- receivedReports_whole_YR + sum(receivedReports_specific_YR)
  
  
  # Nb of reports received and validated (taken into account in the analyses)
  
  recValReports_fullId_whole_YR <- NULL # fullreport ID of reports received and validated at least once from sites active the whole period
  recValReports_fullId_whole_YR <- fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_YR & fullreport$weekNumber %in% numSem_YR & !is.na(fullreport$firstValidationDate))]
  
  recValReports_fullId_specific_YR <- NA #  fullreport ID of reports received and validated at least once from sites reporting only during a specific period, vector with one value per reporting site for a specific period
  
  if(length(list_Id_SpecificSite)==0) {
    recValReports_fullId_specific_YR <- NULL
    
  }else{
    
    Id_SpecificSite <- NA
    counter <- 1
    
    for (Id_SpecificSite in list_Id_SpecificSite) { # in case there is reporting sites for a specific period
      numSemTemp <- NA
      rowLineId <- NA
      rowLineId <- which(sites_specificPeriod_YR$id==Id_SpecificSite)
      
      if(sites_specificPeriod_YR$weekEnd[rowLineId] >= sites_specificPeriod_YR$weekStart[rowLineId]){
        numSemTemp <- seq(from=sites_specificPeriod_YR$weekStart[rowLineId], to=sites_specificPeriod_YR$weekEnd[rowLineId], by=1)
      } else {
        numSemTemp <- c(seq(from=sites_specificPeriod_YR$weekStart[rowLineId], to=nbWeeksYear, by=1), seq(from=1, to=sites_specificPeriod_YR$weekEnd[rowLineId], by=1))
      }
      
      recValReports_fullId_specific_YR[counter] <- paste0(fullreport$id[which(fullreport$FK_SiteRelationShipId==Id_SpecificSite & fullreport$weekNumber %in% numSemTemp & !is.na(fullreport$firstValidationDate))],collapse=",")
      
      counter <- counter+1
    }
  }
  
  reportingValues_YR$nbReceivedValidated[which(reportingValues_YR$Id_Site==Id_Site)] <- length(recValReports_fullId_whole_YR) + length(strsplit(paste0(recValReports_fullId_specific_YR,collapse = ","),split=",")[[1]])
  
  # Ids of fullreports received and validated since week 1(taken into account in the analyses)
  
  reportingValues_YR$ids_fullreport_recVal[which(reportingValues_YR$Id_Site==Id_Site)] <- paste0(recValReports_fullId_whole_YR,paste(recValReports_fullId_specific_YR,collapse=","),collapse = ",")
  
}


levInterest <- NA

for (levInterest in seq(levFirstInter-1, 1, by=-1)) { # completion of the upper levels (sum of the below child nodes)
  
  Id_Site <- NA
  
  for (Id_Site in reportingValues_YR$Id_Site[which(reportingValues_YR$level==levInterest)]) {
    
    reportingValues_YR$nbExpected[which(reportingValues_YR$Id_Site==Id_Site)] <-  sum(reportingValues_YR$nbExpected[which(reportingValues_YR$FK_ParentId==Id_Site)])
    
    reportingValues_YR$nbReceived[which(reportingValues_YR$Id_Site==Id_Site)] <-  sum(reportingValues_YR$nbReceived[which(reportingValues_YR$FK_ParentId==Id_Site)])
    
    reportingValues_YR$nbReceivedValidated[which(reportingValues_YR$Id_Site==Id_Site)] <-  sum(reportingValues_YR$nbReceivedValidated[which(reportingValues_YR$FK_ParentId==Id_Site)])
    
    reportingValues_YR$ids_fullreport_recVal[which(reportingValues_YR$Id_Site==Id_Site)] <- paste0(reportingValues_YR$ids_fullreport_recVal[which(reportingValues_YR$FK_ParentId==Id_Site)],collapse=",")
  }
}


#############
## reportingValues_W12: table with variables of interest for each of the 12 previous weeks for each site from the first intermediate level:
#############

reportingValues_W12<- parentSites # For each week of the 12th previous week
reportingValues_W12$week <- NA

numSem <- NA

for (numSem in numSem_W12) {
  temp <- NA
  temp <- parentSites
  temp$week <- NA
  temp$week <- numSem
  reportingValues_W12 <- rbind(reportingValues_W12,temp)
}

reportingValues_W12 <- reportingValues_W12[-which(is.na(reportingValues_W12$week)),]

reportingValues_W12$nbExpected <- NA # Nb of expected reports
reportingValues_W12$nbReceived <- NA # Nb of received reports from leaf sites
reportingValues_W12$nbReceivedBelow <- NA # Nb of received reports from below level
reportingValues_W12$nbReceivedValidated <- NA # Nb of reports received and validated at least once
reportingValues_W12$nbRecTime <- NA # Nb of received reports on time from leaf sites
reportingValues_W12$nbRecTimeBelow <- NA # Nb of received reports on time from below level
reportingValues_W12$nbReviewed <- NA # Nb of reviewed reports
reportingValues_W12$nbRevTime <- NA # Nb of reviewed reports on time
reportingValues_W12$compReport <- NA # Completeness of data reporting for the period
reportingValues_W12$timeReport <- NA # Timeliness of data reporting for the period
reportingValues_W12$compReview <- NA # Completeness of data review for the period
reportingValues_W12$timeReview <- NA # Timeliness of data review for the period
reportingValues_W12$ids_fullreport_recVal <- NA # Full report IDs of reports received and validated at least once

Id_Site <- NA

for (Id_Site in parentSites$Id_Site[which(parentSites$level==levFirstInter)]) {
  
  numSem <- NA
  
  expectedSites_whole_W12 <- NA # relationships IDs of reporting sites for the whole period
  expectedSites_whole_W12 <- sites_wholePeriod_W12$id[which(sites_wholePeriod_W12$FK_ParentId==Id_Site)]
  
  list_Id_SpecificSite <- NA # relationships IDs of reporting sites for specifc periods during the 12 previous weeks
  list_Id_SpecificSite <- sites_specificPeriod_W12$id[which(sites_specificPeriod_W12$FK_ParentId==Id_Site)]
  
  for (numSem in numSem_W12) {
    
    list_Id_SpecificSite_week <- NULL # id list of specific sites for the week of interest
    
    if(length(list_Id_SpecificSite)==0) {
      
    }else{
      
      Id_SpecificSite <- NA
      counter <- 1
      
      for (Id_SpecificSite in list_Id_SpecificSite) { # in case there is reporting sites for a specific period
        numSemTemp <- NA
        rowLineId <- NA
        rowLineId <- which(sites_specificPeriod_W12$id==Id_SpecificSite)
        
        if(sites_specificPeriod_W12$weekEnd[rowLineId] >= sites_specificPeriod_W12$weekStart[rowLineId]){
          numSemTemp <- seq(from=sites_specificPeriod_W12$weekStart[rowLineId], to=sites_specificPeriod_W12$weekEnd[rowLineId], by=1)
        } else {
          numSemTemp <- c(seq(from=sites_specificPeriod_W12$weekStart[rowLineId], to=nbWeeksYear, by=1), seq(from=1, to=sites_specificPeriod_W12$weekEnd[rowLineId], by=1))
        }
        
        list_Id_SpecificSite_week[counter] <- ifelse(numSem %in% numSemTemp, Id_SpecificSite, NA) 
        
        counter <- counter+1
      }
      
      list_Id_SpecificSite_week <- list_Id_SpecificSite_week[which(!is.na(list_Id_SpecificSite_week))]
    }  
    
    
    # Nb of expected reports
    
    expectedReports_specific_W12 <- NULL # expected number of reports from sites reporting only during a specific period
    
    expectedReports_specific_W12 <- length(list_Id_SpecificSite_week)
    
    reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(length(expectedSites_whole_W12),expectedReports_specific_W12)
    
    
    # Nb of received reports
    
    receivedReports_whole_W12 <- NULL # number of reports received from sites active the whole period
    receivedReports_whole_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_W12 & fullreport$weekNumber==numSem)])
    
    receivedReports_specific_W12 <- NULL #  number of reports from sites reporting only during a specific period
    
    if(length(list_Id_SpecificSite_week)==0) {
      receivedReports_specific_W12 <- NULL
      
    }else{
      
      receivedReports_specific_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% list_Id_SpecificSite_week & fullreport$weekNumber == numSem)])
    }
    
    reportingValues_W12$nbReceived[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(receivedReports_whole_W12,receivedReports_specific_W12)
    
    
    # Nb of reports received and validated (taken into account in the analyses)
    
    recValReports_fullId_whole_W12 <- NULL # fullreport ID of reports received and validated at least once from sites active the whole period
    recValReports_fullId_whole_W12 <- fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_W12 & fullreport$weekNumber==numSem & !is.na(fullreport$firstValidationDate))]
    
    recValReports_fullId_specific_W12 <- NULL #  fullreport ID of reports received and validated at least once from sites reporting only during a specific period
    
    if(length(list_Id_SpecificSite_week)==0) {
      recValReports_fullId_specific_W12 <- NULL
      
    }else{
      
      recValReports_fullId_specific_W12 <- fullreport$id[which(fullreport$FK_SiteRelationShipId %in% list_Id_SpecificSite_week & fullreport$weekNumber == numSem & !is.na(fullreport$firstValidationDate))]
    }
    
    reportingValues_W12$nbReceivedValidated[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(length(recValReports_fullId_whole_W12),length(recValReports_fullId_specific_W12))
    
    
    # Ids of fullreports received and validated since for the 12 previous weeks (taken into account in the analyses)
    
    reportingValues_W12$ids_fullreport_recVal[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- paste0(recValReports_fullId_whole_W12,recValReports_fullId_specific_W12,collapse = ",")
    
    
    # Nb of received reports on time from leaf sites
    
    recTimeReports_whole_W12 <- NULL # number of reports received on time for sites active the whole period
    recTimeReports_whole_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_W12 & fullreport$weekNumber==numSem & fullreport$createdDate < (fullreport$startDate + days(7) + minutes(deadlinePeriph)))])
    
    recTimeReports_specific_W12 <- NULL #  number of reports received on time from sites reporting only during a specific period
    
    if(length(list_Id_SpecificSite_week)==0) {
      recTimeReports_specific_W12 <- NULL
      
    }else{
      
      recTimeReports_specific_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% list_Id_SpecificSite_week & fullreport$weekNumber == numSem & fullreport$createdDate < (fullreport$startDate + days(7) + minutes(deadlinePeriph)))])
    }
    
    reportingValues_W12$nbRecTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(recTimeReports_whole_W12,recTimeReports_specific_W12)
    
    # Nb of reports reviewed
    
    reviewedReports_whole_W12 <- NULL # number of reports reviewed for sites active the whole period
    reviewedReports_whole_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_W12 & fullreport$weekNumber==numSem & fullreport$status!="PENDING")])
    
    reviewedReports_specific_W12 <- NULL #  number of reports reviewed from sites reporting only during a specific period
    
    if(length(list_Id_SpecificSite_week)==0) {
      reviewedReports_specific_W12 <- NULL
      
    }else{
      
      reviewedReports_specific_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% list_Id_SpecificSite_week & fullreport$weekNumber == numSem & fullreport$status!="PENDING")])
    }
    
    reportingValues_W12$nbReviewed[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(reviewedReports_whole_W12,reviewedReports_specific_W12)
    
    
    # Nb of reviewed reports on time
    
    revTimeReports_whole_W12 <- NULL # number of reports received on time and reviewed on time for sites active the whole period
    revTimeReports_whole_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_W12 & fullreport$weekNumber==numSem & fullreport$createdDate < (fullreport$startDate + days(7) + minutes(deadlinePeriph)) & ( (!is.na(fullreport$firstValidationDate) & fullreport$firstValidationDate < (fullreport$startDate + days(7) + minutes(parentSites$weeklyTimelinessMinutes[which(parentSites$Id_Site==Id_Site)]))) | (!is.na(fullreport$firstRejectionDate) & fullreport$firstRejectionDate < (fullreport$startDate + days(7) + minutes(parentSites$weeklyTimelinessMinutes[which(parentSites$Id_Site==Id_Site)])))) )])
    
    revTimeReports_specific_W12 <- NULL #  number of reports received on time and reviewed on time from sites reporting only during a specific period
    
    if(length(list_Id_SpecificSite_week)==0) {
      revTimeReports_specific_W12 <- NULL
      
    }else{
      
      revTimeReports_specific_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% list_Id_SpecificSite_week & fullreport$weekNumber == numSem & fullreport$createdDate < (fullreport$startDate + days(7) + minutes(deadlinePeriph)) & ( (!is.na(fullreport$firstValidationDate) & fullreport$firstValidationDate < (fullreport$startDate + days(7) + minutes(parentSites$weeklyTimelinessMinutes[which(parentSites$Id_Site==Id_Site)]))) | (!is.na(fullreport$firstRejectionDate) & fullreport$firstRejectionDate < (fullreport$startDate + days(7) + minutes(parentSites$weeklyTimelinessMinutes[which(parentSites$Id_Site==Id_Site)])))) )])
    }
    
    reportingValues_W12$nbRevTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(revTimeReports_whole_W12,revTimeReports_specific_W12)
    
    # Completeness of data reporting
    
    reportingValues_W12$compReport[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- reportingValues_W12$nbReceived[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)]
    
    
    # Timeliness of data reporting
    
    reportingValues_W12$timeReport[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- reportingValues_W12$nbRecTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)]
    
    
    # Completeness of data review for the period
    
    reportingValues_W12$compReview[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- reportingValues_W12$nbReviewed[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbReceived[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)]
    
    
    # Timeliness of data review for the period
    
    reportingValues_W12$timeReview[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- reportingValues_W12$nbRevTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbRecTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)]
    
  }  
  
}

# For first intermediate level, nb of reports received and received on time at below level is same as nb of reports received from leaf sites


reportingValues_W12$nbReceivedBelow <- reportingValues_W12$nbReceived
reportingValues_W12$nbRecTimeBelow <- reportingValues_W12$nbRecTime


# Completion of the upper levels

levInterest <- NA

for (levInterest in seq(levFirstInter-1, 1, by=-1)) { # completion of the upper levels one by one
  
  Id_Site <- NA
  
  deadlineBelow <- NA
  deadlineBelow <- unique(parentSites$weeklyTimelinessMinutes[parentSites$level==levInterest+1])[1]
  
  deadlineLev <- NA
  deadlineLev <- unique(parentSites$weeklyTimelinessMinutes[parentSites$level==levInterest])[1]
  
  for (Id_Site in parentSites$Id_Site[which(parentSites$level==levInterest)]) {
    
    numSem <- NA
    
    expectedSites_whole_W12 <- NA # relationships IDs of reporting sites for the whole period
    expectedSites_whole_W12 <- sites_wholePeriod_W12$id[which(sites_wholePeriod_W12$FK_ParentId==Id_Site)]
    
    list_Id_SpecificSite <- NA # relationships IDs of reporting sites for specifc periods during the 12 previous weeks
    list_Id_SpecificSite <- sites_specificPeriod_W12$id[which(sites_specificPeriod_W12$FK_ParentId==Id_Site)]
    
    for (numSem in numSem_W12) {
      
      list_Id_SpecificSite_week <- NULL # id list of specific sites for the week of interest
      
      if(length(list_Id_SpecificSite)==0) {
      }else{
        
        Id_SpecificSite <- NA
        counter <- 1
        
        for (Id_SpecificSite in list_Id_SpecificSite) { # in case there is reporting sites for a specific period
          numSemTemp <- NA
          rowLineId <- NA
          rowLineId <- which(sites_specificPeriod_W12$id==Id_SpecificSite)
          
          if(sites_specificPeriod_W12$weekEnd[rowLineId] >= sites_specificPeriod_W12$weekStart[rowLineId]){
            numSemTemp <- seq(from=sites_specificPeriod_W12$weekStart[rowLineId], to=sites_specificPeriod_W12$weekEnd[rowLineId], by=1)
          } else {
            numSemTemp <- c(seq(from=sites_specificPeriod_W12$weekStart[rowLineId], to=nbWeeksYear, by=1), seq(from=1, to=sites_specificPeriod_W12$weekEnd[rowLineId], by=1))
          }
          
          list_Id_SpecificSite_week[counter] <- ifelse(numSem %in% numSemTemp, Id_SpecificSite, NA) 
          
          counter <- counter+1
        }
        
        list_Id_SpecificSite_week <- list_Id_SpecificSite_week[which(!is.na(list_Id_SpecificSite_week))]
      }  
      
      # nbExpected
      reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <-  sum(reportingValues_W12$nbExpected[which(reportingValues_W12$FK_ParentId==Id_Site & reportingValues_W12$week==numSem)])
      
      #NbReceived: number of reports received from leaf sites
      reportingValues_W12$nbReceived[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <-  sum(reportingValues_W12$nbReceived[which(reportingValues_W12$FK_ParentId==Id_Site & reportingValues_W12$week==numSem)])
      
      
      #nbReceivedBelow : number of reports received from below sites
      
      receivedBelow_whole_W12 <- NULL # number of reports received from sites active the whole period
      receivedBelow_whole_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_W12 & fullreport$weekNumber==numSem)])
      
      receivedBelow_specific_W12 <- NULL #  number of reports from sites reporting only during a specific period
      
      if(length(list_Id_SpecificSite_week)==0) {
        receivedBelow_specific_W12 <- NULL
        
      }else{
        
        receivedBelow_specific_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% list_Id_SpecificSite_week & fullreport$weekNumber == numSem)])
      }
      
      reportingValues_W12$nbReceivedBelow[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(receivedBelow_whole_W12,receivedBelow_specific_W12)
      
      
      # nbReceivedValidated
      reportingValues_W12$nbReceivedValidated[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <-  sum(reportingValues_W12$nbReceivedValidated[which(reportingValues_W12$FK_ParentId==Id_Site & reportingValues_W12$week==numSem)])
      
      # ids_fullreport_recVal
      reportingValues_W12$ids_fullreport_recVal[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <-  paste0(reportingValues_W12$ids_fullreport_recVal[which(reportingValues_W12$FK_ParentId==Id_Site  & reportingValues_W12$week==numSem)],collapse=",")
      
      # nbRecTime: received on time from leaf sites
      
      reportingValues_W12$nbRecTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <-  sum(reportingValues_W12$nbRecTime[which(reportingValues_W12$FK_ParentId==Id_Site & reportingValues_W12$week==numSem)])
      
      # nbRecTimeBelow : Nb of received reports on time from below level
      
      recTimeBelowReports_whole_W12 <- NULL # number of reports received on time for sites active the whole period
      recTimeBelowReports_whole_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_W12 & fullreport$weekNumber==numSem & fullreport$createdDate < (fullreport$startDate + days(7) + minutes(deadlineBelow)))])
      
      recTimeBelowReports_specific_W12 <- NULL #  number of reports received on time from sites reporting only during a specific period
      
      if(length(list_Id_SpecificSite_week)==0) {
        recTimeBelowReports_specific_W12 <- NULL
        
      }else{
        
        recTimeBelowReports_specific_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% list_Id_SpecificSite_week & fullreport$weekNumber == numSem & fullreport$createdDate < (fullreport$startDate + days(7) + minutes(deadlineBelow)))])
      }
      
      reportingValues_W12$nbRecTimeBelow[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(recTimeBelowReports_whole_W12,recTimeBelowReports_specific_W12)
      
      # Nb of reports reviewed
      
      reviewedReports_whole_W12 <- NULL # number of reports reviewed for sites active the whole period
      reviewedReports_whole_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_W12 & fullreport$weekNumber==numSem & fullreport$status!="PENDING")])
      
      reviewedReports_specific_W12 <- NULL #  number of reports reviewed from sites reporting only during a specific period
      
      if(length(list_Id_SpecificSite_week)==0) {
        reviewedReports_specific_W12 <- NULL
        
      }else{
        
        reviewedReports_specific_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% list_Id_SpecificSite_week & fullreport$weekNumber == numSem & fullreport$status!="PENDING")])
      }
      
      reportingValues_W12$nbReviewed[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(reviewedReports_whole_W12,reviewedReports_specific_W12)
      
      
      # Nb of reviewed reports on time
      
      revTimeReports_whole_W12 <- NULL # number of reports received on time and reviewed on time for sites active the whole period
      revTimeReports_whole_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_W12 & fullreport$weekNumber==numSem & fullreport$createdDate < (fullreport$startDate + days(7) + minutes(deadlineBelow))  & ( (!is.na(fullreport$firstValidationDate) & fullreport$firstValidationDate < (fullreport$startDate + days(7) + minutes(deadlineLev))) | (!is.na(fullreport$firstRejectionDate) & fullreport$firstRejectionDate < (fullreport$startDate + days(7) + minutes(deadlineLev)))) )])
      
      revTimeReports_specific_W12 <- NULL #  number of reports received on time and reviewed on time from sites reporting only during a specific period
      
      if(length(list_Id_SpecificSite_week)==0) {
        revTimeReports_specific_W12 <- NULL
        
      }else{
        
        revTimeReports_specific_W12 <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% list_Id_SpecificSite_week & fullreport$weekNumber == numSem & fullreport$createdDate < (fullreport$startDate + days(7) + minutes(deadlineBelow))  & ( (!is.na(fullreport$firstValidationDate) & fullreport$firstValidationDate < (fullreport$startDate + days(7) + minutes(deadlineLev))) | (!is.na(fullreport$firstRejectionDate) & fullreport$firstRejectionDate < (fullreport$startDate + days(7) + minutes(deadlineLev)))) )])
      }
      
      reportingValues_W12$nbRevTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- sum(revTimeReports_whole_W12,revTimeReports_specific_W12)
      
      # Completeness of data reporting
      
      reportingValues_W12$compReport[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- reportingValues_W12$nbReceived[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)]
      
      
      # Timeliness of data reporting
      
      reportingValues_W12$timeReport[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- reportingValues_W12$nbRecTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)]
      
      
      # Completeness of data review for the period
      
      reportingValues_W12$compReview[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- reportingValues_W12$nbReviewed[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbReceivedBelow[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)]
      
      
      # Timeliness of data review for the period
      
      reportingValues_W12$timeReview[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- reportingValues_W12$nbRevTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbRecTimeBelow[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] 
      
    } 
  }
}

#############
## reportingValues_W12_overall : same variables as reportingValues_W12 with aggregated data for the whole period for each site.
#############

reportingValues_W12_overall<- colSums(reportingValues_W12[,6:14]) # For each week of the 12th previous week

reportingValues_W12_overall <- parentSites
reportingValues_W12_overall$nbExpected <- NA # Nb of expected reports
reportingValues_W12_overall$nbReceived <- NA # Nb of received reports from leaf sites
reportingValues_W12_overall$nbReceivedBelow <- NA # Nb of received reports from below level
reportingValues_W12_overall$nbReceivedValidated <- NA # Nb of reports received and validated at least once
reportingValues_W12_overall$nbRecTime <- NA # Nb of received reports on time from leaf sites
reportingValues_W12_overall$nbRecTimeBelow <- NA # Nb of received reports on time from below level
reportingValues_W12_overall$nbReviewed <- NA # Nb of reviewed reports
reportingValues_W12_overall$nbRevTime <- NA # Nb of reviewed reports on time
reportingValues_W12_overall$compReport <- NA # Completeness of data reporting for the period
reportingValues_W12_overall$timeReport <- NA # Timeliness of data reporting for the period
reportingValues_W12_overall$compReview <- NA # Completeness of data review for the period
reportingValues_W12_overall$timeReview <- NA # Timeliness of data review for the period

IdSite <- NA
colnamesSum <- c("nbExpected","nbReceived","nbReceivedBelow","nbReceivedValidated","nbRecTime","nbRecTimeBelow","nbReviewed","nbRevTime")


for (IdSite in reportingValues_W12_overall$Id_Site) {
  reportingValues_W12_overall[reportingValues_W12_overall$Id_Site==IdSite,colnamesSum] <- colSums(reportingValues_W12[reportingValues_W12$Id_Site==IdSite,colnamesSum])
}

reportingValues_W12_overall$compReport <- reportingValues_W12_overall$nbReceived/reportingValues_W12_overall$nbExpected
reportingValues_W12_overall$timeReport <- reportingValues_W12_overall$nbRecTime/reportingValues_W12_overall$nbExpected
reportingValues_W12_overall$compReview <- reportingValues_W12_overall$nbReviewed/reportingValues_W12_overall$nbReceivedBelow
reportingValues_W12_overall$timeReview <- reportingValues_W12_overall$nbRevTime/reportingValues_W12_overall$nbRecTimeBelow


#############
## 


# Table with list of leaf sites without any report received for >= 3 weeks and >= 8 weeks grouped by first intermediate level (name, path, contact of the site)

# For the 8 previous weeks

tempFirstIntermediate <- parentSites[-which(parentSites$level<max(parentSites$level)),c("Id_Site","reference")]
colnames(tempFirstIntermediate) <- c("Id_parentSite","name_parentSite")

noReport_W8 <- data.frame(Id_parentSite=NA, name_parentSite=NA, siteName=NA, contact=NA, phone=NA)
# noReport_W8$Id_parentSite # site id of the parent site (first intermediate level)
# noReport_W8$name_parentSite # name of the parent site
# noReport_W8$siteName  # name of the site with no reporting
# noReport_W8$contact # name of the contact at the site with no reporting
# noReport_W8$phone # phone of the contact at the site with no reporting

IdParentSite <- NA
counter <- 1

for (IdParentSite in tempFirstIntermediate$Id_parentSite) {
  
  expectedSites_whole_W8 <- NA # relationships IDs of reporting sites for the whole period
  expectedSites_whole_W8 <- sites_wholePeriod_W8$id[which(sites_wholePeriod_W8$FK_ParentId==IdParentSite)]
  
  IdSite <- NA
  
  for (IdSite in expectedSites_whole_W8) {
    
    if(any(fullreport$FK_SiteRelationShipId %in% IdSite & fullreport$weekNumber %in% numSem_W8)) {
    } else {
      noReport_W8$Id_parentSite[counter] <- IdParentSite
      noReport_W8$name_parentSite[counter] <- tempFirstIntermediate$name_parentSite[which(tempFirstIntermediate$Id_parentSite==IdParentSite)]
      
      site_ID <- NA
      site_ID <- sites_wholePeriod_W8$FK_SiteId[which(sites_wholePeriod_W8$id==IdSite)]
      
      noReport_W8$siteName[counter] <- sites_id$reference[which(sites_id$id==site_ID)]
      noReport_W8$contact[counter] <- paste(sites_contact$name[which(sites_contact$FK_SiteId==site_ID & (sites_contact$isDeleted==0 | is.na(sites_contact$isDeleted)))], collapse = ", ")
      noReport_W8$phone[counter] <- paste(sites_contact$phoneNumber[which(sites_contact$FK_SiteId==site_ID)], collapse = ", ")
      
      noReport_W8 <- rbind(noReport_W8,NA)
      counter <- counter +1  
    }
  }
}

noReport_W8 <- noReport_W8[-nrow(noReport_W8),]

# For the 3 previous weeks

tempFirstIntermediate <- parentSites[-which(parentSites$level<max(parentSites$level)),c("Id_Site","reference")]
colnames(tempFirstIntermediate) <- c("Id_parentSite","name_parentSite")

noReport_W3 <- data.frame(Id_parentSite=NA, name_parentSite=NA, siteName=NA, contact=NA, phone=NA)
# noReport_W3$Id_parentSite # site id of the parent site (first intermediate level)
# noReport_W3$name_parentSite # name of the parent site
# noReport_W3$siteName  # name of the site with no reporting
# noReport_W3$contact # name of the contact at the site with no reporting
# noReport_W3$phone # phone of the contact at the site with no reporting

IdParentSite <- NA
counter <- 1

for (IdParentSite in tempFirstIntermediate$Id_parentSite) {
  
  expectedSites_whole_W3 <- NA # relationships IDs of reporting sites for the whole period
  expectedSites_whole_W3 <- sites_wholePeriod_W3$id[which(sites_wholePeriod_W3$FK_ParentId==IdParentSite)]
  
  IdSite <- NA
  
  for (IdSite in expectedSites_whole_W3) {
    
    if(any(fullreport$FK_SiteRelationShipId %in% IdSite & fullreport$weekNumber %in% numSem_W3)) {
    } else {
      noReport_W3$Id_parentSite[counter] <- IdParentSite
      noReport_W3$name_parentSite[counter] <- tempFirstIntermediate$name_parentSite[which(tempFirstIntermediate$Id_parentSite==IdParentSite)]
      
      site_ID <- NA
      site_ID <- sites_wholePeriod_W3$FK_SiteId[which(sites_wholePeriod_W3$id==IdSite)]
      
      noReport_W3$siteName[counter] <- sites_id$reference[which(sites_id$id==site_ID)]
      noReport_W3$contact[counter] <- paste(sites_contact$name[which(sites_contact$FK_SiteId==site_ID & (sites_contact$isDeleted==0 | is.na(sites_contact$isDeleted)))], collapse = ", ")
      noReport_W3$phone[counter] <- paste(sites_contact$phoneNumber[which(sites_contact$FK_SiteId==site_ID)], collapse = ", ")
      
      noReport_W3 <- rbind(noReport_W3,NA)
      counter <- counter +1  
    }
  }
}

noReport_W3 <- noReport_W3[-nrow(noReport_W3),]

#### Make the list of diseases that have crossed thresholds in the 12th previous weeks and the previous week




#### Table with for each disease crossing threshold in the 12th previous week, the number of cases and death per week for the whole country



#### Table with one row for each site/disease crossing threshold in the previous week, the number of cases and death and longitude and latitude of the unit, path of the site and contact




#### List of alerts received in the 10 previous days, name of the site, path and contact




#### Cumulative table with the number of cases since beginning of year and the same period the year before for the whole country, one row per disease (all diseases), one column per year, first row: % and nb of reports received and validated out of the number of expected reports for the period.






### Elements to pass to the report
admin_report_input <- list(
  noReport_W3 = noReport_W3,
  noReport_W8 = noReport_W8,
  reportingValues_W12 = reportingValues_W12,
  reportingValues_W12_overall = reportingValues_W12_overall)

save(admin_report_input, file = "src/assets/admin_report_input.RData")
