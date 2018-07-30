#Summary ####
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
# Modification of the format of some variables
# Creation of tables of interest for the dashboards
# Preparatory tables
# Dataframes with sites active for specific periods
# development of each table presented in the results below

#### Results
# Data tables with all data needed to create the Argus dashboards
#
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
#noReport_W3: table with list of leaf sites without any report received for >= 3 weeks grouped by first intermediate level:
# .	noReport_W8$Id_parentSite: site id of the parent site (first intermediate level)
# .	noReport_W8$name_parentSite: name of the parent site
# .	noReport_W8$siteName:  name of the site with no reporting
# .	noReport_W8$contact: name of the contact at the site with no reporting
# .	noReport_W8$phone: phone of the contact at the site with no reporting
# 
# noReport_W8: table with list of leaf sites without any report received for >= 8 weeks grouped by first intermediate level:
# .	noReport_W8$Id_parentSite: site id of the parent site (first intermediate level)
# .	noReport_W8$name_parentSite: name of the parent site
# .	noReport_W8$siteName: name of the site with no reporting
# .	noReport_W8$contact: name of the contact at the site with no reporting
# .	noReport_W8$phone: phone of the contact at the site with no reporting
#
# diseaseThreshold_W12: table with the number of cases of each disease for which the threshold has been reached in any of the 12 previous weeks for the whole country:
# .	disease: reference of the disease
# .	diseaseName: name of the disease
# .	threshold_value: value of the threshold for the disease
# .	variable: variable of interest
# .	week: week
# .	occurrence: number of cases of the disease for the specific week for the whole country
# 
# diseaseThreshold_W2: table with one row for each disease having crossed threshold for the whole country during the 12 previous weeks, and for sites having reported cases in the two previous week, the number of cases for the two previous weeks and longitude and latitude of the site, and contact
# .	FK_SiteId: id number of the site
# .	FK_ParentId: id number of the parent site
# .	name: reference of the site
# .	longitude: longitude of the site
# .	latitude: latitude of the site
# .	name_parentSite: name of the parent site
# .	siteName: name of the site
# .	contact: name of the contact
# .	phone: phone number of the contact
# .	disease: disease
# .	variable: variable of interst
# .	threshold_value: threshold value of the disease
# .	occurence: number of cases for the disease at the site.
# 
# alertList_D10: list of alerts received in the 10 previous days
# .	receptionDate: reception date of the alert
# .	contactName: name of the contact
# .	contactPhoneNumber: phone number of the contact
# .	FK_SiteRelationShipId: ids of the site in the table site_relationship
# .	message: content of the alert
# .	name_parentSite: name of the parent site
# .	name_Site: name of the site.
# 
# tableBeginYear: cumulative numbers for each disease since the beginning of the year and the previous year for the same period.
# .	id: disease id
# .	disease: disease reference
# .	name: disease name
# .	XX.YEAR : nb of cases  for each variable XX of interest in the previous year and then in the current year (nb of columns= nb of unique variables of interest x 2).

#
##### Load packages ####

library(RMySQL)
library(RSQLite)
library(Hmisc)
library(lubridate)
library(ggplot2)
#library(purrr)
#library(grid)
#library(gtools)

 
#### Set the variables of interest ####

db_config <- "db/db_config"

get_config <- function(config) {
  config <- utils::read.table(config, sep = "=",
                              col.names = c("key", "value"),
                              as.is = c(1, 2),
                              comment.char = "")
  split(config$value, config$key)
}

config_list <- get_config(db_config)

#### Set the periods of interest ####

# function to correct week() based on the first day of the week, if Monday use isoweek(), if Sunday use epiweek()

weekFunction <- function(x) {
  if(weekFirstDay==1) {
    return(isoweek(x))
  } else {
    if(weekFirstDay==7) {
      return(epiweek(x))
    } else {
      return("Error first day week, modify the variable weekFirstDay in script constants.R")
    }
  }
}

# Years of interest

yearCurrent <- year(today())
yearPrevious <- yearCurrent -1

# Start date of interest: the first day of the period of interest: first day of week 1 or first day of the 12th previous week (calculation also of first day of the 8th and 3rd previous weeks)

dateStart_W12 <- floor_date(today()-weeks(12),unit="week", week_start = weekFirstDay)
dateStart_W8 <- floor_date(today()-weeks(8),unit="week", week_start = weekFirstDay)
dateStart_W3 <- floor_date(today()-weeks(3),unit="week", week_start = weekFirstDay)

if(weekFunction(floor_date(today(),unit = "year"))==1) {
  dateStart_YR <- floor_date(floor_date(today(),unit = "year"),unit="week",week_start = weekFirstDay)
} else {
  dateStart_YR <- floor_date(floor_date(today(),unit = "year")+7,unit="week",week_start = weekFirstDay)
} 

if(weekFunction(floor_date(today()-years(1),unit = "year"))==1) {
  dateStart_YR_previous <- floor_date(floor_date(today()-years(1),unit = "year"),unit="week",week_start = weekFirstDay)
} else {
  dateStart_YR_previous <- floor_date(floor_date(today()-years(1),unit = "year")+7,unit="week",week_start = weekFirstDay)
} 

dateStart <- min(dateStart_W12,dateStart_YR)

# End date of interest for the weekly reports: first day of the last week to be reported

dateEnd <- floor_date(today()-weeks(1),unit="week", week_start = weekFirstDay)

dateEnd_YR_previous <- floor_date(today()-weeks(1) - years(1),unit="week", week_start = weekFirstDay)

# Creation of a vector including the Weeks of interest

weekStart_W12 <- weekFunction(dateStart_W12)
weekStart_W8 <- weekFunction(dateStart_W8)
weekStart_W3 <- weekFunction(dateStart_W3)

weekEnd <- weekFunction(today()-weeks(1))

weekEnd_YR_previous <- weekFunction(today()-weeks(1) - years(1))

if(weekEnd>weekEnd_YR_previous) {
  dateEnd_YR_previous <- floor_date(today() - years(1),unit="week", week_start = weekFirstDay)
} else {
  if(weekEnd < weekEnd_YR_previous) {
    dateEnd_YR_previous <- floor_date(today()-weeks(2) - years(1),unit="week", week_start = weekFirstDay)
  } else {
  }
}

nbWeeksYear <- ifelse(weekFunction(floor_date(today(),unit = "year"))==1,weekFunction(floor_date(today(),unit = "year")-weeks(1)),weekFunction(floor_date(today(),unit = "year"))) # to know if 52 or 53 weeks in the previous year.

numSem_W12 <- NA # vector with the 12 previous weeks

if(weekEnd>weekStart_W12){
  numSem_W12 <- seq(from=weekStart_W12, to=weekEnd, by=1)
} else {
  numSem_W12 <- c(seq(from=weekStart_W12, to=nbWeeksYear, by=1), seq(from=1, to=weekEnd, by=1))
}

yearSem_W12 <- NA # vector with the year of the 12 previous weeks
yearSem_W12 <- ifelse(numSem_W12>numSem_W12[length(numSem_W12)], yearPrevious, yearCurrent)

numSem_W8 <- NA # vector with the 8 previous weeks

if(weekEnd>weekStart_W8){
  numSem_W8 <- seq(from=weekStart_W8, to=weekEnd, by=1)
} else {
  numSem_W8 <- c(seq(from=weekStart_W8, to=nbWeeksYear, by=1), seq(from=1, to=weekEnd, by=1))
}

yearSem_W8 <- ifelse(numSem_W8>numSem_W8[length(numSem_W8)], yearPrevious, yearCurrent)

numSem_W3 <- NA # vector with the 3 previous weeks

if(weekEnd>weekStart_W3){
  numSem_W3 <- seq(from=weekStart_W3, to=weekEnd, by=1)
} else {
  numSem_W3 <- c(seq(from=weekStart_W3, to=nbWeeksYear, by=1), seq(from=1, to=weekEnd, by=1))
}

yearSem_W3 <- ifelse(numSem_W3>numSem_W3[length(numSem_W3)], yearPrevious, yearCurrent)


numSem_YR <- seq(from=1, to=weekEnd, by=1) # vector with the weeks since first week 1



#### Retrieve raw data of interest from the MySQL database ####


baseSql <- dbConnect(MySQL(), user = config_list$DB_USER,
                     password = config_list$DB_PASSWORD,
                     host = config_list$DB_HOST,
                     dbname = config_list$DB_NAME)

sites_id <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_sites")

sites_contact <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_contacts")

sites_relationships <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_sites_relationship")

sites_dimdates <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_indicatordimdate WHERE fullDate >=", paste("'",dateStart_YR_previous," 00:00:00'", sep=""),"AND fullDate <=",paste("'",today()," 23:59:59'", sep=""),";", sep=" "))

diseases_name <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_diseases")

diseases_variable <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_diseasevalues")

diseases_threshold <- dbGetQuery(baseSql, "SELECT * FROM sesdashboard_thresholds")

fullreport_YR_previous <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_fullreport WHERE startDate >=", paste("'",dateStart_YR_previous," 00:00:00'", sep=""),"AND startDate <=",paste("'",today()," 23:59:59'", sep=""),";", sep=" "))

partreport_YR_previous <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_partreport WHERE FK_FullReportId IN (",paste(fullreport_YR_previous$id,collapse =","),");"))

report_YR_previous <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_report WHERE FK_PartReportId IN (",paste(partreport_YR_previous$id,collapse =","),");"))

report_values_YR_previous <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_reportvalues WHERE FK_ReportId IN (",paste(report_YR_previous$id,collapse =","),");"))

fullreport <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_fullreport WHERE startDate >=", paste("'",dateStart," 00:00:00'", sep=""),"AND startDate <=",paste("'",today()," 23:59:59'", sep=""),";", sep=" "))

partreport <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_partreport WHERE FK_FullReportId IN (",paste(fullreport$id,collapse =","),");"))

report <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_report WHERE FK_PartReportId IN (",paste(partreport$id,collapse =","),");"))

report_values <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_reportvalues WHERE FK_ReportId IN (",paste(report$id,collapse =","),");"))

alerts <- dbGetQuery(baseSql, paste("SELECT * FROM sesdashboard_alert WHERE receptionDate >=", paste("'",dateStart," 00:00:00'", sep=""),"AND receptionDate <=",paste("'",today()," 23:59:59'", sep=""),";", sep=" "))

dbDisconnect(baseSql)
rm(baseSql)


#### Modification of the format of some variables ####

# sites_relationships

sites_relationships$FK_DimDateFromId <- ymd(sites_relationships$FK_DimDateFromId) # put as date
sites_relationships$FK_DimDateToId <- ymd(sites_relationships$FK_DimDateToId) # put as date

# fullreport

fullreport_YR_previous$startDate <- ymd_hms(fullreport_YR_previous$startDate)
fullreport_YR_previous$createdDate <- ymd_hms(fullreport_YR_previous$createdDate)

fullreport$startDate <- ymd_hms(fullreport$startDate)
fullreport$createdDate <- ymd_hms(fullreport$createdDate)

# alerts
alerts$receptionDate <- ymd_hms(alerts$receptionDate)



#### Creation of tables of interest for the dashboards ####


## Preparatory tables ####


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

# Thresholds

thresholds <- unique(diseases_threshold[,c("maximalValue","FK_DiseaseId","FK_DiseaseValueId")])

colnames(thresholds)[which(colnames(thresholds)=="maximalValue")] <- "threshold_value"

rowLineId <- NA
for (rowLineId in 1:nrow(thresholds)) {
  thresholds$disease[rowLineId] <- diseases_name$disease[which(diseases_name$id==thresholds$FK_DiseaseId[rowLineId])]
  thresholds$diseaseName[rowLineId] <- diseases_name$name[which(diseases_name$id==thresholds$FK_DiseaseId[rowLineId])]
  thresholds$variable[rowLineId] <- diseases_variable$value[which(diseases_variable$id==thresholds$FK_DiseaseValueId[rowLineId])]
}

# longitude and latitude when values are not missing

longLat <- unique(sites_relationships[which(!is.na(sites_relationships$longitude) | !is.na(sites_relationships$latitude)),c("name","FK_SiteId","level","longitude","latitude")])


#### Dataframes with sites active for specific periods ####


# Dataframe with the sites active for the whole period of interest

## FK_DimDateFromID: the site is expected to start reporting the week after for the reports of the week including FK_DimDateFromID
## FK_DimDateToID: the site is expected to report for the last time the week that includes (FK_DimDateToID - 1 day) for the reports of the week before the one including (FK_DimDateToID - 1 day)

temp <- NA # to compute the list of sites active for the whole period of interest since week 1
temp <- which(sites_relationships$FK_DimDateFromId <= dateStart_YR + 6 & (is.na(sites_relationships$FK_DimDateToId) | sites_relationships$FK_DimDateToId -1 >= dateEnd +7))

sites_wholePeriod_YR <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]

temp <- NA # to compute the list of sites active for the whole period of interest since week 1 in the previous year
temp <- which(sites_relationships$FK_DimDateFromId <= dateStart_YR_previous + 6 & (is.na(sites_relationships$FK_DimDateToId) | sites_relationships$FK_DimDateToId -1 >= dateEnd_YR_previous +7))

sites_wholePeriod_YR_previous <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]

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

if (length(temp)==0) {
  sites_specificPeriod_YR <- NULL
} else {
  sites_specificPeriod_YR <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]
  
  sites_specificPeriod_YR$weekStart <- ifelse(sites_specificPeriod_YR$FK_DimDateFromId > dateStart_YR +6, weekFunction(sites_specificPeriod_YR$FK_DimDateFromId), 1)
  
  sites_specificPeriod_YR$weekEnd <- ifelse((sites_specificPeriod_YR$FK_DimDateToId -1 < dateEnd +7) & !is.na(sites_specificPeriod_YR$FK_DimDateToId), weekFunction(sites_specificPeriod_YR$FK_DimDateToId-8), weekEnd)
  
  sites_specificPeriod_YR$duration <- NA # nb of weeks for which site active
  sites_specificPeriod_YR$duration <- sites_specificPeriod_YR$weekEnd - sites_specificPeriod_YR$weekStart +1
}

## Since week 1 of the previous year
temp <- NA # to compute the sites active only for specific weeks since week 1
temp <- which((sites_relationships$FK_DimDateFromId > dateStart_YR_previous +6 & sites_relationships$FK_DimDateFromId <= dateEnd_YR_previous +7) | (sites_relationships$FK_DimDateToId -1 < dateEnd_YR_previous +7 & sites_relationships$FK_DimDateToId -1 >= dateStart_YR_previous +7))

if (length(temp)==0) {
  sites_specificPeriod_YR_previous <- NULL
} else {
  sites_specificPeriod_YR_previous <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]
  
  sites_specificPeriod_YR_previous$weekStart <- ifelse(sites_specificPeriod_YR_previous$FK_DimDateFromId > dateStart_YR_previous +6, weekFunction(sites_specificPeriod_YR_previous$FK_DimDateFromId), 1)
  
  sites_specificPeriod_YR_previous$weekEnd <- ifelse((sites_specificPeriod_YR_previous$FK_DimDateToId -1 < dateEnd +7) & !is.na(sites_specificPeriod_YR_previous$FK_DimDateToId), weekFunction(sites_specificPeriod_YR_previous$FK_DimDateToId-8), weekEnd)
  
  sites_specificPeriod_YR_previous$duration <- NA # nb of weeks for which site active
  sites_specificPeriod_YR_previous$duration <- sites_specificPeriod_YR_previous$weekEnd - sites_specificPeriod_YR_previous$weekStart +1
}


## 12 previous weeks
temp <- NA # to compute the list of sites active for the 12 previous weeks
temp <- which((sites_relationships$FK_DimDateFromId > dateStart_W12 +6 & sites_relationships$FK_DimDateFromId <= dateEnd +7) | (sites_relationships$FK_DimDateToId -1 < dateEnd +7 & sites_relationships$FK_DimDateToId -1 >= dateStart_W12 +7))

if (length(temp)==0) {
  sites_specificPeriod_W12 <- NULL
} else {
  sites_specificPeriod_W12 <- sites_relationships[temp, c("FK_SiteId","id","level","FK_ParentId","FK_DimDateFromId","FK_DimDateToId")]
  
  sites_specificPeriod_W12$weekStart <- ifelse(sites_specificPeriod_W12$FK_DimDateFromId > dateStart_W12 +6, weekFunction(sites_specificPeriod_W12$FK_DimDateFromId), weekStart_W12)
  
  sites_specificPeriod_W12$weekEnd <- ifelse((sites_specificPeriod_W12$FK_DimDateToId -1 < dateEnd +7) & !is.na(sites_specificPeriod_W12$FK_DimDateToId), weekFunction(sites_specificPeriod_W12$FK_DimDateToId-8), weekEnd)
  
  sites_specificPeriod_W12$duration <- NA # nb of weeks for which site active
  sites_specificPeriod_W12$duration <- ifelse(sites_specificPeriod_W12$weekEnd >= sites_specificPeriod_W12$weekStart, sites_specificPeriod_W12$weekEnd - sites_specificPeriod_W12$weekStart +1, nbWeeksYear - sites_specificPeriod_W12$weekStart +1 + sites_specificPeriod_W12$weekEnd)
}

## reportingValues_YR: table with variables of interest since the beginning of the year (week 1) for each site from the first intermediate level ####


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
  receivedReports_whole_YR <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_YR & fullreport$weekNumber %in% numSem_YR & fullreport$year==yearCurrent)])
  
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
      
      receivedReports_specific_YR[counter] <- length(fullreport$id[which(fullreport$FK_SiteRelationShipId==Id_SpecificSite & fullreport$weekNumber %in% numSemTemp & fullreport$year==yearCurrent)])
      counter <- counter+1
    }
  }
  
  reportingValues_YR$nbReceived[which(reportingValues_YR$Id_Site==Id_Site)] <- receivedReports_whole_YR + sum(receivedReports_specific_YR)
  
  
  # Nb of reports received and validated (taken into account in the analyses)
  
  recValReports_fullId_whole_YR <- NULL # fullreport ID of reports received and validated at least once from sites active the whole period
  recValReports_fullId_whole_YR <- fullreport$id[which(fullreport$FK_SiteRelationShipId %in% expectedSites_whole_YR & fullreport$weekNumber %in% numSem_YR & fullreport$year==yearCurrent & !is.na(fullreport$firstValidationDate))]
  
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
      
      recValReports_fullId_specific_YR[counter] <- paste0(fullreport$id[which(fullreport$FK_SiteRelationShipId==Id_SpecificSite & fullreport$weekNumber %in% numSemTemp & fullreport$year==yearCurrent & !is.na(fullreport$firstValidationDate))],collapse=",")
      
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

## reportingValues_YR_previous: table with variables of interest for the same period the previous year from the first intermediate level ####

reportingValues_YR_previous <- parentSites # Since week 1

reportingValues_YR_previous$nbExpected <- NA # Nb of expected reports
reportingValues_YR_previous$nbReceived <- NA # Nb of reports received
reportingValues_YR_previous$ids_fullreport_recVal <- # ids of full reports received and validated
  reportingValues_YR_previous$nbReceivedValidated <- NA # Nb of reports received and validated (taken into account in the analyses)

Id_Site <- NA

for (Id_Site in reportingValues_YR_previous$Id_Site[which(reportingValues_YR_previous$level==levFirstInter)]) {
  
  # Nb of expected reports
  
  expectedSites_whole_YR_previous <- NA # relationships IDs of reporting sites for the whole period
  expectedSites_whole_YR_previous <- sites_wholePeriod_YR_previous$id[which(sites_wholePeriod_YR_previous$FK_ParentId==Id_Site)]
  
  expectedReports_specific_YR_previous <- NA # expected number of reports from sites reporting only during a specific period (sum of weekly reporting duration for each site)
  expectedReports_specific_YR_previous <- sum(sites_specificPeriod_YR_previous$duration[which(sites_specificPeriod_YR_previous$FK_ParentId==Id_Site)])
  
  reportingValues_YR_previous$nbExpected[which(reportingValues_YR_previous$Id_Site==Id_Site)] <- length(expectedSites_whole_YR_previous)*length(numSem_YR) + expectedReports_specific_YR_previous
  
  # Nb of received reports
  
  receivedReports_whole_YR_previous <- NA # number of reports received from sites active the whole period
  receivedReports_whole_YR_previous <- length(fullreport_YR_previous$id[which(fullreport_YR_previous$FK_SiteRelationShipId %in% expectedSites_whole_YR_previous & fullreport_YR_previous$weekNumber %in% numSem_YR & fullreport_YR_previous$year==yearPrevious)])
  
  receivedReports_specific_YR_previous <- NA #  number of reports from sites reporting only during a specific period, vector with one value per reporting site for a specific period
  
  list_Id_SpecificSite <- NA
  list_Id_SpecificSite <- sites_specificPeriod_YR_previous$id[which(sites_specificPeriod_YR_previous$FK_ParentId==Id_Site)]
  
  if(length(list_Id_SpecificSite)==0) {
    receivedReports_specific_YR_previous <- 0
    
  }else{
    
    Id_SpecificSite <- NA
    for (Id_SpecificSite in list_Id_SpecificSite) { # in case there is reporting sites for a specific period
      counter <- 1
      numSemTemp <- NA
      rowLineId <- NA
      rowLineId <- which(sites_specificPeriod_YR_previous$id==Id_SpecificSite)
      
      if(sites_specificPeriod_YR_previous$weekEnd[rowLineId] >= sites_specificPeriod_YR_previous$weekStart[rowLineId]){
        numSemTemp <- seq(from=sites_specificPeriod_YR_previous$weekStart[rowLineId], to=sites_specificPeriod_YR_previous$weekEnd[rowLineId], by=1)
      } else {
        numSemTemp <- c(seq(from=sites_specificPeriod_YR_previous$weekStart[rowLineId], to=nbWeeksYear, by=1), seq(from=1, to=sites_specificPeriod_YR_previous$weekEnd[rowLineId], by=1))
      }
      
      receivedReports_specific_YR_previous[counter] <- length(fullreport_YR_previous$id[which(fullreport_YR_previous$FK_SiteRelationShipId==Id_SpecificSite & fullreport_YR_previous$weekNumber %in% numSemTemp & fullreport_YR_previous$year==yearPrevious)])
      counter <- counter+1
    }
  }
  
  reportingValues_YR_previous$nbReceived[which(reportingValues_YR_previous$Id_Site==Id_Site)] <- receivedReports_whole_YR_previous + sum(receivedReports_specific_YR_previous)
  
  
  # Nb of reports received and validated (taken into account in the analyses)
  
  recValReports_fullId_whole_YR_previous <- NULL # fullreport_YR_previous ID of reports received and validated at least once from sites active the whole period
  recValReports_fullId_whole_YR_previous <- fullreport_YR_previous$id[which(fullreport_YR_previous$FK_SiteRelationShipId %in% expectedSites_whole_YR_previous & fullreport_YR_previous$weekNumber %in% numSem_YR & fullreport_YR_previous$year==yearPrevious & !is.na(fullreport_YR_previous$firstValidationDate))]
  
  recValReports_fullId_specific_YR_previous <- NA #  fullreport_YR_previous ID of reports received and validated at least once from sites reporting only during a specific period, vector with one value per reporting site for a specific period
  
  if(length(list_Id_SpecificSite)==0) {
    recValReports_fullId_specific_YR_previous <- NULL
    
  }else{
    
    Id_SpecificSite <- NA
    counter <- 1
    
    for (Id_SpecificSite in list_Id_SpecificSite) { # in case there is reporting sites for a specific period
      numSemTemp <- NA
      rowLineId <- NA
      rowLineId <- which(sites_specificPeriod_YR_previous$id==Id_SpecificSite)
      
      if(sites_specificPeriod_YR_previous$weekEnd[rowLineId] >= sites_specificPeriod_YR_previous$weekStart[rowLineId]){
        numSemTemp <- seq(from=sites_specificPeriod_YR_previous$weekStart[rowLineId], to=sites_specificPeriod_YR_previous$weekEnd[rowLineId], by=1)
      } else {
        numSemTemp <- c(seq(from=sites_specificPeriod_YR_previous$weekStart[rowLineId], to=nbWeeksYear, by=1), seq(from=1, to=sites_specificPeriod_YR_previous$weekEnd[rowLineId], by=1))
      }
      
      recValReports_fullId_specific_YR_previous[counter] <- paste0(fullreport_YR_previous$id[which(fullreport_YR_previous$FK_SiteRelationShipId==Id_SpecificSite & fullreport_YR_previous$weekNumber %in% numSemTemp & fullreport_YR_previous$year==yearPrevious & !is.na(fullreport_YR_previous$firstValidationDate))],collapse=",")
      
      counter <- counter+1
    }
  }
  
  reportingValues_YR_previous$nbReceivedValidated[which(reportingValues_YR_previous$Id_Site==Id_Site)] <- length(recValReports_fullId_whole_YR_previous) + length(strsplit(paste0(recValReports_fullId_specific_YR_previous,collapse = ","),split=",")[[1]])
  
  # Ids of fullreports received and validated since week 1(taken into account in the analyses)
  
  reportingValues_YR_previous$ids_fullreport_recVal[which(reportingValues_YR_previous$Id_Site==Id_Site)] <- paste0(recValReports_fullId_whole_YR_previous,paste(recValReports_fullId_specific_YR_previous,collapse=","),collapse = ",")
  
}


levInterest <- NA

for (levInterest in seq(levFirstInter-1, 1, by=-1)) { # completion of the upper levels (sum of the below child nodes)
  
  Id_Site <- NA
  
  for (Id_Site in reportingValues_YR_previous$Id_Site[which(reportingValues_YR_previous$level==levInterest)]) {
    
    reportingValues_YR_previous$nbExpected[which(reportingValues_YR_previous$Id_Site==Id_Site)] <-  sum(reportingValues_YR_previous$nbExpected[which(reportingValues_YR_previous$FK_ParentId==Id_Site)])
    
    reportingValues_YR_previous$nbReceived[which(reportingValues_YR_previous$Id_Site==Id_Site)] <-  sum(reportingValues_YR_previous$nbReceived[which(reportingValues_YR_previous$FK_ParentId==Id_Site)])
    
    reportingValues_YR_previous$nbReceivedValidated[which(reportingValues_YR_previous$Id_Site==Id_Site)] <-  sum(reportingValues_YR_previous$nbReceivedValidated[which(reportingValues_YR_previous$FK_ParentId==Id_Site)])
    
    reportingValues_YR_previous$ids_fullreport_recVal[which(reportingValues_YR_previous$Id_Site==Id_Site)] <- paste0(reportingValues_YR_previous$ids_fullreport_recVal[which(reportingValues_YR_previous$FK_ParentId==Id_Site)],collapse=",")
  }
}


## reportingValues_W12: table with variables of interest for each of the 12 previous weeks for each site from the first intermediate level ####

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
    
    reportingValues_W12$compReport[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- round((reportingValues_W12$nbReceived[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)])*100,1)
    
    
    # Timeliness of data reporting
    
    reportingValues_W12$timeReport[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- round((reportingValues_W12$nbRecTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)])*100,1)
    
    
    # Completeness of data review for the period
    
    reportingValues_W12$compReview[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- round((reportingValues_W12$nbReviewed[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbReceived[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)])*100,1)
    
    
    # Timeliness of data review for the period
    
    reportingValues_W12$timeReview[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- round((reportingValues_W12$nbRevTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbRecTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)])*100,1)
    
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
      
      reportingValues_W12$compReport[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- round((reportingValues_W12$nbReceived[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)])*100,1)
      
      
      # Timeliness of data reporting
      
      reportingValues_W12$timeReport[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- round((reportingValues_W12$nbRecTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbExpected[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)])*100,1)
      
      
      # Completeness of data review for the period
      
      reportingValues_W12$compReview[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- round((reportingValues_W12$nbReviewed[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbReceivedBelow[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)])*100,1)
      
      
      # Timeliness of data review for the period
      
      reportingValues_W12$timeReview[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] <- round((reportingValues_W12$nbRevTime[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)] / reportingValues_W12$nbRecTimeBelow[which(reportingValues_W12$Id_Site==Id_Site & reportingValues_W12$week==numSem)])*100,1)
      
    } 
  }
}

## reportingValues_W12_overall : same variables as reportingValues_W12 with aggregated data for the whole period for each site ####

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
reportingValues_W12_overall$ids_fullreport_recVal <- NA # id of the fullreports that has been validated at least once

IdSite <- NA
colnamesSum <- c("nbExpected","nbReceived","nbReceivedBelow","nbReceivedValidated","nbRecTime","nbRecTimeBelow","nbReviewed","nbRevTime")

for (IdSite in reportingValues_W12_overall$Id_Site) {
  reportingValues_W12_overall[reportingValues_W12_overall$Id_Site==IdSite,colnamesSum] <- colSums(reportingValues_W12[which(reportingValues_W12$Id_Site==IdSite),colnamesSum])
  
  reportingValues_W12_overall$ids_fullreport_recVal[reportingValues_W12_overall$Id_Site==IdSite] <- paste0(reportingValues_W12$ids_fullreport_recVal[which(reportingValues_W12$Id_Site==IdSite)],collapse = ",")
}

reportingValues_W12_overall$compReport <- round((reportingValues_W12_overall$nbReceived/reportingValues_W12_overall$nbExpected)*100,1)
reportingValues_W12_overall$timeReport <- round((reportingValues_W12_overall$nbRecTime/reportingValues_W12_overall$nbExpected)*100,1)
reportingValues_W12_overall$compReview <- round((reportingValues_W12_overall$nbReviewed/reportingValues_W12_overall$nbReceivedBelow)*100,1)
reportingValues_W12_overall$timeReview <- round((reportingValues_W12_overall$nbRevTime/reportingValues_W12_overall$nbRecTimeBelow)*100,1)


## noReport_W8 and noReport_W3: table with list of leaf sites without any report received for >= 8 weeks  and >= 3 weeks grouped by first intermediate level ####

# Table with list of leaf sites without any report received for >= 3 weeks and >= 8 weeks grouped by first intermediate level (name, path, contact of the site)

# For the 8 previous weeks

tempFirstIntermediate <- parentSites[-which(parentSites$level<max(parentSites$level)),c("Id_Site","reference")]
colnames(tempFirstIntermediate) <- c("Id_parentSite","name_parentSite")

noReport_W8 <- data.frame(Id_parentSite=NA, name_parentSite=NA, siteName=NA, contact=NA, phone=NA)
# noReport_W8$Id_parentSite # site id of the parent site (first intermediate level)
# noReport_W8$name_parentSite # name of the parent site
# noReport_W8$siteID # Id of the site
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
      
      noReport_W8$siteID[counter] <- site_ID
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
# noReport_W3$siteID # Id of the site
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
      
      noReport_W3$siteID[counter] <- site_ID
      noReport_W3$siteName[counter] <- sites_id$reference[which(sites_id$id==site_ID)]
      noReport_W3$contact[counter] <- paste(sites_contact$name[which(sites_contact$FK_SiteId==site_ID & (sites_contact$isDeleted==0 | is.na(sites_contact$isDeleted)))], collapse = ", ")
      noReport_W3$phone[counter] <- paste(sites_contact$phoneNumber[which(sites_contact$FK_SiteId==site_ID)], collapse = ", ")
      
      noReport_W3 <- rbind(noReport_W3,NA)
      counter <- counter +1  
    }
  }
}

noReport_W3 <- noReport_W3[-nrow(noReport_W3),]

# modification of noReport_W3 so that there is no overlap with noReport_W8 (noReport_W3: centers with no reporting during the 3 to 7 previous weeks; noReport_W8: centers with no reporting during the 8 previous weeks)

noReport_W3 <- noReport_W3[-which(noReport_W3$siteID %in% noReport_W8$siteID),]

## diseaseThreshold_W12 ####

# Table with number of cases of each disease with thresholds in the 12 previous weeks for the whole country

fullrep_IDs <- NA # Ids of fullreport of interests
partrep_IDs <- NA # IDs of partreports (versions of the fullreports) of interest
rep_IDs <- NA # Ids of reports (specific values of each report version) of interest

fullrep_IDs <- strsplit(reportingValues_W12_overall$ids_fullreport_recVal[which(reportingValues_W12_overall$level==1)],split=",")[[1]]
fullrep_IDs <- as.numeric(fullrep_IDs)

if(any(is.na(fullrep_IDs))) {
  fullrep_IDs <- fullrep_IDs[-which(is.na(fullrep_IDs))]
} else {
}

partrep_IDs <- partreport$id[which(partreport$FK_FullReportId %in% fullrep_IDs & partreport$status=="VALIDATED")]

diseaseThreshold_W12_overall <- thresholds[,c("disease","diseaseName","threshold_value","variable")] # database with the number of cases of each disease with thresholds in the 12 previous weeks for the whole country

diseaseThreshold_W12_overall$occurence <- NA # nb of cases during the 12 previous weeks for the whole country

diseaseName <- NA

for (diseaseName in diseaseThreshold_W12_overall$disease) {
  
  rep_IDs <- report$id[which(report$FK_PartReportId %in% partrep_IDs & report$disease==diseaseName  & report$isArchived==0 & report$isDeleted==0)]
  
  varDisease <- NA
  varDisease <- diseaseThreshold_W12_overall$variable[which(diseaseThreshold_W12_overall$disease==diseaseName)]
  
  diseaseThreshold_W12_overall$occurence[which(diseaseThreshold_W12_overall$disease==diseaseName)] <- sum(report_values$Value[which(report_values$Key==varDisease & report_values$FK_ReportId %in% rep_IDs)])
  
}

# Table with the number of cases of each disease with threshold crossed in any week of the 12 previous weeks for the whole country

diseaseThreshold_W12 <- thresholds[,c("disease","diseaseName","threshold_value","variable")] # database with the number of cases of each disease with thresholds in the 12 previous weeks for the whole country

diseaseThreshold_W12$week <- NA

numSem <- NA

for (numSem in numSem_W12) {
  temp <- NA
  temp <- thresholds[,c("disease","diseaseName","threshold_value","variable")]
  temp$week <- NA
  temp$week <- numSem
  diseaseThreshold_W12 <- rbind(diseaseThreshold_W12,temp)
}

diseaseThreshold_W12$occurence <- NA # nb of cases during the 12 previous weeks for the whole country

diseaseThreshold_W12 <- diseaseThreshold_W12[-which(is.na(diseaseThreshold_W12$week)),]

listDiseaseNonZero_W12 <- diseaseThreshold_W12_overall$disease[which(diseaseThreshold_W12_overall$occurence>=1)]

diseaseName <- NA

numSem <- NA

fullrep_IDs <- NA
partrep_IDs <- NA
rep_IDs <- NA

for (numSem in numSem_W12) {
  
  for (diseaseName in listDiseaseNonZero_W12) {
    
    fullrep_IDs <- strsplit(reportingValues_W12$ids_fullreport_recVal[which(reportingValues_W12$level==1 & reportingValues_W12$week==numSem)],split=",")[[1]]
    fullrep_IDs <- as.numeric(fullrep_IDs)
    fullrep_IDs <- fullrep_IDs[-which(is.na(fullrep_IDs))]
    
    partrep_IDs <- partreport$id[which(partreport$FK_FullReportId %in% fullrep_IDs & partreport$status=="VALIDATED")]
    
    rep_IDs <- report$id[which(report$FK_PartReportId %in% partrep_IDs & report$disease==diseaseName  & report$isArchived==0 & report$isDeleted==0)]
    
    varDisease <- NA
    varDisease <- unique(diseaseThreshold_W12_overall$variable[which(diseaseThreshold_W12_overall$disease==diseaseName)])
    
    diseaseThreshold_W12$occurence[which(diseaseThreshold_W12$disease==diseaseName & diseaseThreshold_W12$week==numSem)] <- sum(report_values$Value[which(report_values$Key==varDisease & report_values$FK_ReportId %in% rep_IDs)])
    
  }
}

diseaseThreshold_W12 <- diseaseThreshold_W12[-which(is.na(diseaseThreshold_W12$occurence)),]

diseasesInterest_W12 <- unique(diseaseThreshold_W12$disease[which(diseaseThreshold_W12$occurence >= diseaseThreshold_W12$threshold_value)])

diseaseThreshold_W12 <- diseaseThreshold_W12[which(diseaseThreshold_W12$disease %in% diseasesInterest_W12),]


## diseaseThreshold_W2 ####


#### Table with one row for each site/disease having crossed threshold in any of the two previous week, the number of cases and longitude and latitude of the unit, path of the site and contact

fullrep_IDs <- NA # Ids of fullreport of interests
partrep_IDs <- NA # IDs of partreports (versions of the fullreports) of interest
rep_IDs <- NA 

fullrep_IDs <- strsplit(paste0(reportingValues_W12$ids_fullreport_recVal[which(reportingValues_W12$level==1 & reportingValues_W12$week %in% c(weekEnd-1, weekEnd))],collapse = ","),split=",")[[1]]

fullrep_IDs <- as.numeric(fullrep_IDs)

if(any(is.na(fullrep_IDs))) {
  fullrep_IDs <- fullrep_IDs[-which(is.na(fullrep_IDs))]
} else {
}

diseaseThreshold_W2 <- NA # table of interest

diseaseThreshold_W2 <- sites_relationships[which(sites_relationships$id %in% fullreport$FK_SiteRelationShipId[which(fullreport$id %in% fullrep_IDs)]),c("FK_SiteId","FK_ParentId","name","longitude","latitude")]

diseaseThreshold_W2$name_parentSite <- NA # name of the parent site
diseaseThreshold_W2$siteName  <- NA # name of the site
diseaseThreshold_W2$contact <- NA # name of the contact at the site
diseaseThreshold_W2$phone <- NA # phone of the contact at the site

rowLineId <- NA
for (rowLineId in 1:nrow(diseaseThreshold_W2)) {
  
  if (is.na(diseaseThreshold_W2$longitude[rowLineId]) | is.na(diseaseThreshold_W2$latitude[rowLineId]) ) {
    diseaseThreshold_W2$longitude[rowLineId] <- longLat$longitude[which(longLat$FK_SiteId==diseaseThreshold_W2$FK_ParentId[rowLineId])]
    diseaseThreshold_W2$latitude[rowLineId] <- longLat$latitude[which(longLat$FK_SiteId==diseaseThreshold_W2$FK_ParentId[rowLineId])]
  } else {
  }
  
  diseaseThreshold_W2$name_parentSite[rowLineId] <- sites_id$reference[which(sites_id$id==diseaseThreshold_W2$FK_ParentId[rowLineId])]
  diseaseThreshold_W2$siteName[rowLineId] <- sites_id$reference[which(sites_id$id==diseaseThreshold_W2$FK_SiteId[rowLineId])]
  diseaseThreshold_W2$contact[rowLineId] <- paste(sites_contact$name[which(sites_contact$FK_SiteId==diseaseThreshold_W2$FK_SiteId[rowLineId] & (sites_contact$isDeleted==0 | is.na(sites_contact$isDeleted)))], collapse = ", ")
  diseaseThreshold_W2$phone[rowLineId] <- paste(sites_contact$phoneNumber[which(sites_contact$FK_SiteId==diseaseThreshold_W2$FK_SiteId[rowLineId])], collapse = ", ")
}

disInt <- NA
diseaseThreshold_W2$disease <- NA
diseaseThreshold_W2$diseaseName <- NA
diseaseThreshold_W2$variable <- NA
diseaseThreshold_W2$threshold_value <- NA
temp <- NA
temp <- diseaseThreshold_W2

for (disInt in unique(diseaseThreshold_W12$disease)) {
  temp$disease <- NA
  temp$disease <- disInt
  temp$diseaseName <- thresholds$diseaseName[which(thresholds$disease==disInt)]
  temp$variable <- diseaseThreshold_W12_overall$variable[which(diseaseThreshold_W12_overall$disease==disInt)]
  temp$threshold_value <- diseaseThreshold_W12_overall$threshold_value[which(diseaseThreshold_W12_overall$disease==disInt)]
  diseaseThreshold_W2 <- rbind(diseaseThreshold_W2,temp)
}

diseaseThreshold_W2 <- diseaseThreshold_W2[-which(is.na(diseaseThreshold_W2$disease)),]

diseaseThreshold_W2$occurence <- NA

fullrep_IDs_site <- NA

site_ID <- NA

for (site_ID in unique(diseaseThreshold_W2$FK_SiteId)) {
  
  for (diseaseName in unique(diseaseThreshold_W12$disease)) {
    
    fullrep_IDs_site <- NA
    fullrep_IDs_site <- fullreport$id[which(fullreport$id %in% fullrep_IDs & fullreport$FK_SiteId==site_ID)]
    
    partrep_IDs <- NA
    partrep_IDs <- partreport$id[which(partreport$FK_FullReportId %in% fullrep_IDs_site & partreport$status=="VALIDATED")]
    
    rep_IDs <- NA
    rep_IDs <- report$id[which(report$FK_PartReportId %in% partrep_IDs & report$disease==diseaseName  & report$isArchived==0 & report$isDeleted==0)]
    
    varDisease <- NA
    varDisease <- unique(diseaseThreshold_W12_overall$variable[which(diseaseThreshold_W12_overall$disease==diseaseName)])
    
    diseaseThreshold_W2$occurence[which(diseaseThreshold_W2$disease==diseaseName & diseaseThreshold_W2$FK_SiteId==site_ID)] <- sum(report_values$Value[which(report_values$Key==varDisease & report_values$FK_ReportId %in% rep_IDs)])
    
  }
  
}

diseaseThreshold_W2 <- diseaseThreshold_W2[which(diseaseThreshold_W2$occurence!=0),]

## alertList_D10 ####

#### List of alerts received in the 10 previous days, name of the site, path and contact

alertList_D10 <- alerts[which(alerts$receptionDate >= today() -10),c("receptionDate","contactName","contactPhoneNumber","FK_SiteRelationShipId","message")]
alertList_D10$receptionDate <- as.character(alertList_D10$receptionDate)
alertList_D10 <- alertList_D10[order(alertList_D10$receptionDate,decreasing = T),]


if(nrow(alertList_D10)!=0) {
  alertList_D10$name_parentSite <- NA
  alertList_D10$name_Site <- NA
  
  rowLineId <- NA
  for (rowLineId in 1:nrow(alertList_D10)) {
    alertList_D10$name_parentSite[rowLineId] <- sites_id$reference[which(sites_id$id==sites_relationships$FK_ParentId[which(sites_relationships$id==alertList_D10$FK_SiteRelationShipId[rowLineId])])]
    alertList_D10$name_Site[rowLineId] <- sites_id$reference[which(sites_id$id==sites_relationships$FK_SiteId[which(sites_relationships$id==alertList_D10$FK_SiteRelationShipId[rowLineId])])]
  }
  
} else {  
}

## tableBeginYear ####

#### Cumulative table with the number of cases since beginning of year and the same period the year before for the whole country, one row per disease (all diseases), one column per year

tableBeginYear <- diseases_name[-which(diseases_name$disease=="ALERT"),c("id","disease","name")]


variablesNames <- unique(diseases_variable$value[which(diseases_variable$FK_DiseaseId %in% tableBeginYear$id)])

seqId <- NA
for (seqId in 1:length(variablesNames)) {
  tableBeginYear <- data.frame(tableBeginYear,a=NA)
}

colnames(tableBeginYear)[(ncol(tableBeginYear)-length(variablesNames)+1):ncol(tableBeginYear)] <- variablesNames

fullrep_IDs_YR_previous <- NA # Ids of fullreport of interests for the previous year
partrep_IDs <- NA # IDs of partreports (versions of the fullreports) of interest
rep_IDs <- NA 

fullrep_IDs_YR_previous <- strsplit(reportingValues_YR_previous$ids_fullreport_recVal[which(reportingValues_YR_previous$level==1)],split=",")[[1]]
fullrep_IDs_YR_previous <- as.numeric(fullrep_IDs_YR_previous)

if(any(is.na(fullrep_IDs_YR_previous))) {
  fullrep_IDs_YR_previous <- fullrep_IDs_YR_previous[-which(is.na(fullrep_IDs_YR_previous))]  
} else {
}

disInt <- NA
varInt <- NA

for (disInt in unique(tableBeginYear$disease)) {
  
  for (varInt in variablesNames) {
    partrep_IDs <- partreport_YR_previous$id[which(partreport_YR_previous$FK_FullReportId %in% fullrep_IDs_YR_previous & partreport_YR_previous$status=="VALIDATED")]
    
    rep_IDs <- report_YR_previous$id[which(report_YR_previous$FK_PartReportId %in% partrep_IDs & report_YR_previous$disease==disInt  & report_YR_previous$isArchived==0 & report_YR_previous$isDeleted==0)]
    
    tableBeginYear[which(tableBeginYear$disease==disInt),varInt] <- sum(report_values_YR_previous$Value[which(report_values_YR_previous$Key==varInt & report_values_YR_previous$FK_ReportId %in% rep_IDs)])
  }
}

colnames(tableBeginYear)[(ncol(tableBeginYear)-length(variablesNames)+1):ncol(tableBeginYear)] <- paste(variablesNames,yearPrevious)

seqId <- NA
for (seqId in 1:length(variablesNames)) {
  tableBeginYear <- data.frame(tableBeginYear,a=NA)
}

colnames(tableBeginYear)[(ncol(tableBeginYear)-length(variablesNames)+1):ncol(tableBeginYear)] <- variablesNames

fullrep_IDs_YR <- NA # Ids of fullreport of interests
partrep_IDs <- NA # IDs of partreports (versions of the fullreports) of interest
rep_IDs <- NA 

fullrep_IDs_YR <- strsplit(reportingValues_YR$ids_fullreport_recVal[which(reportingValues_YR$level==1)],split=",")[[1]]
fullrep_IDs_YR <- as.numeric(fullrep_IDs_YR)

if(any(is.na(fullrep_IDs_YR))) {
  fullrep_IDs_YR <- fullrep_IDs_YR[-which(is.na(fullrep_IDs_YR))]  
} else {
}

disInt <- NA
varInt <- NA

for (disInt in unique(tableBeginYear$disease)) {
  
  for (varInt in variablesNames) {
    partrep_IDs <- partreport$id[which(partreport$FK_FullReportId %in% fullrep_IDs_YR & partreport$status=="VALIDATED")]
    
    rep_IDs <- report$id[which(report$FK_PartReportId %in% partrep_IDs & report$disease==disInt  & report$isArchived==0 & report$isDeleted==0)]
    
    tableBeginYear[which(tableBeginYear$disease==disInt),varInt] <- sum(report_values$Value[which(report_values$Key==varInt & report_values$FK_ReportId %in% rep_IDs)])
  }
}

colnames(tableBeginYear)[(ncol(tableBeginYear)-length(variablesNames)+1):ncol(tableBeginYear)] <- paste(variablesNames,yearCurrent)
