#####################################
##### Prerequisites
# None

##### Data 
# Argus API

##### Steps
# Authentification to Argus server
# Collection of the data of interest
#
#
#
#

#### Results
# Dataframes with the needed data for Argus dashboards
#######################################

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

############ Needed variables
# access to the server
source("src/constants.R")

# period of interest


########### Authentication to Argus server

url_login_check <- paste0(main_api_url, "api/login_check")
params <- list(
  username = user_name,
  password = api_password
)

con <- POST(url_login_check, body = params)
json <- content(con)

api_token <- json$token

########### Collection of the data of interest

# Function to retrieve data from the API 
get_url_to_json <- function(url){
  get_request <- GET(url, add_headers(Authorization = paste("Bearer", api_token)))
  content(get_request, type="application/json", encoding = "UTF-8")
}

# list of sites

sites_url <- paste0(main_api_url,"api/sites")
json_sites_list <- get_url_to_json(sites_url)

#GET(sites_url, add_headers(Authorization = paste("Bearer", api_token)))

# list of diseases
diseases_url <- paste0(main_api_url, language, "/api/diseases")
json_disease_list <- get_url_to_json(diseases_url)


# list of reports
reports_url <- paste0(main_api_url, language, "/api/reportData")
json_report <- get_url_to_json(reports_url)


# Extract the diseases id
diseases_id <- json_disease_list$diseases[[1]]$id

diseases_specific_url <- paste0(main_api_url,
                                language, "/api/diseases/", diseases_id)
diseases_raw <- get_url_to_json(diseases_specific_url)

## Get data report
reports_url <- paste0(main_api_url, language, "/api/reportData")
json_report <- get_url_to_json(reports_url)
report <- json_report %>% map_df(~bind_rows(.))
head(report)

report_sitde_status_url <- paste0(main_api_url, language,
                                  "/api/reportData/", report$sid[1], "/", report$sts[1])

report_site_status <- get_url_to_json(report_sitde_status_url)
head(report_site_status)

report_site_status_df <- report_site_status %>% map_df(~bind_rows(.))
head(report_site_status_df)

## Fetch multiple results this will loop over siteid and status
## from report table and query the results
get_multiple_results <- function(sid, sts, ...) {
  report_sitde_status_url <- paste0(main_api_url, language,
                                    "/api/reportData/", sid, "/", sts)
  
  report_site_status <- get_url_to_json(report_sitde_status_url)
  report_site_status %>% map_df(~bind_rows(.))
}

all_reports <- report %>%
  pmap_df(get_multiple_results)
