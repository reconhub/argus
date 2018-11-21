### Master file to run all the calculations to compute Argus admnistrative and epidemiological dashboards

### !!!!!!!!!!! Set working director !!!!!!!!!!!!!!!###

workingDir <- "C:/xampp/htdocs/ScriptsR/argus/dashboards/master.R"
setwd(workingDir)

# Load needed Packages ####

library(shiny.i18n,quietly = T)
library(RMySQL,quietly = T)
library(lubridate,quietly = T)
library(ggplot2,quietly = T)
library(svglite,quietly = T)
library(dplyr,quietly = T)
library(tidyr,quietly = T)
library(data.table,quietly = T)
library(sf,quietly = T)
library(grid,quietly = T)
library(rmarkdown,quietly = T)
library(yaml,quietly = T)

# Set the constants ###

db_config <- read_yaml(file="db/db_config.yaml")

config <- read_yaml(file ="dashboards/config.yaml")

administrative_report_plots_paths <- paste0(config$assets_path, config$administrative_report_plots_names)

epidemiological_report_plots_paths <- paste0(config$assets_path, config$epidemiological_report_plots_names)

# Translations setting ####
# file with translations 
i18n <- Translator$new(translation_csvs_path = config$translations_path,
                       translation_csv_config = paste0(config$translations_path, "config.yaml"))

# set translation language
i18n$set_translation_language(config$language)

# Process data for the dashboards
source("dashboards/scripts/argus_dashboard_raw_input_script.R")

# Generate tables and charts pof the dashboards #### 

source("dashboards/scripts/administrative_dashboard_input.R")
source("dashboards/scripts/epidemiological_dashboard_input.R")

# Render dashboards as HTML flexdashboards ####

reportTime <- paste0(day(now()),"-",month(now()),"-",year(now())," ",substr(now(),12,16))

rmarkdown::render("dashboards/reports/admin_dashboard.Rmd",
                  params = list(custom_title = paste(i18n$t("admin_report_title"),
                                                     reportTime,i18n$t("update"),
                                                     paste0(config$freqCron,"mn"))),
                                quiet=T)     

rmarkdown::render("dashboards/reports/epi_dashboard.Rmd",
                  params = list(custom_title = paste(i18n$t("epi_report_title"),
                                                     reportTime,i18n$t("update"),
                                                     paste0(config$freqCron,"mn"))), 
                                quiet=T)
