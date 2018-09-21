### Master file to run all the calculations to compute Argus admnistrative and epidemiological dashboards

workingDir <- "C:/xampp/htdocs/ScriptsR/argus/"
# Set working directory and load correct libraries

# setwd(workingDir)
# packrat::on(getwd())

# Set the constants

source("src/constants.R")

# Loading Packages ####
library(tidyr)
library(dplyr)
library(purrr)
library(sf)
library(ggplot2)
library(hrbrthemes)
library(shiny.i18n)
library(scales)
library(grid)
library(flexdashboard)
library(gdtools)


# Translations setting ####
# file with translations 
i18n <- Translator$new(translation_csvs_path = translations_path,
                       translation_csv_config = paste0(translations_path, "/config.yaml"))

# set translation language
i18n$set_translation_language(language)

# Generate Rdata for dashboard pre-processing scripts
source("src/dashboard_input_scripts/argus_dashboard_raw_input_script.R")

# Generate tables and charts for the dashboards ####
# In order to have the flexdashboards as light as possible we generate the charts and tables in the separate scripts
# not to load plotly dependencies (or other) to Rmarkdown.

source("src/dashboard_input_scripts/administrative_dashboard_input.R")
source("src/dashboard_input_scripts/epidemiological_dashboard_input.R")

# Render dashboards ####
# Dashboards are rendered as flexdashboards
reportTime <- paste0(day(now()),"-",month(now()),"-",year(now())," ",substr(now(),12,16))

rmarkdown::render("src/reports/admin_dashboard.Rmd",
                  params = list(custom_title = paste(i18n$t("admin_report_title"),reportTime,i18n$t("update"),paste0(freqCron,"mn"))))     

rmarkdown::render("src/reports/epi_dashboard.Rmd",
                  params = list(custom_title = paste(i18n$t("epi_report_title"),reportTime,i18n$t("update"),paste0(freqCron,"mn"))))
