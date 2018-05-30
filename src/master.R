### Master file to run all the calculations to compute Argus admnistrative and epidemiological dashboards

# Loading Packages and modules with functions ####
library(plotly)
library(tidyr)
library(dplyr)
library(purrr)
library(sf)
library(ggplot2)
library(hrbrthemes)
library(shiny.i18n)
library(RSelenium)

source("src/plots/ploting.R")
source("src/munging/munging.R")
source("src/constants.R")

# Translations setting ####
# file with translations 
i18n <- Translator$new(translation_csvs_path = translations_path,
                       translation_csv_config = paste0(translations_path, "/config.yaml"))

#### JG: currently in the translation file the year number is hardcoded, should not be the case (e.g. raw:cas_this_year; en: Cas 2018).


# set translation language
i18n$set_translation_language(language)

# Setup RSelenium server ####  
# RSelenium is required to save plotly charts as svg  # JG: remove the use of RSelenium replacing plotly by ggplot2 to produce the charts

# rselenium_server <- RSelenium::rsDriver(remoteServerAddr = "localhost", port = 4445L, browser = "chrome", extraCapabilities = extra_capabilities) # linux

rselenium_server <- RSelenium::rsDriver(remoteServerAddr = "192.168.99.100", port = 4445L, browser = "chrome", extraCapabilities = extra_capabilities) #### JG: windows request when using Docker Toolbox (check ip: on docker: docker-machine ip)

# Generate tables and charts for the dashboards ####
# In order to have the flexdashboards as light as possible we generate the charts and tables in the separate scripts
# not to load plotly dependencies (or other) to Rmarkdown.

#### JG : add the necessary lines to run the script argus_dashboard_raw_input_script and produce the required objects for the two below scripts

source("src/dashboard_input_scripts/administrative_dashboard_input.R", echo = TRUE)
source("src/dashboard_input_scripts/epidemiological_dashboard_input.R", echo = TRUE)

# Close RSelenium chrom windows # JG: remove the use of RSelenium replacing plotly by ggplot2 to produce the charts
rselenium_server$client$closeWindow()
rselenium_server$server$stop()

# Render dashboards ####
# Dashboards are rendered as flexdashboards
rmarkdown::render("src/reports/administrative_dashboard.Rmd",
                  params = list(custom_title = i18n$t("admin_report_title")))

rmarkdown::render("src/reports/epidemiological_dashboard.Rmd",
                  params = list(custom_title = i18n$t("epi_report_title")))
