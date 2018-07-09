### Master file to run all the calculations to compute Argus admnistrative and epidemiological dashboards

# Loading Packages and modules with functions ####
library(tidyr)
library(dplyr)
library(purrr)
library(sf)
library(ggplot2)
library(hrbrthemes)
library(shiny.i18n)

source("src/plots/ploting.R")
source("src/munging/munging.R")
source("src/constants.R")

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

source("src/dashboard_input_scripts/administrative_dashboard_input.R", echo = TRUE)
source("src/dashboard_input_scripts/epidemiological_dashboard_input.R", echo = TRUE)

# Render dashboards ####
# Dashboards are rendered as flexdashboards
rmarkdown::render("src/reports/administrative_dashboard.Rmd",
                  params = list(custom_title = i18n$t("admin_report_title")))

rmarkdown::render("src/reports/epidemiological_dashboard.Rmd",
                  params = list(custom_title = i18n$t("epi_report_title")))
