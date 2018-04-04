Sys.setenv(locale = "en")

## Options
options(viewer = NULL)
options(browser = "/opt/google/chrome/chrome")

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

## file with translations
i18n <- Translator$new(translation_csvs_path = translations_path,
                       translation_csv_config = paste0(translations_path, "/config.yaml"))

## set translation language
i18n$set_translation_language(language)

## setup RSelenium server
rselenium_server <- RSelenium::rsDriver(browser = "chrome", extraCapabilities = extra_capabilities)

source("src/dashboard_input_scripts/admnistrative_dashboard.R", echo = TRUE)
source("src/dashboard_input_scripts/epidemiological_dashboard_input.R", echo = TRUE)

rselenium_server$client$closeWindow()
rselenium_server$server$stop()

rmarkdown::render("src/reports/administrative_dashboard.Rmd",
                  params = list(custom_title = i18n$t("admin_report_title")))

rmarkdown::render("src/reports/epidemiological_dashboard.Rmd",
                  params = list(custom_title = i18n$t("epi_report_title")))
