#### JG; put a header with the objective of the script

## API connection constants 
# JG: for the moment, no API will be used

# main_api_url <- "http://avadar.novel-t.ch/argus-r/sesdashboard/web/"
# user_name <- Sys.getenv("ARGUS_DATA_API_USER_NAME")
# api_password <- Sys.getenv("ARGUS_DATA_API_PASSWORD")
language <-  Sys.getenv("ARGUS_DASHBOARD_LOCALE")

## RSelenium options #JG: to be removed, use of ggplot2 instead to produce the svg files
## Options
extra_capabilities <- list(
  chromeOptions = 
    list(prefs = list(
      "profile.default_content_settings.popups" = 0L,
      "download.prompt_for_download" = FALSE,
      "download.default_directory" = Sys.getenv("ARGUS_CHROME_DOWNLOAD_DIR")
    )
    )
)

## Paths
assets_path <- "src/assets/"
assets_admin_path <- paste0(assets_path, "administrative_report/")
translations_path <- "src/translations"

## Dashboard settings
plot_colors <- c('#5A0A69', '#62B200')

## Administrative report settings
administrative_report_plots_names <- c("central_plot.svg", "reporting_parent_sites.svg", 
                                        "review_plots.svg")
epidemiological_report_plots_names <- c("subplots_disease_occurance_w12.svg", "maps.svg")

shape_files <- "ne_50m_admin_0_countries" #JG: needs to be the unzipped repository

administrative_report_plots_paths <- paste0(assets_path, administrative_report_plots_names)
epidemiological_report_plots_paths <- paste0(assets_path, epidemiological_report_plots_names)

max_intermediate_levels <- 50

admin_plot_margins <- list(
  l = 70,
  r = 50,
  b = 100,
  t = 10,
  pad = 4
)

bar_plot_margins <- list(
  l = 50,
  r = 50,
  b = 120,
  t = 20,
  pad = 4
)
