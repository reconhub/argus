########################################## TO SET ###############################################
## API connection constants
# main_api_url <- "http://avadar.novel-t.ch/argus-r/sesdashboard/web/"
# user_name <- "ARGUS_DATA_API_USER_NAME"
# api_password <- "ARGUS_DATA_API_PASSWORD"

## Translation language constant
language <-  "en"

## Country for which report is generated (used for selecting proper map)
country <- "Togo"

#################################################################################################

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

shape_files <- "ne_50m_admin_0_countries.shp"

administrative_report_plots_paths <- paste0(assets_path, administrative_report_plots_names)
epidemiological_report_plots_paths <- paste0(assets_path, epidemiological_report_plots_names)

max_intermediate_levels <- 50
