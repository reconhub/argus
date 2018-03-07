## API connection constants
main_api_url <- "http://avadar.novel-t.ch/argus-r/sesdashboard/web/"
user_name <- Sys.getenv("api_user_name")
api_password <- Sys.getenv("api_password")
language <-  Sys.getenv("api_locale")

## Paths
assets_path <- "src/assets/"
assets_admin_path <- paste0(assets_path, "administrative_report/")
translations_path <- "src/translations"

## Dashboard settings
plot_colors <- c('#5A0A69', '#62B200')

## Administrative report settings
adiministrative_report_plots_names <- c("central_plot.svg", "reporting_parent_sites.svg", 
                                        "review_plots.svg")

adiministrative_report_plots_paths <- paste0(assets_path, adiministrative_report_plots_names)

max_intermediate_levels <- 50

admin_plot_margins <- list(
  l = 70,
  r = 50,
  b = 70,
  t = 10,
  pad = 4
)
