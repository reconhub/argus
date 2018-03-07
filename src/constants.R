## API connection constants
main_api_url <- "http://avadar.novel-t.ch/argus-r/sesdashboard/web/"
user_name <- Sys.getenv("api_user_name")
api_password <- Sys.getenv("api_password")
langueage <-  Sys.getenv("api_locale")

## Dashboard settings
plot_colors <- c('#5A0A69', '#62B200')

## Administrative report settings
max_intermediate_levels <- 50

admin_plot_margins <- list(
  l = 70,
  r = 50,
  b = 70,
  t = 10,
  pad = 4
)
