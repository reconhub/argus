library(plotly)
library(tidyr)
library(dplyr)
library(purrr)

load("src/assets/epidemiological_report_input.RData")
source("src/plots/ploting.R")

plot_colors <- c("red", "blue", "green", "pink", "yellow")
bar_plot_margins <- list(
    l = 50,
    r = 50,
    b = 120,
    t = 20,
    pad = 4
  )

disease_occurance_w12 <- epi_report_input$diseaseThreshold_W12 %>%
  mutate(
    year = ifelse(week < 8, 2018, 2017), # This is temporary - year column needs to be in the raw data
    year_week = paste0(year, " - W", week)) %>%
  arrange(year_week)

max_occurence <- max(disease_occurance_w12$occurence) + 1

plots_disease_occurance_w12 <- disease_occurance_w12 %>%
  split(disease_occurance_w12$disease) %>% 
  map2(plot_colors, ~plot_occurance(.x, max_occurence, .y,
                      line_plot_margins = bar_plot_margins))

plots_disease_occurance_w12[[1]]$x$attrs[[1]]$showlegend <- TRUE
nrow_charts <- ceiling(max(length(plots_disease_occurance_w12)/3, 1))

subplots_disease_occurance_w12 <- plots_disease_occurance_w12 %>%
  plotly::subplot(nrows = nrow_charts,
                  titleX = TRUE,
                  titleY = TRUE,
                  margin = 0.07,
                  widths = c(0.3, 0.4, 0.3))

options(viewer = NULL)
subplots_disease_occurance_w12 %>%
  htmlwidgets::onRender(
    "function(el, x) {
      var gd = document.getElementById(el.id); 
      Plotly.downloadImage(gd, {format: 'svg',
      width: 1200, height: 800, filename: 'subplots_disease_occurance_w12'});
    }"
    )

epi_report_input$diseaseThreshold_W2 %>%
  group_by(disease, longitude, latitude) %>%
  summarise(occurence  = sum(occurence))

leaflet() %>% addTiles()
