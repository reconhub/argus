library(plotly)
library(tidyr)
library(dplyr)
library(purrr)
library(sf)
library(ggplot2)
library(hrbrthemes)
library(shiny.i18n)

## file with translations
i18n <- Translator$new(translation_csvs_path = "src/translations")

## set translation language
i18n$set_translation_language("en")

load("src/assets/epidemiological_report_raw_input.RData")
source("src/plots/ploting.R")

sp_files <- st_read("src/assets/ne_50m_admin_0_countries.shp")

plot_colors <- c('#5A0A69', '#62B200')
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
  map(~plot_occurance(., max_occurence, plot_colors[1],
                      line_plot_margins = bar_plot_margins,
                      x_title = i18n$t("epi_week_nb"),
                      y_title = i18n$t("nb_of_cases")))

plots_disease_occurance_w12[[1]]$x$attrs[[1]]$showlegend <- TRUE
nrow_charts <- ceiling(max(length(plots_disease_occurance_w12)/3, 1))

subplots_disease_occurance_w12 <- plots_disease_occurance_w12 %>%
  plotly::subplot(nrows = nrow_charts,
                  titleX = TRUE,
                  titleY = TRUE,
                  margin = 0.08,
                  widths = c(0.3, 0.4, 0.3))

subplots_disease_occurance_w12

options(viewer = NULL)
subplots_disease_occurance_w12 %>%
  htmlwidgets::onRender(
    "function(el, x) {
      var gd = document.getElementById(el.id); 
      Plotly.downloadImage(gd, {format: 'svg',
      width: 1200, height: 800, filename: 'subplots_disease_occurance_w12'});
    }"
    )

diseaseThreshold_W2 <- epi_report_input$diseaseThreshold_W2 %>%
  mutate(`disease_threshold` = paste(disease, variable, occurence, ">=", threshold_value))

disease_location <- diseaseThreshold_W2 %>%
  group_by(disease, longitude, latitude) %>%
  summarise(occurence  = sum(occurence))

country_data <- sp_files %>% dplyr::filter(GEOUNIT == "Togo")

disease_maps <- ggplot() +
  geom_sf(data = country_data, fill = "white") +
  geom_point(data = disease_location, aes(x = longitude, y = latitude, size = occurence),
             color = plot_colors[1], alpha = 0.7) +
  scale_size(breaks = unique(disease_location$occurence)) +
  facet_wrap(~disease, ncol = 2) +
  theme_ipsum() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        legend.text = element_text(size = 10), legend.title = element_blank(),
        legend.key = element_rect(fill = "white", colour = "white"))

ggsave(file = "src/assets/maps.svg", plot = disease_maps, width = 10, height = 8)

disease_occurance_above_threshold <- diseaseThreshold_W2 %>%
  select(siteName, name_parentSite, contact, phone, disease_threshold)

data.table::setnames(disease_occurance_above_threshold,
                     old = c("siteName", "name_parentSite", "contact", "phone", "disease_threshold"),
                     new = c(i18n$t("siteName"), i18n$t("name_parentSite"), i18n$t("contact"),
                               i18n$t("phone"), i18n$t("disease_threshold")))
  
alert_list_D10 <- epi_report_input$alertList_D10 %>%
  select(name_Site, name_parentSite, contactName, contactPhoneNumber, message)

data.table::setnames(alert_list_D10,
                     old = names(alert_list_D10),
                     new = c(i18n$t("name_Site"), i18n$t("name_parentSite"), i18n$t("contactName"),
                             i18n$t("contactPhoneNumber"), i18n$t("message")))

cumulative_table <- epi_report_input$tableBeginYear %>%
  select(-id, -name)

data.table::setnames(cumulative_table,
                     old = names(cumulative_table),
                     new = c(i18n$t("disease"), i18n$t("cas_previous_year"), i18n$t("desease_previous_year"),
                             i18n$t("cas_this_year"), i18n$t("desease_this_year")))

save(disease_occurance_above_threshold, alert_list_D10, cumulative_table,
     file = "src/assets/epi_report.Rdata")
