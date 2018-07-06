# Load RData from argus_dashboard_raw_input_script.R
load("src/assets/epidemiological_report_raw_input.RData")

# Clean assets ####
# Remove previous plots
unlink(epidemiological_report_plots_paths)

# Load shapefiles ####
sp_files <- st_read(paste0(assets_path, shape_files))

# Preprocess data ####
disease_occurance_w12 <- epi_report_input$diseaseThreshold_W12 %>%
  mutate(
    year = ifelse(week <= 8, 2018, 2017), # This is temporary - year column needs to be in the raw data
    year_week = paste0("'", substr(year, 3, 4), " - W", week)) %>%
  arrange(year, week)

max_occurence <- max(disease_occurance_w12$occurence) + 1

# Create plots ####
# Disease occurance in the last 12 weeks
plots_disease_occurance_w12 <- disease_occurance_w12 %>%
  plot_occurance(., plot_colors[1],
                      x_title = i18n$t("epi_week_nb"),
                      y_title = i18n$t("nb_of_cases"),
                 max_occurence)

plots_disease_occurance_w12
ggsave(file = paste0(assets_path, "subplots_disease_occurance_w12.svg"),
       plot = plots_disease_occurance_w12, height = 10, width = 12)

# Create maps ####
diseaseThreshold_W2 <- epi_report_input$diseaseThreshold_W2 %>%
  mutate(`Occurence` = paste(disease, variable, occurence))

disease_location <- diseaseThreshold_W2 %>%
  group_by(disease, longitude, latitude) %>%
  summarise(occurence  = sum(occurence))

mock_data <- data.frame(disease = c("a", "b"), longitude =  rep(0.204, 2), latitude = rep(10.9, 2), occurence = c(20,30),
                        stringsAsFactors = F)

disease_location <- bind_rows(disease_location, mock_data)

country_data <- sp_files %>% dplyr::filter(GEOUNIT == country)

disease_maps <- plot_maps(country_data)

disease_maps

ggsave(file = paste0(assets_path, "maps.svg"), plot = disease_maps)

# Create tables with diseases ####
# Disease table occurrence
disease_occurance_above_threshold <- diseaseThreshold_W2 %>%
  select(siteName, name_parentSite, disease, contact, phone, occurence, threshold_value)

data.table::setnames(disease_occurance_above_threshold,
                     old = c("siteName", "name_parentSite", "disease", "contact", "phone", "occurence", "threshold_value"),
                     new = c(i18n$t("siteName"), i18n$t("name_parentSite"),
                             i18n$t("disease"),
                             i18n$t("contact"),
                             i18n$t("phone"),
                             i18n$t("occurrence"),
                             i18n$t("threshold")))

# Disease alerts
alert_list_D10 <- epi_report_input$alertList_D10 %>%
  select(receptionDate, name_Site, name_parentSite, contactName, contactPhoneNumber, message)

data.table::setnames(alert_list_D10,
                     old = names(alert_list_D10),
                     new = c(i18n$t("reception_Date"), i18n$t("name_Site"), i18n$t("name_parentSite"), i18n$t("contactName"),
                             i18n$t("contactPhoneNumber"), i18n$t("message")))

# Cummulative table
cumulative_table <- epi_report_input$tableBeginYear %>%
  select(-id, -disease)

this_year <-  format(Sys.Date(),"%Y")
last_year <-  as.numeric(format(Sys.Date(),"%Y")) - 1

data.table::setnames(cumulative_table,
                     old = names(cumulative_table),
                     new = c(i18n$t("disease"), paste(i18n$t("cas"), last_year), paste(i18n$t("desease"), last_year),
                             paste(i18n$t("cas"), this_year), paste(i18n$t("desease"), this_year)))

# Save output for markdown report ####
save(disease_occurance_above_threshold, alert_list_D10, cumulative_table,
     disease_maps, country_data,
     file = paste0(assets_path, "epi_report.Rdata"))
