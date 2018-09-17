## Purpose of this script is to generate svg plots and Rdata with tables input for html report

# Remove previous plots
unlink(epidemiological_report_plots_paths)

# Load shapefiles ####
sp_files <- st_read(paste0(assets_path, shape_files))

# plots

main_font_size = 14
axis_font_size = 12

plot_theme <- function() {
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = main_font_size, family = "sans"),
        axis.title.y = element_text(size = main_font_size, family = "sans"),
        legend.text = element_text(size = main_font_size, family = "sans"),
        axis.text.x = element_text(size = axis_font_size, angle = -25, hjust = 0.4, family = "sans"),
        axis.text.y = element_text(size = axis_font_size, family = "sans"),
        legend.position = "top")
}

# nb of report received and validated taken into account in the analyses

nbRecValid_W12 <- reportingValues_W12_overall$nbReceivedValidated[which(reportingValues_W12_overall$level==1)]
nbExpected_W12 <- reportingValues_W12_overall$nbExpected[which(reportingValues_W12_overall$level==1)]
percRecValid_W12 <- round((nbRecValid_W12/nbExpected_W12)*100,0)

nbRecValid_YR <- reportingValues_YR$nbReceivedValidated[which(reportingValues_YR$level==1)]
nbExpected_YR <- reportingValues_YR$nbExpected[which(reportingValues_YR$level==1)]
percRecValid_YR <- round((nbRecValid_YR/nbExpected_YR)*100,0)

nbRecValid_YR_previous <- reportingValues_YR_previous$nbReceivedValidated[which(reportingValues_YR_previous$level==1)]
nbExpected_YR_previous <- reportingValues_YR_previous$nbExpected[which(reportingValues_YR_previous$level==1)]
percRecValid_YR_previous <- round((nbRecValid_YR_previous/nbExpected_YR_previous)*100,0)

# graph on disease occurrence

# TODO: to allow several variables to be displayed in case of multiple variables per disease
plot_occurrence <- ggplot(data = diseaseThreshold_W12,aes(x = week, y = occurence, group = variable, color = variable)) +
  facet_wrap(~diseaseName, ncol = 3, scales = "free") +
  geom_line(size = 2) + geom_point(size = 4) +
  theme_ipsum(axis_title_just = "middle",grid = "XY",base_family = "sans",subtitle_family = "sans") +
  scale_x_continuous(labels=paste0(i18n$t("weekAbbr"),c(numSem_W12[seq.int(1,12,by=2)])),breaks = numSem_W12[seq.int(1,12,by=2)]) +
  labs(x = i18n$t("epi_week_nb"), y = i18n$t("nb_of_cases")) +
  scale_color_manual(values = plot_colors[1])+
  scale_y_continuous(limits=c(0,NA), expand=expand_scale(add = c(0.8,2))) +
  plot_theme() +
  theme(
    axis.title.x = element_text(size = main_font_size, family = "sans"),
    axis.title.y = element_text(size = main_font_size, family = "sans"),
    axis.text.x = element_text(size = axis_font_size, angle = 0, family = "sans"),
    axis.text.y = element_text(size = axis_font_size, family = "sans"),
    legend.position = "none", strip.text.x = element_text(size = main_font_size, family = "sans", hjust = 0.5)) +
  ggtitle(paste0(i18n$t("representativeness"),nbRecValid_W12,"/",nbExpected_W12," (",percRecValid_W12,"%)"))

ggsave(file = paste0(assets_path, "trends_occurrence.svg"), plot = plot_occurrence, width = 10, height = 9)

# Create maps ####

if(nrow(diseaseThreshold_W2)==0) {
  
} else {
  
  disease_interest <- diseaseThreshold_W2 %>% group_by(disease,threshold_value) %>% summarise(occurence = sum(occurence)) 
  disease_interest <- disease_interest$disease[which(disease_interest$occurence >= disease_interest$threshold_value)]
  
  disease_location <- diseaseThreshold_W2 %>%
    group_by(disease, diseaseName,longitude, latitude) %>%
    summarise(occurence  = sum(occurence))
  
  disease_location <- disease_location[which(disease_location$disease%in% disease_interest),]

  diseasesMap <- unique(disease_location$diseaseName)

  country_data <- sp_files %>% dplyr::filter(GEOUNIT == country)

  svg(paste0(assets_path, "map_occurrence.svg"), width = 10, height = 10)

  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow=ceiling(length(diseasesMap)/3), ncol=3)))

  j <- rep(x = c(1,2,3), times=ceiling(length(diseasesMap)/3))

  mapTemp <- NA

  for (i in 1:length(diseasesMap)) {
  
  mapTemp <-  ggplot() +
      geom_sf(data = country_data, fill = "white") +
      geom_point(data = disease_location[which(disease_location$diseaseName==diseasesMap[i]),], aes(x = longitude, y = latitude, size = occurence), color = plot_colors[1], alpha = 1) +
      scale_size(breaks = sort(unique(disease_location$occurence[which(disease_location$diseaseName==diseasesMap[i])])), name = i18n$t("maps_legend"), range = c(2,4)) +
      theme_ipsum(base_family = "sans",subtitle_family = "sans") +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_text(size = main_font_size, family = "sans"), 
            legend.text = element_text(size = main_font_size, family = "sans"),
            legend.key = element_rect(fill = "white", colour = "white"),
            panel.spacing.x = unit(1, "lines"),
            legend.position = "right", legend.box = "vertical") +
            ggtitle(diseasesMap[i])
  
  
    print(mapTemp, vp=viewport(layout.pos.row=ceiling(i/3), layout.pos.col=j[i]))
  } 

  dev.off()

}

# Create tables with diseases ####
# Disease table occurrence

if(nrow(diseaseThreshold_W2)==0) {
  
  disease_occurrence_above_threshold <- data_frame(name_parentSite=NA, siteName=NA, diseaseName=NA,  threshold_value=NA, occurence=NA)
  
  data.table::setnames(disease_occurrence_above_threshold,
                       old = c("name_parentSite", "siteName", "diseaseName", "threshold_value", "occurence"),
                       new = c(i18n$t("name_parentSite"),
                               i18n$t("siteName"), 
                               i18n$t("disease"),
                               i18n$t("threshold"),
                               i18n$t("occurrence")))
  
} else {
  
  disease_occurrence_above_threshold <- diseaseThreshold_W2[which(diseaseThreshold_W2$occurence>=diseaseThreshold_W2$threshold_value),c("name_parentSite", "siteName",  "diseaseName", "threshold_value", "occurence")]
  
  disease_occurrence_above_threshold <- data.table::setorder(disease_occurrence_above_threshold,name_parentSite)

  data.table::setnames(disease_occurrence_above_threshold,
                       old = c("name_parentSite", "siteName", "diseaseName", "threshold_value", "occurence"),
                       new = c(i18n$t("name_parentSite"),
                               i18n$t("siteName"), 
                               i18n$t("disease"),
                               i18n$t("threshold"),
                               i18n$t("occurrence")))
}
# Disease alerts

if(length(alertList_D10$receptionDate)==0) {
  alert_list_D10 <- data_frame(receptionDate=NA, name_Site=NA, name_parentSite=NA, message=i18n$t("no_alert"))
  data.table::setnames(alert_list_D10,
                       old = names(alert_list_D10),
                       new = c(i18n$t("reception_Date"), i18n$t("name_Site"), i18n$t("name_parentSite"),  i18n$t("message")))
} else {
  alert_list_D10 <- alertList_D10 %>%
  select(receptionDate, name_Site, name_parentSite, message)
    data.table::setnames(alert_list_D10,
                       old = names(alert_list_D10),
                       new = c(i18n$t("reception_Date"), i18n$t("name_Site"), i18n$t("name_parentSite"), i18n$t("message")))
}

# Cummulative table
cumulative_table <- tableBeginYear %>% select(-id, -disease)

this_year <-  format(Sys.Date(),"%Y")
last_year <-  as.numeric(format(Sys.Date(),"%Y")) - 1

data.table::setnames(cumulative_table,
                     old = names(cumulative_table),
                     new = c(i18n$t("disease"), paste(i18n$t("cas"), last_year), paste(i18n$t("desease"), last_year),
                             paste(i18n$t("cas"), this_year), paste(i18n$t("desease"), this_year)))
