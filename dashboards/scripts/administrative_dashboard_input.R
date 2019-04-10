## Purpose of this script is to generate svg plots and Rdata with tables input for html report

# Remove previous plots
unlink(config$administrative_report_plots_paths)

# translation

recode_report <- function(label) {
  recode(label, "compReport" = i18n$t("compReport"),
         "timeReport" = i18n$t("timeReport"))
}

recode_review <- function(label) {
  recode(label, "compReview" = i18n$t("compReview"),
         "timeReview" = i18n$t("timeReview"))
}

# plots

main_font_size = 14
axis_font_size = 12

plot_theme <- function() {
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.title.x = element_text(size = main_font_size, family = "sans"),
        axis.title.y = element_text(size = main_font_size, family = "sans"),
        legend.text = element_text(size = main_font_size, family = "sans"),
        axis.text.x = element_text(size = axis_font_size, angle = -25, hjust = 0.4, family = "sans"),
        axis.text.y = element_text(size = axis_font_size, family = "sans"),
        legend.position = "top",
        panel.border = element_blank())
}

last_12_weeks_central <- reportingValues_W12 %>%
  filter(level==1) %>% 
  select(week, compReport, timeReport) %>%
  gather(key=label, value=number, -week) %>%
  mutate(label=recode_report(label))


# Create plots ####


# reporting trend

last_12_weeks_central$week <- factor(as.character(last_12_weeks_central$week), levels = numSem_W12, labels=paste0(i18n$t("weekAbbr"),c(numSem_W12)))

central_plots <- ggplot(data = last_12_weeks_central, aes(x = week, y = number, color = label, group = label)) +
  geom_line(size = 2) + geom_point(size = 4) +
  # theme_ipsum(axis_title_just = "middle",grid = "XY") +
  labs(x = i18n$t("epi_week_nb"), y = "%") +
  scale_color_manual(values = config$plot_colors) +
  ylim(0, 100) +
  theme_bw() +
  plot_theme() +
  theme(axis.text.x = element_text(angle = 0), 
        axis.title.y = element_text(angle = 180,vjust = 0.5),
        panel.border = element_blank())

ggsave(file = paste0(config$assets_path, "central_plot.svg"), plot = central_plots, width = 7, height = 3.68)

# Overall reporting plot 

overall_reporting <- reportingValues_W12_overall %>%
  filter(level %in% c(1,max(reportingValues_W12_overall$level))) %>%
  select(reference, compReport, timeReport) %>%
  gather(key=label, value=number, -reference) %>%
  mutate(label=recode_report(label))
  
reporting_parent_sites <- ggplot(data = overall_reporting, aes(x = reference, y = number, group = label)) +
  geom_bar(stat="identity",aes(fill=label), position="dodge") +
  labs(x ="", y = "%") +
  scale_x_discrete(limits=unique(overall_reporting$reference)) +
  scale_fill_manual(values = config$plot_colors) +
  geom_vline(xintercept = 1.5, size=1.4) +
  ylim(0, 100) +
  theme_bw() +
  plot_theme() +
  theme(axis.title.y = element_text(angle = 180,vjust = 0.5),
        panel.border = element_blank())
  
ggsave(file = paste0(config$assets_path, "reporting_parent_sites.svg"), plot = reporting_parent_sites, width = 7, height = 3.68)

## Review plot 

overal_review <- reportingValues_W12_overall %>%
  select(reference, compReview, timeReview) %>%
  gather(key=label, value=number, -reference) %>%
  mutate(label=recode_review(label))

seq_group_vline <- which(!duplicated(parentSites$level) & parentSites$level!=1)-0.5

weekly_review_plots <- ggplot(data = overal_review, aes(x = reference, y = number, group = label)) +
  geom_bar(stat="identity",aes(fill=label), position="dodge") +
  # theme_ipsum(axis_title_just = "middle",grid = "XY") +
  labs(x ="", y = "%") +
  scale_x_discrete(limits=unique(overal_review$reference)) +
  scale_fill_manual(values = config$plot_colors) +
  geom_vline(xintercept = seq_group_vline, size=1.4) +
  ylim(0, 100) +
  theme_bw() +
  plot_theme() +
  theme(axis.title.y = element_text(angle = 180,vjust = 0.5),
        panel.border = element_blank())


ggsave(file = paste0(config$assets_path, "review_plots.svg"), plot = weekly_review_plots, width = 10, height = 3.68)

# Generate tables ####
# Silent sites
sites_no_report_3weeks <- noReport_W3 %>%
  select(name_parentSite, siteName)

data.table::setnames(sites_no_report_3weeks,
                    old = names(sites_no_report_3weeks),
                    new = c(i18n$t("name_parentSite"), i18n$t("siteName")))

sites_no_report_8weeks <- noReport_W8 %>%
  select(name_parentSite, siteName)

data.table::setnames(sites_no_report_8weeks,
                    old = names(sites_no_report_8weeks),
                    new = c(i18n$t("name_parentSite"), i18n$t("siteName")))
