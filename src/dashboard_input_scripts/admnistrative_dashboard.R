library(plotly)
library(tidyr)
library(dplyr)
library(purrr)

load("src/assets/admin_report_raw_input.RData")
source("src/plots/ploting.R")
source("src/munging/munging.R")
source("src/constants.R")

## Options
options(viewer = NULL)

## Reporting
last_12_weeks_report_status <- admin_report_input$reportingValues_W12 %>%
  round_review_report()

last_12_weeks_level_2 <- last_12_weeks_report_status %>%
  dplyr::filter(level == 1) %>%
  mutate(
    year = ifelse(week < 12, 2018, 2017), # This is temporary - year column needs to be in the raw data
    year_week = paste0(year, " - W", week)) %>%
  arrange(year_week)

min_year <- min(last_12_weeks_level_2$year)
max_year <- max(last_12_weeks_level_2$year)

min_week <- last_12_weeks_level_2 %>%
  filter(year == min_year) %>%
  pull(week) %>% min()

max_week <- last_12_weeks_level_2 %>%
  filter(year == max_year) %>%
  pull(week) %>% max()

last_12_weeks_level_1_long <- last_12_weeks_level_2 %>%
  select(Id_Site, year_week, week, compReport, timeReport, reference) %>% 
  gather(key = label, value = number, -Id_Site, -year_week, -week, -reference) %>%
  mutate(label = recode_report(label))

central_plots <- last_12_weeks_level_1_long %>%
  plot_reporting_central_level(plot_colors, line_plot_margins = admin_plot_margins)


central_plots %>%
  htmlwidgets::onRender(
    "function(el, x) {
     var gd = document.getElementById(el.id); 
     Plotly.downloadImage(gd, {format: 'svg', width: 800, height: 400, filename: 'central_plot'});
   }"
  )

export(central_plots, "src/assets/administrative_report/central_plot.png")

## Overall reporting
overall_12_weeks_report_status <- admin_report_input$reportingValues_W12_overall %>%
  round_review_report()

first_intermediate_level <- overall_12_weeks_report_status$level %>% max()

count_levels <- overall_12_weeks_report_status %>%  group_by(level) %>% count()
count_first_intermediate_level <- count_levels %>% filter(level == first_intermediate_level) %>% pull(n)

selected_level <- ifelse(count_first_intermediate_level < max_intermediate_levels, first_intermediate_level, first_intermediate_level + 1)

parent_sites <- overall_12_weeks_report_status %>%
  dplyr::filter(level == selected_level) %>%
  select(compReport, timeReport, Id_Site, reference, FK_ParentId) %>% 
  gather(key = label, value = number, -Id_Site, -reference, -FK_ParentId) %>%
  mutate(label = recode_report(label),
         parent_label = factor(paste(FK_ParentId, reference, sep = "_"))) %>%
  arrange(parent_label)

order_sites <- unique(parent_sites$parent_label)

reporting_parent_sites <- parent_sites %>%
  plot_1st_itermediate_level(plot_colors = plot_colors,
                             margins = admin_plot_margins,
                             order_x = order_sites,
                             x_title = '',
                             y_title = '%',
                             plot_margins = admin_plot_margins)

reporting_parent_sites %>%
  htmlwidgets::onRender(
    "function(el, x) {
     var gd = document.getElementById(el.id); 
     Plotly.downloadImage(gd, {format: 'svg', width: 800, height: 400, filename: 'reporting_parent_sites'});
   }"
  )

export(reporting_parent_sites, "src/assets/administrative_report/reporting_parent_sites.png")

## Review 
reviewing_sites <- overall_12_weeks_report_status

reviewing_sites_long <- reviewing_sites %>%
  select(compReview, timeReview, FK_ParentId, reference, level) %>% 
  gather(key = label, value = number, -reference, -FK_ParentId, -level) %>%
  mutate(label = recode_review(label)) %>%
  arrange(level)

order_sites_review <- unique(reviewing_sites_long$reference)

plots_above_first_intermediate_level <- reviewing_sites_long %>%
  filter(level < first_intermediate_level) %>%
  plot_1st_itermediate_level(plot_colors = plot_colors,
                             margins = admin_plot_margins,
                             order_x = order_sites_review,
                             x_title = '',
                             y_title = '%',
                             is_show_legend = FALSE,
                             plot_margins = admin_plot_margins)

reviewing_sites_long_1st_intermediate_level <-  reviewing_sites_long %>%
  filter(level == first_intermediate_level)

reviewing_sites_long_1st_intermediate_level %>% 
  split(reviewing_sites_long_1st_intermediate_level$FK_ParentId) %>%
  map(function(df){
    plot_1st_itermediate_level(data = df, plot_colors = plot_colors,
                               margins = admin_plot_margins,
                               order_x = order_sites_review,
                               x_title = '',
                               y_title = '%',
                               is_show_legend = FALSE,
                               plot_margins = admin_plot_margins) }) -> plots_first_intermediate_level

plots_first_intermediate_level[[1]]$x$attrs[[1]]$showlegend <- TRUE
nrow_charts <- ceiling(max(length(plots_first_intermediate_level)/2, 1))

subplots_plots_first_intermediate_level <- plots_first_intermediate_level %>%
  subplot(nrows = nrow_charts, titleX = TRUE, titleY = TRUE, margin = 0.15)

review_plots <- subplot(plots_above_first_intermediate_level, subplots_plots_first_intermediate_level,
        nrows = nrow_charts + 1)

review_plots %>%
  htmlwidgets::onRender(
    "function(el, x) {
     var gd = document.getElementById(el.id); 
     Plotly.downloadImage(gd, {format: 'svg', width: 800, height: 400, filename: 'review_plots'});
   }"
  )

export(review_plots, "src/assets/administrative_report/review_plots.png")

## Silent sites
sites_no_report_3weeks <- admin_report_input$noReport_W3 %>%
  select(`Area` = name_parentSite,
         `Site name` = siteName,
         Contact = contact,
         Phone = phone)

sites_no_report_8weeks <- admin_report_input$noReport_W8 %>%
  select(`Area` = name_parentSite,
         `Site name` = siteName,
         Contact = contact,
         Phone = phone)

## Save output for markdown report
save(min_week, max_week, min_year, max_year,
  sites_no_report_3weeks, sites_no_report_8weeks, file = "src/assets/admin_report.RData")

write.csv(sites_no_report_8weeks, "src/assets/administrative_report/sites_no_report_8weeks.csv", row.names = FALSE)
write.csv(sites_no_report_3weeks, "src/assets/administrative_report/sites_no_report_3weeks.csv", row.names = FALSE)

files_to_zip <- dir("src/assets/administrative_report/", full.names = TRUE)
zip(zipfile = 'src/assets/administrative_report', files = files_to_zip)
unlink('src/assets/administrative_report', recursive = TRUE)
