main_font_size = 14
axis_font_size = 12

plot_occurance <- function(disease_occurance_w12, plot_colors, x_title, y_title, max_occurence){
  ggplot(data = disease_occurance_w12,
          aes(x = year_week, y = occurence, group = variable, color = variable)) +
    facet_wrap(~disease, ncol = 2, scales = "free") +
    geom_line(size = 2) + geom_point(size = 4) +
    theme_ipsum() +
    labs(x = x_title, y = y_title) +
    scale_color_manual(values = plot_colors) +
    ylim(-0.5, max_occurence + 1) +
    plot_theme() +
    theme(
      axis.title.x = element_text(size = 18, family = "sans serif"),
      axis.title.y = element_text(size = 18, family = "sans serif"),
      axis.text.x = element_text(size = main_font_size, angle = -45, hjust = 0, family = "sans serif"),
      axis.text.y = element_text(size = main_font_size, family = "sans serif"),
      legend.position = "none", strip.text.x = element_text(size = 16, family = "sans serif", hjust = 0.5))
}

plot_theme <- function() {
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = main_font_size, family = "sans serif"),
        axis.title.y = element_text(size = main_font_size, family = "sans serif"),
        legend.text = element_text(size = main_font_size, family = "sans serif"),
        axis.text.x = element_text(size = axis_font_size, angle = -45, hjust = 0, family = "sans serif"),
        axis.text.y = element_text(size = axis_font_size, family = "sans serif"),
        legend.position = "top")
}

plot_reporting_central_level <- function(last_12_weeks_level_1_long, plot_colors,
                                         x_title, y_title) {
  ggplot2::ggplot(data = last_12_weeks_level_1_long,
          aes(x = year_week, y = number, color = label, group = label)) +
          geom_line(size = 2) + geom_point(size = 4) +
    theme_ipsum() +
    labs(x = x_title, y = y_title) +
    scale_color_manual(values = plot_colors) +
    ylim(0, 100) +
    plot_theme()
}

plot_1st_itermediate_level <- function(data, plot_colors,
                                       x_title, y_title) {
  ggplot2::ggplot(data,
          aes(x = reference, y = number, fill = label, group = label)) +
    geom_bar(stat = "identity", aes(fill = label), position = "dodge") +
    theme_ipsum() +
    labs(x = x_title, y = y_title) +
    scale_fill_manual(values = plot_colors) +
    ylim(0, 100) +
    plot_theme()
   
}

plot_maps <- function(country_data){
  country_box <- st_bbox(country_data)
  country_length <- country_box$xmax - country_box$xmin 
  x_margin <- 0.3 # percent
  long_range <- c(country_box$xmin - country_length * x_margin, country_box$xmax + country_length * x_margin)
  ggplot() +
    geom_sf(data = country_data, fill = "white") +
    geom_point(data = disease_location, aes(x = longitude, y = latitude, size = occurence),
                 color = plot_colors[1], alpha = 1) +
      scale_size(breaks = unique(disease_location$occurence), name = i18n$t("maps_legend")) +
      facet_wrap(~disease, ncol = 3) +
      theme_ipsum() +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            strip.text.x = element_text(size = 8, family = "sans serif", hjust = 0.5),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.title = element_text(size = 10, family = "sans serif"), 
            legend.text = element_text(size = 10, family = "sans serif"),
            legend.key = element_rect(fill = "white", colour = "white"),
            panel.spacing.x = unit(1, "lines"),
            legend.position = "top") + 
      expand_limits(x = long_range)
}
