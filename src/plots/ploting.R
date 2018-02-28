plot_reporting_central_level <- function(last_12_weeks_level_1_long, plot_colors, line_plot_margins){
  plot_ly(data = last_12_weeks_level_1_long,
          x = ~year_week, y = ~number,
          type = 'scatter', mode = 'lines+markers',
          color = ~label,
          line = list(width = 4),
          marker = list(size = 8),
          colors = plot_colors,
          split = ~label,
          text = ~paste0(label, ": ", number), hoverinfo = "text",
          showlegend = TRUE) %>%
    layout(
      margin =  line_plot_margins,
      legend = list(orientation = 'h', y = 1.1, x = 0.5, font = list(size = 14)),
      xaxis = list(title = "Epidemiological week number"),
      yaxis = list(title = '%', range =~c(0, 100)))
}

plot_1st_itermediate_level <- function(data, plot_colors, margins, order_x,
                                       x_title, y_title, is_show_legend = TRUE,
                                       plot_margins) {
  plot_ly(data,
          x = ~reference,
          y = ~number,
          type = "bar",
          color = ~label,
          colors = plot_colors,
          split = ~label,
          text = ~paste0(label, ": ", number),
          hoverinfo = "text",
          showlegend = is_show_legend) %>%
    layout(
      legend = list(orientation = 'h', y = 1.1, x = 0.5, font = list(size = 14)),
      margin =  plot_margins,
      barmode = 'group',
      xaxis = list(
        categoryorder = "array",
        categoryarray = order_x,
        title = x_title),
      yaxis = list(title = y_title, range =~c(0, 100)))
}
