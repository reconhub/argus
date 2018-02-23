plot_reporting_central_level <- function(last_12_weeks_level_1_long, plot_colors){
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
      xaxis = list(title = ~paste("<b>", reference,  '</b> Year Week', min(year_week), "-", max(year_week))),
      yaxis = list(title = 'Reporting <br> completeness/timeliness[%]', range =~c(0, 100)))
}

plot_1st_itermediate_level <- function(data, plot_colors, margins, order_x,
                                                 x_title, y_title) {
  plot_ly(data,
          x = ~reference,
          y = ~number,
          type = "bar",
          color = ~label,
          colors = plot_colors,
          split = ~label,
          text = ~paste0(label, ": ", number),
          hoverinfo = "text",
          showlegend = TRUE) %>%
    layout(
      margin =  bar_plot_margins,
      barmode = 'group',
      xaxis = list(
        categoryorder = "array",
        categoryarray = order_x,
        title = x_title),
      yaxis = list(title = y_title, range =~c(0, 100)))
}
