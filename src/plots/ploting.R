f <- list(
  family = "sans serif",
  size = 14)

plot_occurance <- function(disease_occurance_w12, max_occurence, plot_colors, line_plot_margins){
  plot_ly(data = disease_occurance_w12,
          x = ~year_week, y = ~occurence,
          type = 'scatter', mode = 'lines+markers',
          color = ~variable,
          line = list(width = 4),
          marker = list(size = 8),
          colors = plot_colors,
          split = ~variable,
          text = ~paste0(disease, ": ", occurence), hoverinfo = "text",
          showlegend = FALSE) %>%
    layout(
      annotations =  list(
        text = ~disease, font = f,
        xref = "paper", yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE
      ),
      margin =  line_plot_margins,
      legend = list(orientation = 'h', y = 1.1, x = 0.5, font = list(size = 14)),
      xaxis = list(title = "Epidemiological week number", tickangle = 45),
      yaxis = list(title = 'Number of cases', range =~c(0, max_occurence)))
}

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
      legend = list(orientation = 'h', y = 1.1, x = 0.5, font = list(size = 14)),
      xaxis = list(title = "Epidemiological week number"),
      yaxis = list(title = '%', range =~c(0, 100)))
}

plot_1st_itermediate_level <- function(data, plot_colors, margins, order_x,
                                                 x_title, y_title, is_show_legend = TRUE) {
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
      margin =  bar_plot_margins,
      barmode = 'group',
      xaxis = list(
        categoryorder = "array",
        categoryarray = order_x,
        title = x_title),
      yaxis = list(title = y_title, range =~c(0, 100)))
}
