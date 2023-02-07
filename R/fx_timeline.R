
# function to build timelines for fellows


fix_cols <- function (data) 
  
{
  data$event <- data$Fellow
  data$col <- data$cols
  data$fontcol <- data$fontcolor
  
  data$start <- data$start
  data$start <- as.POSIXct(data$start)
  data$end <- as.POSIXct(data$end)
  if (any(is.na(data$end))) 
    data$end[is.na(data$end)] <- data$start[is.na(data$end)]
  data$event <- trimws(data$event)
  data$label <- data$event
  return(data[, c("event", "start", "end", 
                  "label", "col", "fontcol")])
}


gg_vistime_fellows <- function(data, title) {
  
  ylimit <- nrow(data)
  title <- as.character(title)
  
  gg_vistime(data,
    col.event = "Fellow",  # event names
    col.color = "cols",  # colors
    linewidth = 6, 
    optimize_y = FALSE, 
    title = "**Timeline of Fellowships**"
  ) + 
    scale_x_datetime(
      breaks = scales::breaks_width("1 year"), 
      labels = scales::date_format("%Y"), 
      limits = c(as.POSIXct("2015-06-01"), as.POSIXct("2022-10-01"))
    ) +
    coord_cartesian(
      ylim = c(0, ylimit + 1), 
      expand = FALSE, 
      clip = "off"
    ) +
    theme_classic(base_family = "sans") + 
    theme(
      legend.title = element_blank(), 
      plot.title = ggtext::element_markdown(size = 10, hjust = 0.5),
      panel.border = element_rect(size = .6, fill = NA, color = "grey60"),
      panel.grid = element_blank(), 
      axis.line = element_blank(), 
      axis.ticks.x = element_line(size = 0.5, color = "grey60"), 
      axis.ticks.length.x = unit(.25, "cm"),
      axis.ticks.y = element_blank(), 
      axis.text.x = element_text(size = 6, angle = 0, hjust = .5, vjust = .5),
      plot.margin = margin(0,0,0,0)
    )
}
