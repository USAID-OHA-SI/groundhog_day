si_style <- function(font_title = "Helvetica",
  font_subtitle = "Helvetica",
  font_plot = "Helvetica",
  font_caption = "Helvetica") {
  
  # Depends on plot_setup being run first
  
  # build off of theme_minimal settings
  ggplot2::theme_minimal() %+replace%
    
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = font_title,
        size = 14,
        face = "bold",
        color = "#202020",
        hjust = 0),
      
      #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
      plot.subtitle = ggplot2::element_text(
        family = font_subtitle,
        size = 12,
        color = "#202020",
        hjust = 0),
      plot.caption = ggplot2::element_text(
        family = font_caption,
        size = 9,
        color = "#202020"
      ),
      
      
      #Legend format
      # Set the legend to be at the top left of the graphic, below title
      legend.position = "top",
      legend.text.align = 0,
      legend.justification = c(0, 0),
      legend.background = ggplot2::element_blank(),
      #legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        family = font_title,
        size = 11),
      
      #Axis format
      axis.text = ggplot2::element_text(
        family = font_plot,
        size = 10,
        color = "#505050"),
      axis.text.x = ggplot2::element_text(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(
        family = font_plot,
        size = 10,
        colour = '#505050'),
      
      #Grid lines
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#D3D3D3"),
      panel.grid.major.x = ggplot2::element_line(colour = "#D3D3D3"),
      
      #Blank background
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      
      #Strip background (This sets the panel background for facet-wrapped plots to white)
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        family = font_title,
        size  = 11,
        hjust = 0.05,
        color = '#505050'),
    )
}
