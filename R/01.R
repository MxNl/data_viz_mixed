plot_national_area <- function(national_data, pal, color_bknd, year){
  
  # to label flow categories
  sec_labels <- national_data  %>%
    filter(week == max(national_data$week)) %>%
    distinct(percentile_cond, prop) %>%
    mutate(prop = cumsum(prop))
  
  plot_nat <- national_data %>% 
    ggplot(aes(week, prop)) +
    geom_area(aes(fill = percentile_bin)) +
    theme_classic() +
    labs(x = "Week in Year",
         y="") +
    scale_fill_manual(values = rev(pal)) +
    scale_y_continuous(trans = "reverse",
                       breaks = rev(c(0.05,0.5, 0.95)), 
                       labels = c("0%","obs. wells","100%"),
                       sec.axis = dup_axis(
                         labels = c("Dry", "", "Wet")
                       )) +
    theme_flowfacet(base = 12, color_bknd, text_color) +
    theme(axis.text.y = 
            element_text(size = 12, 
                         vjust = c(1, 0), 
                         hjust = 1),
          axis.title.x.bottom = element_text(size = 20,
                                             vjust = -1,
                                             margin = margin(t = 5)),
          axis.title.x.top = element_text(size = 20,
                                          vjust = 0,
                                          margin = margin(b = -5)),
          axis.text.x.bottom = element_text(size = 12,
                                            vjust = 1,
                                            # nudge labels up closer to bottom
                                            margin = margin(t = -7))) +
    scale_x_continuous(
      breaks = c(1, seq(10, 40, 10), 52),
                 position = "bottom",
                 labels = c(1, seq(10, 40, 10), 52),
                 sec.axis = dup_axis(
                   name = "National"
                 )) +
    coord_fixed(ratio = 28, clip = "off")
  
  
  return(plot_nat)
}

#' @description Compose the final plot and annotate
#' @param file_out Filepath to save to
#' @param plot_left The national plot to position on the left
#' @param plot_right The state tiles to position on the right
#' @param date_start first day of focal month
#' @param width Desired width of output plot
#' @param height Desired height of output plot
#' @param color_bknd Plot background color
combine_plots <- function(file_svg, plot_left, plot_right, width, height, color_bknd){
  
  plot_year <- "Year"
  
  # import fonts
  font_legend <- 'Noto Sans Mono'
  font_add_google(font_legend)
  sysfonts::font_add("sand", "fonts/Angeline Vintage_Demo.ttf")
  showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 700)
  showtext_auto(enable = TRUE)
  
  text_color <- "#444444"
  
  # usgs logo
  # usgs_logo <- magick::image_read('in/usgs_logo.png') %>%
  #   magick::image_colorize(100, text_color)
  
  # streamflow title
  title_flow <- magick::image_read('01_usgs_flowperc_geofacets/groundwater_germany_logo.png')
  
  plot_margin <- 0.025
  
  # background
  canvas <- grid::rectGrob(
    x = 0, y = 0, 
    width = 16, height = 9,
    gp = grid::gpar(fill = color_bknd, alpha = 1, col = color_bknd)
  )
  
  # Restyle legend
  plot_left <- plot_left +
    guides(fill = guide_colorsteps(
      title = "",
      nrow = 1,
      direction = 'horizontal',
      label.position = "bottom",
      barwidth = 22,
      barheight = 1,
      background = element_rect(fill = NA),
      show.limits = TRUE,
      even.steps = FALSE
    )) +
    theme(legend.background = element_rect(fill = NA),
          text = element_text(family = font_legend, color = text_color))
  
  # Extract from plot
  plot_legend <- get_legend(plot_left)
  
  # compose final plot
  ggdraw(ylim = c(0,1), 
         xlim = c(0,1)) +
    # a white background
    draw_grob(canvas,
              x = 0, y = 1,
              height = 9, width = 16,
              hjust = 0, vjust = 1) +
    # national-level plot
    draw_plot(plot_left+theme(legend.position = 'none'),
              x = plot_margin*2,
              y = 0.25,
              height = 0.45 ,
              width = 0.3-plot_margin*2) +
    # state tiles
    draw_plot(plot_right+theme(text = element_text(family = font_legend, color = text_color)),
              x = 1,
              y = 0+plot_margin*2,
              height = 1- plot_margin*4, 
              width = 1-(0.3+plot_margin*3),
              hjust = 1,
              vjust = 0) +
    # add legend
    draw_plot(plot_legend,
              x = plot_margin*2,
              y = 0.1,
              height = 0.13 ,
              width = 0.3-plot_margin) +
    # draw title
    # draw_label(sprintf('%s %s', plot_month, plot_year),
    #            x = plot_margin*2, y = 1-plot_margin*4, 
    #            size = 42, 
    #            hjust = 0, 
    #            vjust = 1,
    #            fontfamily = font_legend,
    #            color = text_color,
    #            lineheight = 1)  +
    # stylized streamflow title
    draw_image(title_flow,
               x = -plot_margin*12,
               y = 1-(6*plot_margin),
               height = 0.2,
               width = 1.1,
               hjust = 0,
               vjust = 1) +
    # stylized streamflow title
    draw_label(year,
               x = plot_margin*4.5,
               y = 1-(10*plot_margin),
               hjust = 0,
               vjust = 1,
               size = 28,
               fontfamily = font_legend,
               color = "white") +
    # percentile info
    draw_label("Groundwater level percentile at observation wells\nrelative to the historic record.", 
               x = plot_margin*2,
               y = 0.25,
               hjust = 0,
               vjust = 1,
               fontfamily = font_legend,
               color = text_color) +
    # add data source
    draw_label("Data: Geological Surveys of the Federal States of Germany\nIdea and Code: Adapted from USGS Data Science", 
               x = 1-plot_margin*20, y = plot_margin*2, 
               fontface = "italic", 
               size = 14, 
               hjust = 0, vjust = 0,
               fontfamily = font_legend,
               color = text_color,
               lineheight = 1.1)
    # add logo
    # draw_image(usgs_logo, x = plot_margin*2, y = plot_margin*2, width = 0.1, hjust = 0, vjust = 0, halign = 0, valign = 0)
  
  # Save and convert file
  ggsave(file_svg |> str_c(".svg"), width = width, height = height, dpi = 300)
  ggsave(file_svg |> str_c(".pdf"), width = width, height = height, dpi = 300)
  return(file_svg)
  
}


