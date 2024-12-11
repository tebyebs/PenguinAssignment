plot_boxplot <- function(data, 
                         x_column, 
                         y_column, 
                         x_label, 
                         y_label, 
                         colour_mapping = NULL) {
  
  # First remove NA values
  data <- data %>%
    drop_na({{ y_column }})
  
  # Now make the plot
  ggplot(data = data, 
         aes(
           x = {{ x_column }}, 
           y = {{ y_column }}, 
           color = {{ x_column }})) +  # Use {{ }} for x and y columns
    geom_boxplot(
      width = 0.3, 
      show.legend = FALSE) +
    geom_jitter(
      alpha = 0.3,
      size = 1,
      show.legend = FALSE,
      position = position_jitter(width = 0.2, seed = 0)) +
    scale_color_manual(
      values = colour_mapping) +  # Use color_mapping input here
    labs(
      x = x_label, 
      y = y_label) +  # Use provided x and y labels
    theme_bw()
}

#plot scatterplot
plot_scatter <- function(data, 
                         x_axis, 
                         y_axis, 
                         x_label, 
                         y_label,
                         Species,
                         colour_mapping) {
  
  # Now make the plot
  ggplot(data = data,
         aes(x = {{x_axis}},
             y = {{y_axis}},
             color = {{Species}}, 
             shape = {{Species}})) +
    geom_point(size = 2, alpha = 0.8) +
    labs(x = x_label,
         y = y_label) +
    scale_color_manual(values = colour_mapping) +
    theme_light() +
    theme(legend.position = "bottom")
}


#saves pngs
save_plot_png <- function(plot,
                          filename,
                          size,
                          res,
                          scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  print(plot)
  dev.off()
}

#saves svgs
save_plot_svg <- function(plot,
                          filename,
                          size,
                          scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  print(plot)
  dev.off()
}
