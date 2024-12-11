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
                         colour_factor) {
  
  # Now make the plot
  ggplot(data = data, 
         aes(
           x =  x_axis, 
           y =  y_axis, 
           color =  colour_factor,
           shape = colour_factor)) +  # Use {{ }} for x and y columns
    geom_point(
      size = 2,
      alpha = 0.8,
      show.legend = FALSE) +
    labs(
      x = x_label, 
      y = y_label) +  # Use provided x and y labels
    theme_light() + 
    theme(legend.position = "bottom")
}


#plots pngs
save_flipper_plot_png <- function(boxplot, 
                                  filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  print(boxplot)
  dev.off()
}

#plots svgs
save_flipper_plot_svg <- function(boxplot, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  print(boxplot)
  dev.off()
}
