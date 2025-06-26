# This function finds returns a ggplot of summary standard data

# TEHuth 4/25/2020

# Inputs
# the data you want to plot (should be individual data type)
# data MUST have x, y, and z columns. Does not handle datetime values
ggplot.maker.loop <- function(dataset,x.label,y.label,title.label) {
  plot.OUT <- ggplot() + geom_point(
    data = dataset,
    aes(x = dataset$x,
        y = dataset$y,
        color = dataset$z),
    size = 3) +
    labs(x = x.label,
         y = y.label,
         title = title.label) +
    theme(legend.position = "none")
  return(plot.OUT)
}