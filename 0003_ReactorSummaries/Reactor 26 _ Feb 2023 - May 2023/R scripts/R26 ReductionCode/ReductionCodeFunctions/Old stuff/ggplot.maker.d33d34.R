# This function finds returns a ggplot so that you can make for loops work
# Only intended for the IPL D17O datasets

# TEHuth 4/21/2020

# Inputs
# the data you want to plot (should be individual data type)
# data MUST have $Date.Time and $D17O.per.meg columns
# Or it MUST have $IPL.num and $D17O.per.meg columns starting in V05 of the code
ggplot.maker.d33d34 <- function(dataset) {
  if (names(dataset)[2] == "d33") {
    plot.OUT <- ggplot() + geom_point(
      data = dataset,
      aes(x = IPL.num,
          y = d33),
      size = 3) + scale_color_manual(values = plot.col.byCorr)+
      xlim(min(dataset$IPL.num), max(dataset$IPL.num))
    } else if (names(dataset)[2] == "d34") {
    plot.OUT <- ggplot() + geom_point(
      data = dataset,
      aes(x = IPL.num,
          y = d34),
      size = 3) + scale_color_manual(values = plot.col.byCorr) +
      xlim(min(dataset$IPL.num), max(dataset$IPL.num))
  }
  # Add axis labels
  plot.OUT <- plot.OUT + 
    labs(x = "IPL #",
         y = paste(names(dataset)[2], " for SMOW (per mil)", sep=""),
         title = paste(plot.col.byCorr.names[r], " correction" ,sep=""))
  return(plot.OUT)
}

# Code from V04 and lower (uses Date.Time instead of IPL.num for plots)
# ggplot.maker.d33d34 <- function(dataset) {
#   if (names(dataset)[2] == "d33") {
#     plot.OUT <- ggplot() + geom_point(
#       data = dataset,
#       aes(x = ymd_hms(dataset$Date.Time,truncated=3),
#           y = dataset$d33),
#       size = 3) + scale_color_manual(values = plot.col.byCorr) +
#       scale_x_datetime(date_labels = "%b/%Y") # month/year for x axis
#   } else if (names(dataset)[2] == "d34") {
#     plot.OUT <- ggplot() + geom_point(
#       data = dataset,
#       aes(x = ymd_hms(dataset$Date.Time,truncated=3),
#           y = dataset$d34),
#       size = 3) + scale_color_manual(values = plot.col.byCorr) +
#       scale_x_datetime(date_labels = "%b/%Y") # month/year for x axis
#   }
#   # Add axis labels
#   plot.OUT <- plot.OUT + 
#     labs(x = "Date",
#          y = paste(names(dataset)[2], " for SMOW (per mil)", sep=""),
#          title = paste(plot.col.byCorr.names[r], " correction" ,sep=""))
#   return(plot.OUT)
# }