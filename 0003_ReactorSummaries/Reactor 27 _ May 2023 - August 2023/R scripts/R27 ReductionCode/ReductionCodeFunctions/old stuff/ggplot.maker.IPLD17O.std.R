# This function finds returns a ggplot so that you can make for loops work hopefully
# Only intended for the IPL D17O datasets

# TEHuth 2/19/2020

# Inputs
# the data you want to plot (should be individual data type)
# data MUST have $Date.Time and $D17O.per.meg columns
# Or it MUST have $IPL.num and $D17O.per.meg columns starting in V05 of the code
ggplot.maker.IPLD17O.std <- function(dataset) {
    plot.OUT <- ggplot() + geom_point(
      data = dataset,
      aes(x = IPL.num,
          y = residual,
            color = Type.2),
      size = 3) + scale_color_manual(values = plotting.colors.byStd)
    labs(x = "IPL #",
           y = bquote(~ Delta ^ 17 ~ "O residual (obs-exp, per meg) "),
         title = paste(plot.col.byCorr.names[r], " correction" ,sep="")) +
    theme(legend.title = element_blank())
  return(plot.OUT)
}

# Code from V04 and lower (uses Date.Time instead of IPL.num for plots)
# ggplot.maker.IPLD17O.std <- function(dataset) {
#   plot.OUT <- ggplot() + geom_point(
#     data = dataset,
#     aes(x = ymd_hms(dataset$Date.Time),
#         y = dataset$residual,
#         color = dataset$Type.2),
#     size = 3) + scale_color_manual(values = plotting.colors.byStd) +
#     scale_x_datetime(date_labels = "%b/%Y") + # month/year for x axis
#     labs(x = "Date",
#          y = bquote(~ Delta ^ 17 ~ "O residual (obs-exp, per meg) "),
#          title = paste(plot.col.byCorr.names[r], " correction" ,sep="")) +
#     theme(legend.title = element_blank())
#   return(plot.OUT)
# }