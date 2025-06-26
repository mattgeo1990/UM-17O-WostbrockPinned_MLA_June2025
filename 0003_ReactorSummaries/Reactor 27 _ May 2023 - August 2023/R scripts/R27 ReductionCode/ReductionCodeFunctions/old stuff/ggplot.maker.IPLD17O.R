# This function finds returns a ggplot so that you can make for loops work hopefully
# Only intended for the IPL D17O datasets

# TEHuth 2/19/2020

# Inputs
# the data you want to plot (should be individual data type)
# data MUST have $Date.Time and $D17O.SMOWSLAP.per.meg.carbNorm columns
# Or it MUST have $IPL.num and $D17O.SMOWSLAP.per.meg.carbNorm columns starting in V05 of the code
ggplot.maker.IPLD17O <- function(dataset) {
    plot.OUT <- ggplot() + geom_point(
      data = dataset,
      aes(x = IPL.num,
          y = D17O.SMOWSLAP.per.meg.carbNorm),
      size = 3) + scale_color_manual(values = plot.col.byCorr)
  
    # Add axis labels
    plot.OUT <- plot.OUT + 
      labs(x = "IPL #",
           y = bquote(~ Delta ^ 17 ~ "O of SMOW (per meg)"),
           title = paste(plot.col.byCorr.names[r], " correction" ,sep=""))
    # title = paste(plot.col.byCorr.names[r], " correction" ,sep=""))
    
  return(plot.OUT)
}


# Code from V04 and lower (uses Date.Time instead of IPL.num for plots)
# ggplot.maker.IPLD17O <- function(dataset) {
#   plot.OUT <- ggplot() + geom_point(
#     data = dataset,
#     aes(x = ymd_hms(dataset$Date.Time),
#         y = dataset$D17O.SMOWSLAP.per.meg.carbNorm),
#     size = 3) + scale_color_manual(values = plot.col.byCorr) +
#     scale_x_datetime(date_labels = "%b/%Y") # month/year for x axis
#   
#   # Add axis labels
#   plot.OUT <- plot.OUT + 
#     labs(x = "Date",
#          y = bquote(~ Delta ^ 17 ~ "O for SMOW (per meg)"),
#          title = paste(plot.col.byCorr.names[r], " correction" ,sep=""))
#   
#   return(plot.OUT)
# }