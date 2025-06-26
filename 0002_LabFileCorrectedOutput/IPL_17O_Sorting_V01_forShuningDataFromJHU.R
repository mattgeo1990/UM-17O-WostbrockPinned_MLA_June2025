# IPL_17O_Sorting
# Sorts the corrected data files into a list of samples and list of sample averages

#packages
library(ggplot2)

# Clear the workspace and plots
rm(list = ls())
if (!is.null(dev.list()))
  dev.off()

# Do you want to sort the JHU or UM data?
JHUorUM <- 2 # 1 for JHU, 2 for UM

######### data file locations setup ###########################################
# Data reduction folder data path. This address should lead to "Data Reduction Procedure." No final "/" is needed.
# If the auto-finding function doesn't work, here is an example way to statically specify the address
# path.data.red <- "D:/Documents/000_Michigan/Laboratory Data Files/Data Reduction Procedure"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path.data.red <- dirname(getwd())

# importing path: corrected reactor data files
path.in <-
  paste(path.data.red,"/","0002_LabFileCorrectedOutput","/",sep="")

# Output path: path for global and average analyses lists
path.out <-
  paste(path.data.red,"/",sep="")

# Define path to the functions needed for this program
path.to.functions <-
  paste(path.data.red,"/","0001_ReductionCode/ReductionCodeFunctions","/",sep="")
# Function sources
source(paste(path.to.functions, "ggplot.maker.loop.R", sep = ""))
source(paste(path.to.functions, "multiplot.R", sep = ""))

############################################################################################
# Import files
############################################################################################
#import the post-processing input file, which contains information about which of the correction schemes is preferred for each reactor.
setwd(path.in)
# Which data file to import for the post-processing informations?
if (JHUorUM == 1){ # sort the JHU data
  data.post <- paste("postProcessInputJHU.csv", sep = "")
}else{ # sort the UM data
  data.post <- paste("postProcessInput_forShuningDataFromJHU.csv", sep = "")
}
post.inp <- read.csv(data.post, stringsAsFactors = FALSE, header = TRUE)

############################################################################################
# Make a list of all samples ever run (compile everything)
############################################################################################
# Import all reactor data corrected by the preferred scheme. Note that this requires user input in "postProcessInput.csv" to force users to look at the data and not blindly accept the auto-selected correction scheme.
count <- 0
file.verify <- data.frame(matrix(NA, nrow = 10000, ncol = 1)) # data.frame to verify the right files are pulled in
names(file.verify) <- "files.pulled"
for (k in 1:nrow(post.inp)){ # each row corresponds to a reactor
  curFileNum <- post.inp$reactor.file.number[k] # set the file number by the spreadsheet
  if (post.inp$prefer.user[k]!=-9999){ # no correction is possible for this reactor - no normalized data exists
    if (post.inp$prefer.user[k]==1){ # basic correction preferred for this reactor
      count <- count+1
      data.file2 <- paste("R",curFileNum,"_corData_basic.csv", sep = "")
      file.verify[[1]][count] <- data.file2
      # Add data to the overall list
      if (count==1){
        data <- read.csv(data.file2,stringsAsFactors = FALSE, header = TRUE)
      } else {
        hold <- read.csv(data.file2,stringsAsFactors = FALSE, header = TRUE)
        data <- rbind(data,hold)
      }
      
    } else if (post.inp$prefer.user[k]==2){ # linear correction preferred for this reactor
      count <- count+1
      data.file2 <- paste("R",curFileNum,"_corData_linear.csv", sep = "")
      file.verify[[1]][count] <- data.file2
      # Add data to the overall list
      if (count==1){
        data <- read.csv(data.file2,stringsAsFactors = FALSE, header = TRUE)
      } else {
        hold <- read.csv(data.file2,stringsAsFactors = FALSE, header = TRUE)
        data <- rbind(data,hold)
      }
      
    } else if (post.inp$prefer.user[k]==3){ # segmented correction preferred for this reactor
      # loop through the segments and get the preferred data for each segment
      numSeg <- post.inp$numSeg[k]
      for (kk in 1:numSeg){
        count <- count+1
        cur.prefer.user.seg <- post.inp[[paste("prefer.user.seg",kk,sep="")]][k]
        if (cur.prefer.user.seg==1){ # basic correction preferred for this segment
          data.file2 <- paste("R",k,"_corData_seg",kk,"basic.csv", sep = "")
        } else if (cur.prefer.user.seg==2){ # linear correction preferred for this reactor
          data.file2 <- paste("R",k,"_corData_seg",kk,"linear.csv", sep = "")
        }
        file.verify[[1]][count] <- data.file2
        # Add data to the overall list
        if (count==1){
          data <- read.csv(data.file2,stringsAsFactors = FALSE, header = TRUE)
        } else {
          hold <- read.csv(data.file2,stringsAsFactors = FALSE, header = TRUE)
          data <- rbind(data,hold)
        }
      }
    }
  }
}
file.verify <- subset(file.verify,!is.na(files.pulled))

############################################################################################
# Make a list of sample averages
############################################################################################
# Make a list of unique sample names and find the average information for samples. Desired output is Type.1, Type.2, sample.ID, isotopes
name.list <- unique(data$sample.ID)
names.data.avg = c("Type.1",
                   "Type.2",
                   "sample.ID",
                   "n",
                   "avg.dp17O",
                   "avg.dp18O",
                   "avg.D17O",
                   "sd.dp17O",
                   "sd.dp18O",
                   "sd.D17O")
data.avg <- data.frame(matrix(nrow = length(name.list), ncol = length(names.data.avg)))
names(data.avg) = names.data.avg

# Average globally
for (k in 1:length(name.list)){
  # Find relevant rows for the current sample.ID
  curData <- subset(data,sample.ID==name.list[k])
  # Only use rows with complete D17O correction (i.e., including tertiary normalization). Note that all waters are automatically accepted.
  curData <- curData[!is.na(curData$D17O.SMOWSLAP.per.meg.carbNorm),]
  
  # number of replicates
  n <- nrow(curData)
  
  # average values
  avg.dp17O <- mean(curData$dp17O.SMOWSLAP.CO2norm)
  avg.dp18O <- mean(curData$dp18O.SMOWSLAP.CO2norm)
  avg.D17O <- mean(curData$D17O.SMOWSLAP.per.meg.carbNorm)
  
  # standard deviation
  sd.dp17O <- sd(curData$dp17O.SMOWSLAP.CO2norm)
  sd.dp18O <- sd(curData$dp18O.SMOWSLAP.CO2norm)
  sd.D17O <- sd(curData$D17O.SMOWSLAP.per.meg.carbNorm)
  
  data.avg[k,] <-c(curData$Type.1[1], curData$Type.2[1], curData$sample.ID[1], n, avg.dp17O, avg.dp18O, avg.D17O, sd.dp17O, sd.dp18O, sd.D17O)
}

############################################################################################
# Make a list of standards by reactor
############################################################################################
# Make a list of all the standard data
data.std <-subset(data,Type.1=="WaterStd" | Type.1 == "CarbonateStd" | Type.1 == "PhosphateStd")
# Make a list of unique standards names
names.std <- sort(unique(data.std$Type.2))

# Make a storage list for the standard data. This includes information for each reactor and all-time. Each data.frame has information for one of the following variables:
# n - number of replicates
# d'17O (per mil)
# d'17O sd (per mil)
# d'18O (per mil)
# d'18O sd (per mil)
# D17O (per meg)
# D17O sd (per meg)

# Names for list
sum.std.listnames <- c("n", "dp17O", "dp17O.sd", "dp18O", "dp18O.sd", "D17O", "D17O.sd")
# Empty list of appropriate size
sum.std <- vector("list", length(sum.std.listnames))
# Name the data.frames in the list
names(sum.std) <- sum.std.listnames

# Set up matrix for naming data.frame columns (rows are just standard names)
sum.std.names.col <- matrix(NA, nrow = 1, ncol = nrow(post.inp))
for (k in 1:(nrow(post.inp)+1)){ # name columns by reactor number, add one column for all-time values
  curRnum <- post.inp$reactor.file.number[k] # set the file number by the spreadsheet
  sum.std.names.col[k] <- paste("R",curRnum,sep="")
  if (k == nrow(post.inp)+1){ # last column is all-time
    sum.std.names.col[k] <- "all.time"
  }
}

# Initialize empty, named data.frames in the list
for (k in 1:length(sum.std)){
  curRnum <- post.inp$reactor.file.number[k] # set the file number by the spreadsheet
  sum.std[[sum.std.listnames[k]]] <- data.frame(matrix(NA, nrow = length(names.std), ncol = nrow(post.inp)+1))
  colnames(sum.std[[sum.std.listnames[k]]]) <- sum.std.names.col
  rownames(sum.std[[sum.std.listnames[k]]]) <- names.std
}

# Fill the lists with desired information
for (k in 1:length(names.std)){ # go through each standard separately
  for (kk in 1:length(sum.std.names.col)){ # go through each reactor separately
    curRnum <- post.inp$reactor.file.number[kk] # set the file number by the spreadsheet
    if (kk<length(sum.std.names.col)){ # individual reactor summaries
      # Get all data from this reactor
      curData <- subset(data.std,reactor.ID==curRnum & sample.ID==names.std[k])
      # Only use rows with complete D17O correction (i.e., including tertiary normalization). Note that all waters are automatically accepted.
      curData <- curData[!is.na(curData$D17O.SMOWSLAP.per.meg.carbNorm),]
      # Get desired data and record in the list
      sum.std[[sum.std.listnames[1]]][k,kk] <- nrow(curData) # n - number of replicates
      sum.std[[sum.std.listnames[2]]][k,kk] <- mean(curData$dp17O.SMOWSLAP.CO2norm)
      sum.std[[sum.std.listnames[3]]][k,kk] <- sd(curData$dp17O.SMOWSLAP.CO2norm)
      sum.std[[sum.std.listnames[4]]][k,kk] <- mean(curData$dp18O.SMOWSLAP.CO2norm)
      sum.std[[sum.std.listnames[5]]][k,kk] <- sd(curData$dp18O.SMOWSLAP.CO2norm)
      sum.std[[sum.std.listnames[6]]][k,kk] <- mean(curData$D17O.SMOWSLAP.per.meg.carbNorm)
      sum.std[[sum.std.listnames[7]]][k,kk] <- sd(curData$D17O.SMOWSLAP.per.meg.carbNorm)
    } else { # all-time summary
      # Get all-time data
      curData <- subset(data,sample.ID==names.std[k]) 
      # all reactor.ID values are accepted
      # Only use rows with complete D17O correction (i.e., including tertiary normalization). Note that all waters are automatically accepted.
      curData <- curData[!is.na(curData$D17O.SMOWSLAP.per.meg.carbNorm),]
      # Get desired data and record in the list
      sum.std[[sum.std.listnames[1]]][k,kk] <- nrow(curData) # n - number of replicates
      sum.std[[sum.std.listnames[2]]][k,kk] <- mean(curData$dp17O.SMOWSLAP.CO2norm)
      sum.std[[sum.std.listnames[3]]][k,kk] <- sd(curData$dp17O.SMOWSLAP.CO2norm)
      sum.std[[sum.std.listnames[4]]][k,kk] <- mean(curData$dp18O.SMOWSLAP.CO2norm)
      sum.std[[sum.std.listnames[5]]][k,kk] <- sd(curData$dp18O.SMOWSLAP.CO2norm)
      sum.std[[sum.std.listnames[6]]][k,kk] <- mean(curData$D17O.SMOWSLAP.per.meg.carbNorm)
      sum.std[[sum.std.listnames[7]]][k,kk] <- sd(curData$D17O.SMOWSLAP.per.meg.carbNorm)
    }
  }
}

############################################################################################
# Plot the standard data summary
############################################################################################
# Variables to plot as residuals compared to the all-time average.
flag.resid = c("dp17O", "dp18O", "D17O")

# for (k in 1:length(sum.std)){ # each summary variable gets its own plot
for (k in c(6)){ # each summary variable gets its own plot
  plot.list.std = list() # empty plot list each loop
  x.count <- 1 # counter to decide when to plot x label
  y.count <- 1 # counter to decide when to plot y label
  for (kk in 1:length(names.std)){ # each standard gets its own subplot
    # Make a list of relevant data, coordinated by reactor.
    # 1 - reactor number
    # 2 - data
    # 3 - data type (unused)
    x <- 1:(nrow(post.inp)+1) # reactor numbers + 1 extra for all-time value
    y <- as.numeric(sum.std[[sum.std.listnames[k]]][kk,]) # current data
    z <- x*0 # arbitrary value for individual reactors
    z[length(z)] <- 1 # arbitrary value to make the all-time value distinct
    dataset <- data.frame(x,y,z)
    if (x.count == 4){ # bottom row gets x-axis label
      x.label <- "reactor"
      x.count <- 1
    } else {
      x.label <- ""
      x.count <- x.count+1
    }
    if (y.count < 5){
      y.label <- sum.std.listnames[k]
      y.count <- y.count+1
    } else {
      y.label <- ""
      y.count <- y.count+1
    }
    title.label <- names.std[kk]
    plot.list.std[[kk]] <- ggplot.maker.loop(dataset,x.label,y.label,title.label)
  }
  # plot up the current variable
  multiplot(plotlist = plot.list.std, cols = 6)
}

############################################################################################
# Output files
############################################################################################
# Output the final data and average data lists
if (JHUorUM == 1){ # output as JHU data
  write.csv(data, "cor.data.allJHU.csv", row.names = FALSE)
  write.csv(data.avg, "cor.data.all.avgJHU.csv", row.names = FALSE)
}else{ # output as UM data
  write.csv(data, "cor.data.all_forShuningDataFromJHU.csv", row.names = FALSE)
  write.csv(data.avg, "cor.data.all.avg.forShuningDataFromJHUcsv", row.names = FALSE)
}

