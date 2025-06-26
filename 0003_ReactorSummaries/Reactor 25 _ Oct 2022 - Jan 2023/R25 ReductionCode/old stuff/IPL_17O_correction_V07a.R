# Notes on fixes:
# 2020-12-16
# Fixed the order of the secondary normalizations. Carbonate D'17O is first normalized to 102-GC-AZ01. Then the secondary d'18O and d'17O normalizations are done. This keeps the d'17O internally consistent with the reported D'17O values.
# Fixed an if statement in the secondary d'18O and d'17O normalization section. It would proceed as if it could do the normalization even if there were not samples. Issue was that the entering if statement used the total number of unique carbonates run to proceed (instead of the total number of unique carbonates for which we have BOTH d18O(O2) and d18O(CO2) information - you need at least two unique samples with both analyses)
# Updated the list of known d18O(CO2)
# Added three columns to the output file that tell you which secondary corrections were applied to d'17O, d'18O, and D'17O, if any
# Added a column to let you know what segment of a reactor the sample was analyzed in

# 2021-02-24
# Changed program to an R Script
# Changed the output column names to be more readily identifiable
# a.	The names now have qualifiers to tell you what corrections have been applied (if it is possible, note that these corrections never happen for waters)
# i.	SMOWSLAP – has SMOW-SLAP correction (basic, linear, and segmented are not distinguished, but these are in the lists)
# ii.	CO2norm – the d18O-CO2 based normalization has been applied for d17O and d18O, if any
# iii.	carbNorm – the carbonate normalization has been applied for D’17O
# b.	Three columns at the end confirm what normalization have been applied to the columns that we draw data from. (d17O.SMOW.SLAP.CO2norm,  d17O.SMOW.SLAP.CO2norm, and D17O.SMOWSLAP.per.meg.carbNorm)

# Also got rid of the "R#dataFinal.csv" and "R#_dataAutoFinal..." files. These 
# have been superceded by the postprocessinput.csv system, where the user must 
# select which correction (basic, linear, segmented-basic, or segmented-linear)
# is most appropriate for each reactor and segment 

# 2021-04-13
# Code now picks out Phosphate and PhosphateStd as categories. It does NOT perform a secondary normalization for them (i.e., as is done for carbonates) because we have not defined long-term values yet

# 2021-06-17
# Changed plots to show samples by sample #, not by date

# 2021-06-24
# Added functionality to normalize D'17O values to the Wostbrock et al. 2020 value for IAEA-603(and IAEA-C1) as calcite (not as CO2)

## 2021-10-11
# V07. Added cosmetic changes to graphs (meaningful labels).

### Nu Data O2 data normalization
# This is the second of two scripts to analyze Nu data text files and calculate $\delta$^18^O, $\delta$^17^O, and $\Delta$^17^O.

#### Setup
# Clear plots
if (!is.null(dev.list()))
  dev.off()

# ```{r,warning=FALSE,message=FALSE}
#packages
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(ggpubr)
library(lubridate)
library(knitr)
library(kableExtra)
library(stringr)
library(readxl)

# Update reactor names with each new reactor
reactor.file.names <-
  c("01",
    "02",
    "03",
    "04",
    "05",
    "06",
    "07",
    "08",
    "09",
    "10",
    "11",
    "12",
    "13",
    "14",
    "15",
    "16",
    "17",
    "18")

reactor.file.names.JHU <-
  c("510-607",
    "666-679",
    "680-847",
    "848-880",
    "881-980",
    "981-1103",
    "1104-1208",
    "1209-1301",
    "1302-1332",
    "1333-1459",
    "1461-1531",
    "1534-1540",
    "1543-1584",
    "1586-1671",
    "1673-1768",
    "1769-1793",
    "1793-1862",
    "1863-1906",
    "1944-2041",
    "2042-2115",
    "2755-2806",
    "2116-2234",
    "2235-2307",
    "2308-2425",
    "2426-2496",
    "2495a-2618",
    "2619-2671",
    "2673-2754",
    "2807-2854")

# Data reduction folder data path. This address should lead to "Data Reduction Procedure." No final "/" is needed.
# If the auto-finding function doesn't work, here is an example way to statically specify the address
# path.data.red <- "D:/Documents/000_Michigan/Laboratory Data Files/Data Reduction Procedure"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path.data.red <- dirname(getwd())

# Option for tertiary correction
# 1 - use 'best' available standard set (see section below)
# 2 - Always use 102-GC-AZ01 (keeps standard normalization constant between reactors)
# 3 - Always use the calcite values for IAEA-603 and/or IAEA-C1 (keeps standard normalization constant between reactors). This is consistent with Wostbrock et al. (2020) and assumes that IAEA-C1 is the same material as IAEA-603. Note that this will make the carbonate residuals off by ~47 per meg because the residual values have been specified as CO2, not O2 values.
# 4 - Wostbrock-normalization (7/1/2021). Uses (1) defined 18alpha(m-O2, 90C) and IPL-long term-based 17alpha(m-O2, 90C) followed by (2) temporal drift correction for D'17O.
tert.cor.option <- 4

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################

# Are you looking to normalize a JHU or UM file?
JHUorUM <- 2 # 1 for JHU, 2 for UM

for (reactor.file.number in 10) { # can be used for either UM or JHU files
# for(reactor.file.number in 1:length(reactor.file.names)) { # for UM files
  # for(reactor.file.number in 1:length(reactor.file.names.JHU)) { # for JHU files
  
  print(paste("reducing Reactor ", reactor.file.number,sep=""))
  # Clear before each reactor reduction - to keep different reactors from interacting
  rm(list = ls()[!ls() %in% c("reactor.file.names", "reactor.file.number",
                              "path.data.red","tert.cor.option","reactor.file.names.JHU","JHUorUM")])
  
  ######### data file locations setup ###########################################
  # importing path
  path.in <- paste(path.data.red,"/","0000_LabFileFormatting/001_Reactor Spreadsheet Formatted","/",sep="")
  
  # Output path
  path.out <- paste(path.data.red,"/","0002_LabFileCorrectedOutput","/",sep="")
  
  # define path to the accepted standard values spreadsheet
  path.to.accept.std <-
    paste(path.data.red,"/","0001_ReductionCode","/",sep="")
  accepted.std.file = "acceptedStds.csv"
  
  # define path to the accepted d18O-mineral values spreadsheet (traditional d18O(CO2/CaCO3) measurements used for secondary normalization, see Passey 2014)
  path.to.trad.master <-
    paste(path.data.red,"/","0001_ReductionCode","/",sep="")
  # Name of the spreadsheet
  trad.master.file = "d18O_secondarycorrection.xlsx"
  
  # Define path to the functions needed for this program
  path.to.functions <-
    paste(path.data.red,"/","0001_ReductionCode/ReductionCodeFunctions","/",sep="")
  # Function sources
  source(paste(path.to.functions, "correct.SMOW.SLAP.basic.R", sep = ""))
  source(paste(path.to.functions, "correct.SMOW.SLAP.linearSMOW.R", sep = ""))
  source(paste(path.to.functions, "multiplot.R", sep = ""))
  source(paste(path.to.functions, "ggplot.maker.IPLD17O.R", sep = ""))
  source(paste(path.to.functions, "ggplot.maker.IPLD17O.all.R", sep = ""))
  source(paste(path.to.functions, "ggplot.maker.IPLD17O.std.R", sep = ""))
  source(paste(path.to.functions, "segment.finder.R", sep = ""))
  source(paste(path.to.functions, "ggplot.maker.d33d34.R", sep = ""))
  
  # Parameters to automatically decide which correction scheme to use for the data. The program starts by assuming the basic SMOW-SLAP correction, then does a linear correction above a certain r-sqared value in SMOW data, then does a segmented correction (with basic and linear corrections decided within each section) if assigned in the lab spreadsheet (flag.major).
  threshold.rsq = 0.00 # R-squared threshold to use a linear correction for data reduction instead of basic corection. Zero value always prefers linear correction.
  threshold.singlePrime <-
    5 # The per mil difference between samples beyond which at least a single prime is required (or else data is flagged out)
  threshold.doublePrime <-
    10 # The per mil difference between samples beyond which at least two primes are required (or else data is flagged out)
  
  
  ######### Start reduction ###########################################
  
  ############################################################################################
  # Import files
  ############################################################################################
  #import data
  setwd(path.in)
  # Which data file to import?
  if (JHUorUM == 1){ # setup to read in a JHU file
    data.file <-
      paste("JHUsession_", reactor.file.names.JHU[reactor.file.number], "Final.csv", sep =
              "")
  }else{ # setup to read in a UM file
    data.file <-
      paste("Reactor", reactor.file.names[reactor.file.number], "Final.csv", sep =
              "")
  }
  compiled.nu.data_0 <-
    read.csv(data.file, stringsAsFactors = FALSE, header = TRUE)
  
  # Import the accepted standard values
  std.accepted <-
    read.csv(
      paste(path.to.accept.std, accepted.std.file, sep = ""),
      stringsAsFactors = FALSE,
      header = TRUE
    )
  
  # Import the master list of d18O-mineral values
  trad.master <-
    read_excel(paste(path.to.trad.master, trad.master.file, sep = ""),
               skip = 0)
  
  ############################################################################################
  # Initial data formatting (flagged analyses, segmented reactors)
  ############################################################################################
  
  # Change IPL.num from character to numeric
  compiled.nu.data_0$IPL.num <- as.numeric(compiled.nu.data_0$IPL.num)
  
  # Create the overall flag column, which combines flags from flag.analysis and from samples that were not primed correctly. These flagged analyses cannot be trusted and are removed from the dataset.
  compiled.nu.data <- subset(compiled.nu.data_0, group.num > 0 & !is.na(d.18O.prelim))
  compiled.nu.data$test.d.18O.prelim <-
    compiled.nu.data$flag.analysis * 0 - 9999 # some fill values for the test.d.18O.prelim column
  compiled.nu.data$flag.analysis[is.na(compiled.nu.data$flag.analysis)] <-
    0 # NA values mean no flag
  compiled.nu.data$flag.d18O <-
    compiled.nu.data$flag.analysis * 0 + 1 # default to reject (ones) for start
  compiled.nu.data$primes[is.na(compiled.nu.data$primes)] <-
    0 # NA values mean no prime
  
  for (dk in 1:nrow(compiled.nu.data)) {
    # calculate the test value
    if (dk == 1) {
      # first row, nothing to test against
      compiled.nu.data$test.d.18O.prelim[dk] = 6 # this value forces code to always require at least one prime for the first analysis
    } else {
      compiled.nu.data$test.d.18O.prelim[dk] = abs(compiled.nu.data$d.18O.prelim[dk] -
                                                     compiled.nu.data$d.18O.prelim[dk - 1])
    }
    if (compiled.nu.data$test.d.18O.prelim[dk] < threshold.singlePrime) {
      # no check required, sample jump is small
      compiled.nu.data$flag.d18O[dk] <- 0 # accept the row
    } else if (compiled.nu.data$test.d.18O.prelim[dk] > threshold.singlePrime &
               compiled.nu.data$test.d.18O.prelim[dk] < threshold.doublePrime) {
      #  check required, sample jump large enough to require a single prime
      if (compiled.nu.data$primes[dk] > 0) {
        # check passed
        compiled.nu.data$flag.d18O[dk] <- 0 # accept the row
      }
    } else if (compiled.nu.data$test.d.18O.prelim[dk] > threshold.doublePrime) {
      #  check required, sample jump large enough to require a double prime
      if (compiled.nu.data$primes[dk] > 1) {
        # check passed
        compiled.nu.data$flag.d18O[dk] <- 0 # accept the row
      } else {
        compiled.nu.data$flag.d18O[dk] <- 2
      }
    }
  }
  # Combine flag.analytical and flag.d18O
  compiled.nu.data$flag <-
    compiled.nu.data$flag.d18O + compiled.nu.data$flag.analysis
  test.flag <- compiled.nu.data$flag > 0 # was it flagged?
  compiled.nu.data$flag[test.flag] <- 1 # anything flagged gets a 1
  
  
  # Make D17O.data, a subset of the whole spreadsheet. Bad analyses and lines without a group number (run comments) are removed.
  D17O.data <- subset(compiled.nu.data, flag == 0 & group.num > 0)
  
  # If flag.major ~=0, create segment.IPL (controls segmented runs) from the flag.major column. Segments are only created from a user-defined column (flag.major) in the lab spreadsheet. These must be major events - e.g., changing the filament.
  segment.IPL <-
    matrix(-9999, nrow = 1, ncol = 2) # Default value for no segments
  compiled.nu.data_0$flag.major[is.na(compiled.nu.data_0$flag.major)] <-
    0 # NA values mean no flag
  if (sum(compiled.nu.data_0$flag.major) > 0) {
    # if there are any segments
    # Find the IPL number after each break
    flag.major.test2 <-
      which(compiled.nu.data_0$flag.major %in% 1) # find the breaks
    segment.IPL.numList <-
      as.numeric(compiled.nu.data_0$IPL.num[flag.major.test2 + 1]) # start of each new segment
    # add on beginning and end of the reactor
    subset.compiled.nu.data_0 <-
      subset(compiled.nu.data_0, group.num > 0)
    segment.IPL.numList <-
      as.numeric(c(
        min(subset.compiled.nu.data_0$IPL.num),
        segment.IPL.numList,
        max(subset.compiled.nu.data_0$IPL.num)
      ))
    # For each break, set up the min/max IPL.nums
    segment.IPL = matrix(NA,
                         nrow = length(segment.IPL.numList) - 1,
                         ncol = 2)
    # Now fill the matrix
    for (mm in 1:(length(segment.IPL.numList) - 1)) {
      cur.starter <- segment.IPL.numList[mm]
      if (mm < length(segment.IPL.numList) - 1) {
        # this is not the end
        cur.ender <- segment.IPL.numList[mm + 1] - 1
      } else {
        # This is the last segment - end at the last IPL. number
        cur.ender <- segment.IPL.numList[mm + 1]
      }
      segment.IPL[mm, 1:2] <- c(cur.starter, cur.ender)
    }
    # Revise the segment.IPL matrix. Numbers are either matched up to their corresponding IPL number in the flag-cleared data list or are rolled to the next highest number available.
    for (mm in 1:nrow(segment.IPL)) {
      for (mmm in 1:ncol(segment.IPL)) {
        safe.count <- -999 # just a default starting value
        match.IPL = which(as.numeric(D17O.data$IPL.num) == segment.IPL[mm, mmm])
        if (length(match.IPL) < 1) {
          # there is not a match right away
          safe.count <- 0
          # Find next nearest IPL.num above this one and replace in segment.IPL
          while (length(match.IPL) < 1) {
            # there is not a match
            safe.count <- safe.count + 1
            if (mmm == 1) {
              # this is a start segment, count forwards
              segment.IPL[mm, mmm] <- segment.IPL[mm, mmm] + 1
            } else if (mmm == 2) {
              # this is an end segment, count backwards
              segment.IPL[mm, mmm] <- segment.IPL[mm, mmm] - 1
            }
            match.IPL <-
              which(as.numeric(D17O.data$IPL.num) == segment.IPL[mm, mmm])
            if (safe.count > 1000) {
              # If the segment is not real, break this look
              print("error in making segment.IPL")
              break
            }
          }
        } else {
          # there is a match right away
          safe.count <-
            0 # happens often for the end of the run (e.g., last SMOW/SLAP)
        }
        print(
          paste(
            "segment correction: IPL.num match for ",
            segment.IPL[mm, mmm],
            " found in ",
            safe.count,
            " loop(s)",
            sep = ""
          )
        ) # This is a printout that checks how far the program looked for a match. Used for troubleshooting.
      }
    }
  }
  
  # # Add a column that contains the reactor segment number - useful for making SI Appendices
  D17O.data$reactor.segment <- 1 # There is always at least one segment
  for (k in 1:nrow(segment.IPL)){
    # Test for the data that match the segment and add them
    curRows <- as.numeric(D17O.data$IPL.num)>=segment.IPL[k,1] & as.numeric(D17O.data$IPL.num)<=segment.IPL[k,2]
    D17O.data$reactor.segment[curRows] <- k
  }
  
  
  ############################################################################################
  # Plotting variables and output information
  ############################################################################################
  # Define the reactor prefix prefix (reactor.ID)
  prefix <-
    unique(D17O.data$reactor.ID)[!(unique(D17O.data$reactor.ID) == "")]
  
  #setup colors for plots
  #possible standards
  standards <-
    c(
      "SMOW",
      "SLAP",
      "GISP",
      "USGS45",
      "USGS46",
      "USGS47",
      "USGS48",
      "USGS49",
      "USGS50",
      "102-GC-AZ01",
      "IAEA-C1",
      "GON06-OES",
      "NBS-18",
      "NBS-19",
      "IAEA-603",
      "USGS80",
      "B2207",
      "USGS81",
      "RSP-1"
    )
  
  #non-standard water samples
  unknown.waters <-
    unique(subset(D17O.data, Type.1 == "Water")$Type.2)
  unknown.carbonates <-
    unique(subset(D17O.data, Type.1 == "Carbonate")$Type.2)
  
  plot.col.byCorr <-
    c(
      "black",
      "darkcyan",
      "lightblue",
      "darkgreen",
      "blueviolet",
      "goldenrod",
      "darkorange3",
      "firebrick",
      "orchid",
      rep("grey40", length(unknown.waters)),
      rep("grey80", length(unknown.carbonates))
    )
  # Following section is unused, names are made below as needed.
  # plot.col.byCorr.names = c("basic",
  #                           "linear",
  #                           "seg.1",
  #                           "seg.2",
  #                           "seg.3",
  #                           "seg.4",
  #                           "seg.5",
  #                           "seg.6",
  #                           "seg.7")
  # names(plot.col.byCorr) = plot.col.byCorr.names
  
  plotting.colors.byStd <-
    c(
      "black",
      "darkcyan",
      "lightblue",
      "darkgreen",
      "blueviolet",
      "goldenrod",
      "darkorange3",
      "firebrick",
      "orchid",
      "yellow",
      "blue",
      "green",
      "red",
      "purple",
      "white",
      "aquamarine",
      "aquamarine4",
      "brown",
      "brown1",
      rep("grey40", length(unknown.waters)),
      rep("grey80", length(unknown.carbonates))
    )
  names(plotting.colors.byStd) = c(
    "SMOW",
    "SLAP",
    "GISP",
    "USGS45",
    "USGS46",
    "USGS47",
    "USGS48",
    "USGS49",
    "USGS50",
    "102-GC-AZ01",
    "IAEA-C1",
    "GON06-OES",
    "NBS-18",
    "NBS-19",
    "IAEA-603",
    "USGS80",
    "B2207",
    "USGS81",
    "RSP-1",
    unknown.waters,
    unknown.carbonates
  )
  
  ############################################################################################
  # Begin data reduction (basic, linear, segmented, final). Final=preferred by code in order of basic -> linear -> segmented. Note that threshold values are set at top.
  ############################################################################################
  # ```
  # Input data file = `r data.file`
  # Reactor ID = `r prefix`
  
  ## BASIC - Do basic SMOW-SLAP reduction.
  D17O.data.basic.cor = correct.SMOW.SLAP.basic(D17O.data)
  D17O.final = D17O.data.basic.cor # BASIC correction is always assumed first
  D17O.final.line.stats = NA
  basic.linear = -9999 # This is a default flag that says not to do the segment correction
  cat = 1
  
  ## LINEAR DRIFT - reduction for case where there is a linear drift in the standards. Regression is done on the SMOW analyses.
  D17O.data.linearSMOW.cor.HOLD = correct.SMOW.SLAP.linearSMOW(D17O.data)
  names(D17O.data.linearSMOW.cor.HOLD) <-
    c("D17O.data.linearSMOW.cor",
      "D17O.data.line.stats.linearSMOW")
  list2env(D17O.data.linearSMOW.cor.HOLD, envir = .GlobalEnv)
  if (D17O.data.line.stats.linearSMOW[3, 2] > threshold.rsq &
      D17O.data.line.stats.linearSMOW[3, 3] > threshold.rsq) { # for d33 and d34, respectively
    D17O.final = D17O.data.linearSMOW.cor # if threshold met, switch to the linear correction
    D17O.final.line.stats = D17O.data.line.stats.linearSMOW
  }
  
  ## SEGMENTS - reduction for case where there is a large jump in the SMOW values mid-reactor. Before and after the jump(s) are reduced separately and results are concatenated (i.e., you will get overlapping SMOW, and potentially SLAP, sets).
  # Now run the segment correction if there were identified jumps
  if (segment.IPL[1, 1] != -9999) {
    basic.linear = 1 # This overrides the default setting, identifies segment correction exists
    
    # Run the BASIC and LINEAR reductions for each segment.
    for (k in 1:nrow(segment.IPL)) {
      IPL.num.list.b = as.numeric(D17O.data$IPL.num) # note that this produces a warning message as R will convert all characters (e.g., "the reactor did XYZ") into N/A values. Doesn't seem to affect the indexing though.
      # Note that this formulation deals with some of the weirly implemented IPL.num
      starter <- which(IPL.num.list.b == segment.IPL[k, 1])
      ender <- which(IPL.num.list.b == segment.IPL[k, 2])
      D17O.data.segment = D17O.data[starter:ender, ]
      D17O.data.segment <-
        D17O.data.segment[!is.na(D17O.data.segment$IPL.num),] # delete NA rows - they are not needed.
      
      # correct segments using BASIC correction
      segment.cor.HOLD.basic <-
        correct.SMOW.SLAP.basic(D17O.data.segment)
      if (k == 1) {
        # make the lists on the first run through
        D17O.data.segment.cor.basic <-
          vector(mode = "list", length = nrow(segment.IPL))
      }
      # Now fill the list with the corrected data
      D17O.data.segment.cor.basic[[k]] <- segment.cor.HOLD.basic
      
      # Also correct segments using LINEAR correction
      segment.cor.HOLD.linear <-
        correct.SMOW.SLAP.linearSMOW(D17O.data.segment)
      if (k == 1) {
        # make the lists on the first run through (these contain all corrections done for the data set)
        D17O.data.segment.cor.linear <-
          vector(mode = "list", length = nrow(segment.IPL))
        D17O.data.segment.cor.line.stats.linear <-
          vector(mode = "list", length = nrow(segment.IPL))
      }
      # Now fill the list with the corrected data
      D17O.data.segment.cor.linear[[k]] <-
        segment.cor.HOLD.linear[["name1"]]
      D17O.data.segment.cor.line.stats.linear[[k]] <-
        segment.cor.HOLD.linear[["name2"]]
      
      # Now automatically decide whether to use BASIC or LINEAR correction for each segment. With threshold.rsq=0, this defaults to using the LINEAR correction
      temp.rsq33 <-
        D17O.data.segment.cor.line.stats.linear[[k]][3, 2]
      temp.rsq34 <-
        D17O.data.segment.cor.line.stats.linear[[k]][3, 3]
      if (k == 1) {
        # make the lists on the first run through
        D17O.data.segment.cor.all <-
          vector(mode = "list", length = nrow(segment.IPL))
        D17O.data.segment.cor.line.stats.all <-
          vector(mode = "list", length = nrow(segment.IPL))
      }
      
      if (temp.rsq33 > threshold.rsq &
          temp.rsq34 > threshold.rsq) {
        # did meet LINEAR threshold
        D17O.data.segment.cor.all[[k]] <-
          segment.cor.HOLD.linear[["name1"]]
        D17O.data.segment.cor.line.stats.all[[k]] <-
          segment.cor.HOLD.linear[["name2"]]
      } else {
        # did not meet LINEAR threshold
        D17O.data.segment.cor.all[[k]] <- segment.cor.HOLD.basic
        D17O.data.segment.cor.line.stats.all[[k]] <- NA
      }
    }
    D17O.final = D17O.data.segment.cor.all
    D17O.final.line.stats = D17O.data.segment.cor.line.stats.all
  }
  
  ############################################################################################
  # Put all D17O-corrected data in a single list (primary normalization complete)
  ############################################################################################
  # Set up a list to contain all of the data for easier plotting
  # set up empty list with correct number of spots
  if (basic.linear == -9999) {
    # no segments identified
    data.cor = vector(mode = "list", length = 2)
  } else if (basic.linear >= 1) {
    # Segments were identified
    data.cor = vector(mode = "list", length = 2 + nrow(segment.IPL) * 2) # *2 to keep both the basic and linear segmented corrections
  }
  data.cor[[1]] = D17O.data.basic.cor
  data.cor[[2]] = D17O.data.linearSMOW.cor
  if (basic.linear == 1) {
    # segmented correction exists
    # All of the segment-basic corrected data
    for (k in 3:(2 + nrow(segment.IPL))) {
      data.cor[[k]] = D17O.data.segment.cor.basic[[k - 2]]
    }
    # All of the segment-linear corrected data
    for (k in (2 + nrow(segment.IPL) + 1):length(data.cor)) {
      data.cor[[k]] = D17O.data.segment.cor.linear[[k - (2 + nrow(segment.IPL))]]
    }
  }
  
  # Set up a list to contain all of the line stats for reference
  data.cor.line.stats <-
    vector(mode = "list", length = length(data.cor))
  data.cor.line.stats[[1]] = NA # There are no line stats for the basic correction
  data.cor.line.stats[[2]] = D17O.data.line.stats.linearSMOW
  if (basic.linear == 1) {
    # segmented correction exists
    # All of the segment-basic corrected data
    for (k in 3:(2 + nrow(segment.IPL))) {
      data.cor.line.stats[[k]] = NA # There are no line stats for the basic correction
    }
    # All of the segment-linear corrected data
    for (k in (2 + nrow(segment.IPL) + 1):length(data.cor)) {
      data.cor.line.stats[[k]] = D17O.data.segment.cor.line.stats.linear[[k -
                                                                            (2 + nrow(segment.IPL))]]
    }
  }
  
  
  
  
  ############################################################################################
  # Secondary data normalization setup
  ############################################################################################   
  # Set up the columns needed for the secondary data normalization for d'17O, d'18O and D'17O data
  # The default is that no correction is applied - note this information as well
  for (m in 1:length(data.cor)) {
    # Values
    data.cor[[m]]$dp17O.SMOWSLAP.CO2norm <- data.cor[[m]]$dp17O.SMOWSLAP
    data.cor[[m]]$dp18O.SMOWSLAP.CO2norm <- data.cor[[m]]$dp18O.SMOWSLAP
    data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm <- data.cor[[m]]$D17O.SMOWSLAP.per.meg
    
    # Type of correction applied
    data.cor[[m]]$dp17O.SMOWSLAP.CO2norm.type <- 'no correction'
    data.cor[[m]]$dp18O.SMOWSLAP.CO2norm.type <- 'no correction'
    data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type <- 'no correction'
  }
  
  ############################################################################################
  # Tertiary data normalization for D17O
  ############################################################################################
  # Holding lists
  norm.tert.model <-
    vector(mode = "list", length = length(data.cor)) # holding list for correlations
  norm.tert.model.data <-
    vector(mode = "list", length = length(data.cor)) # holding list for correlation data
  
  # Applies an  correction for D17O due to acid bath-reduction line. Uses Wostbrock et al. (2020) values for NBS-18, NBS-19, and IAEA-603. If these are not available, or have <2 replicates, the correction is applied based on 102-GCAZ-01 (average value = -125 per meg based on all-time average vs. NBS-18 and NBS-19 standards)
  for (m in 1:length(data.cor)){
    # List of standard names - 102-GC-AZ01 must always be listed last
    tert.names.D17O <- c("NBS-18", "NBS-19", "IAEA-603", "102-GC-AZ01")
    
    # Accepted true values of D17O (per meg)
    tert.true.D17O <- matrix(0,4,1)
    tert.true.D17O[1] <- -100 # NBS-18, Wostbrock et al. (2020), as CO2
    tert.true.D17O[2] <- -155 # NBS-19, Wostbrock et al. (2020), as CO2
    tert.true.D17O[3] <- -147 # IAEA-603 Wostbrock et al. (2020), as CO2
    tert.true.D17O[4] <- -114 # 102-GC-AZ01, as CO2, based on Ben's calculations. This value must always be listed last.
    
    # Find carbonate standards for this correction scheme
    curData <-
      data.cor[[m]][data.cor[[m]]$Type.2 == tert.names.D17O[1] |
                      data.cor[[m]]$Type.2 == tert.names.D17O[2] |
                      data.cor[[m]]$Type.2 == tert.names.D17O[3] |
                      data.cor[[m]]$Type.2 == tert.names.D17O[4], ]
    # Find average observed values
    tert.meas.D17O <- matrix(0,4,1)
    tert.meas.D17O[1] <- mean(subset(curData,Type.2 == tert.names.D17O[1])$D17O.SMOWSLAP.per.meg) # NBS-18
    tert.meas.D17O[2] <- mean(subset(curData,Type.2 == tert.names.D17O[2])$D17O.SMOWSLAP.per.meg) # NBS-19
    tert.meas.D17O[3] <- mean(subset(curData,Type.2 == tert.names.D17O[3])$D17O.SMOWSLAP.per.meg) # IAEA-603
    tert.meas.D17O[4] <- mean(subset(curData,Type.2 == tert.names.D17O[4])$D17O.SMOWSLAP.per.meg) # 102-GC-AZ01
    
    # Set up flag.tert matrix - were sufficient analyses of a standard run?
    # Threshold value for number of analyses
    num.tert.thresh <- 2
    
    flag.tert <- matrix(0,4,1) # default to no values
    flag.tert[1] <- nrow(subset(curData,Type.2 == tert.names.D17O[1]))>=num.tert.thresh
    flag.tert[2] <- nrow(subset(curData,Type.2 == tert.names.D17O[2]))>=num.tert.thresh
    flag.tert[3] <- nrow(subset(curData,Type.2 == tert.names.D17O[3]))>=num.tert.thresh
    flag.tert[4] <- nrow(subset(curData,Type.2 == tert.names.D17O[4]))>=num.tert.thresh
    len.flag.tert <- length(flag.tert)
    
    if (tert.cor.option==1){ # use 'best' available carbonate standard(s)
      # Determine which normalization to use. Order of preference is:
      # 2-pt on NBS-18, NBS-19, and/or IAEA-603
      # 1-pt (offset) on NBS-18, NBS-19, and/or IAEA-603
      # 1-pt (offset) on 102-GC-AZ01 (no international standards available)
      # No correction possible. Give back SMOW-SLAP-normalized data and warning
      
      if (sum(flag.tert[1:(len.flag.tert-1),1])>1){ # do 2-pt on NBS-18, NBS-19, and/or IAEA-603
        # Note that this normalization will make a line from all available standard data - technically it could be a 3-pt line. However, we have never run all three international standards at once and have little NBS-19 left, so this is an unlikely occurrence.
        # Decide which standards to use
        # Cut out 102-GC-AZ01, which is not in use
        tert.true.D17O <- tert.true.D17O[1:(len.flag.tert-1)] # 102-GC-AZ01 is not used
        tert.meas.D17O <- tert.meas.D17O[1:(len.flag.tert-1)] # 102-GC-AZ01 is not used
        tert.names.D17O <- tert.names.D17O[1:(len.flag.tert-1)]
        flag.tert <-flag.tert[1:(len.flag.tert-1)]
        # Pick the available standards
        cur.tert.true.D17O <- tert.true.D17O[flag.tert==1]
        cur.tert.meas.D17O <- tert.meas.D17O[flag.tert==1]
        cur.tert.names.D17O <- tert.names.D17O[flag.tert==1]
        for (mm in 1:length(cur.tert.names.D17O)){
          if (mm==1){
            cur.tert.names.D17O.2 <- cur.tert.names.D17O[mm]
          } else {
            cur.tert.names.D17O.2 <- paste(cur.tert.names.D17O.2, ", ", cur.tert.names.D17O[mm],sep="")
          }
        }
        # Determine the line of best fit for the measured vs. accepted values
        lm.norm.tert <-
          lm(cur.tert.true.D17O ~ cur.tert.meas.D17O) # all standards except 102-GC-AZ01
        lm.norm.tert.slope <-
          lm.norm.tert$coefficients[2] #pull slope from linear model
        lm.norm.tert.intercept <-
          lm.norm.tert$coefficients[1] #pull intercept from linear model
        lm.norm.tert.rsq <-
          summary(lm.norm.tert)$r.squared #pull rsq from linear model
        # Store the model information and the data used to make it
        norm.tert.model[[m]] <-
          c("2-pt",
            cur.tert.names.D17O,
            lm.norm.tert.slope,
            lm.norm.tert.intercept,
            lm.norm.tert.rsq)
        norm.tert.model.data[[m]] <- curData
        # Apply tertiary correction (line of best fit) to data
        # Set up column for secondary-corrected D17O (default is no correction applied)
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm <- data.cor[[m]]$D17O.SMOWSLAP.per.meg
        # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
        test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
          data.cor[[m]]$Type.1 == "Carbonate"
        # Apply 2-pt tertiary correction to D17O data
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <-
          data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.carb]*lm.norm.tert.slope + lm.norm.tert.intercept
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("2-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O.2, ". slope=", round(lm.norm.tert.slope,2), " int=", round(lm.norm.tert.intercept,2), sep="")
        print(paste("2-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O.2, ". slope=", round(lm.norm.tert.slope,2), " int=", round(lm.norm.tert.intercept,2), sep=""))
        
      } else if (sum(flag.tert[1:(len.flag.tert-1),1])==1){ # do 1-pt (offset) on NBS-18, NBS-19, or IAEA-603
        # Decide which standard to use
        # Cut out 102-GC-AZ01, which is not in use
        tert.true.D17O <- tert.true.D17O[1:(len.flag.tert-1)] # 102-GC-AZ01 is not used
        tert.meas.D17O <- tert.meas.D17O[1:(len.flag.tert-1)] # 102-GC-AZ01 is not used
        tert.names.D17O <- tert.names.D17O[1:(len.flag.tert-1)]
        flag.tert <-flag.tert[1:(len.flag.tert-1)]
        # Pick the only standard available
        cur.tert.true.D17O <- tert.true.D17O[flag.tert==1]
        cur.tert.meas.D17O <- tert.meas.D17O[flag.tert==1]
        cur.tert.names.D17O <- tert.names.D17O[flag.tert==1]
        # Determine and apply offset
        offset.tert <- cur.tert.true.D17O-cur.tert.meas.D17O
        # Set up column for secondary-corrected D17O (default is no correction applied)
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm <- data.cor[[m]]$D17O.SMOWSLAP.per.meg
        # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
        test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
          data.cor[[m]]$Type.1 == "Carbonate"
        # Store the model information and the data used to make it
        norm.tert.model[[m]] <- c("1-pt", cur.tert.names.D17O, offset.tert)
        norm.tert.model.data[[m]] <- curData
        # Apply 2-pt tertiary correction to D17O data
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <-
          data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.carb] + offset.tert
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("1-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep="")
        print(paste("1-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep=""))
        
      } else if (sum(flag.tert[1:(len.flag.tert-1),1])==0 & flag.tert[4]==1){ # do 1-pt (offset) on 102-GC-AZ01 (no international standards available)
        # Decide which standard to use
        # Cut out all except 102-GC-AZ01, which is the only standard available
        tert.true.D17O <- tert.true.D17O[len.flag.tert]
        tert.meas.D17O <- tert.meas.D17O[len.flag.tert]
        tert.names.D17O <- tert.names.D17O[len.flag.tert]
        flag.tert <-flag.tert[len.flag.tert]
        # Pick the only standard available
        cur.tert.true.D17O <- tert.true.D17O[flag.tert==1]
        cur.tert.meas.D17O <- tert.meas.D17O[flag.tert==1]
        cur.tert.names.D17O <- tert.names.D17O[flag.tert==1]
        # Determine and apply offset
        offset.tert <- cur.tert.true.D17O-cur.tert.meas.D17O
        # Set up column for secondary-corrected D17O (default is no correction applied)
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm <- data.cor[[m]]$D17O.SMOWSLAP.per.meg
        # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
        test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
          data.cor[[m]]$Type.1 == "Carbonate"
        # Store the model information and the data used to make it
        norm.tert.model[[m]] <- c("1-pt", cur.tert.names.D17O, offset.tert)
        norm.tert.model.data[[m]] <- curData
        # Apply 2-pt tertiary correction to D17O data
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <-
          data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.carb] + offset.tert
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("1-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep="")
        print(paste("1-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep=""))
        
      } else { # No correction possible. Give back SMOW-SLAP-normalized data and warning
        # Apply no correction to data - just copy the SMOW-SLAP-normalized data
        # Set up column for secondary-corrected d18O (default is no correction applied). No need this is done above.
        # data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm <- NA # carbonate data not useful in context of this code
        # test.water <-data.cor[[m]]$Type.1 == "WaterStd" |
        #   data.cor[[m]]$Type.1 == "Water"
        # data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.water] <-
        #   data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.water] # Water data is still useful
        norm.tert.model[[m]] <- c("none")
        norm.tert.model.data[[m]] <- curData
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("No tertiary normalization possible for data.cor[[", m , "]]", sep="")
        print(paste("No tertiary normalization possible for data.cor[[", m , "]]", sep=""))
      }
      
    } else if (tert.cor.option==2){ # Always use 102-GC-AZ01 as the standard
      # Other option is just to always use 102-GC-AZ01 as an offset standard as it is the most consistently used carbonate standard. By only using this standard the accuracy is reduced a little as we don't know the exact D17O value. However, because everything is normalized to the same standard, inter-reactor comparisons are always valid.
      if (flag.tert[4]==1){ # do 1-pt (offset) on 102-GC-AZ01 (no international standards available)
        # Cut out all standards except 102-GC-AZ01
        tert.true.D17O <- tert.true.D17O[len.flag.tert]
        tert.meas.D17O <- tert.meas.D17O[len.flag.tert]
        tert.names.D17O <- tert.names.D17O[len.flag.tert]
        flag.tert <-flag.tert[len.flag.tert]
        # Pick the only standard
        cur.tert.true.D17O <- tert.true.D17O[flag.tert==1]
        cur.tert.meas.D17O <- tert.meas.D17O[flag.tert==1]
        cur.tert.names.D17O <- tert.names.D17O[flag.tert==1]
        # Determine and apply offset
        offset.tert <- cur.tert.true.D17O-cur.tert.meas.D17O
        # Set up column for secondary-corrected D17O (default is no correction applied)
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm <- data.cor[[m]]$D17O.SMOWSLAP.per.meg
        # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
        test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
          data.cor[[m]]$Type.1 == "Carbonate"
        # Store the model information and the data used to make it
        norm.tert.model[[m]] <- c("1-pt", cur.tert.names.D17O, offset.tert)
        norm.tert.model.data[[m]] <- curData
        # Apply tertiary correction to D17O data
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <-
          data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.carb] + offset.tert
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("1-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep="")
        print(paste("1-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep=""))
        
      } else { # No correction possible. Give back SMOW-SLAP-normalized data and warning
        # Apply no correction to data - just copy the SMOW-SLAP-normalized data
        # Set up column for secondary-corrected d18O (default is no correction applied). No need this is done above
        # data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <- data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.carb] # this is just the SMOW-SLAP corrected column copied over
        # test.water <-data.cor[[m]]$Type.1 == "WaterStd" |
        #   data.cor[[m]]$Type.1 == "Water"
        # data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.water] <-
        #   data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.water] # Water data is still useful
        # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
        test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
          data.cor[[m]]$Type.1 == "Carbonate"
        norm.tert.model[[m]] <- c("none")
        norm.tert.model.data[[m]] <- curData
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("No tertiary normalization possible for this analysis, kept SMOW-SLAP corrected data", sep="")
        print(paste("No tertiary normalization possible for data.cor[[", m , "]]", sep=""))
      }
      
    } else if (tert.cor.option==3){ # Normalize to the Wostbrock et al. (2020) calcite value for IAEA-603(and IAEA-C1)
      ##### WARNING: This option does not work prior to reactor 10, which was when we first started measuring IAEA-C1 (IAEA-603 was first measured in reactor 13)
      ##### WARNING: This option does not work for JHU data as these standards were not measured (IAEA-C1) or didn't exist (IAEA-603)
      # List of standard names for correcting directly to calcite
      tert.names.D17O <- c("IAEA-603", "IAEA-C1")
      
      # Accepted true values of D17O (per meg), as calcite
      tert.true.D17O <- matrix(0,1,1)
      tert.true.D17O[1] <- -100 # IAEA-603 Wostbrock et al. (2020), as calcite (presumably also good for IAEA-C1)
      tert.true.D17O[2] <- -100 # IAEA-603 Wostbrock et al. (2020), as calcite (presumably also good for IAEA-C1)
      
      # Find carbonate standards for this correction scheme
      curData <-
        data.cor[[m]][data.cor[[m]]$Type.2 == tert.names.D17O[1] |
                        data.cor[[m]]$Type.2 == tert.names.D17O[2], ]
      # Find average observed value
      tert.meas.D17O <- matrix(0,1,1)
      tert.meas.D17O[1] <- mean(curData$D17O.SMOWSLAP.per.meg) # IAEA-603 and IAEA-C1 are the same, so lump them together
      
      # Set up flag.tert matrix - were sufficient analyses of a standard run?
      # Threshold value for number of analyses
      num.tert.thresh <- 2
      
      flag.tert <- matrix(0,1,1) # default to no values
      flag.tert[1] <- nrow(subset(curData,Type.2 == tert.names.D17O[1])) + nrow(subset(curData,Type.2 == tert.names.D17O[2]))>=num.tert.thresh
      len.flag.tert <- length(flag.tert)
      
      if (flag.tert[1]==1){ # do 1-pt (offset) on IAEA-603 and IAEA-C1 using the calcite value for IAEA-603 (-100)
        # Cut out all standards except 102-GC-AZ01
        tert.true.D17O <- tert.true.D17O[len.flag.tert]
        tert.meas.D17O <- tert.meas.D17O[len.flag.tert]
        tert.names.D17O <- tert.names.D17O[len.flag.tert]
        flag.tert <-flag.tert[len.flag.tert]
        # Pick the only standard value
        cur.tert.true.D17O <- tert.true.D17O[flag.tert==1]
        cur.tert.meas.D17O <- tert.meas.D17O[flag.tert==1]
        cur.tert.names.D17O <- "IAEA-603 and/or IAEA-C1 as calcite"
        # Determine and apply offset
        offset.tert <- cur.tert.true.D17O-cur.tert.meas.D17O
        # Set up column for secondary-corrected D17O (default is no correction applied)
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm <- data.cor[[m]]$D17O.SMOWSLAP.per.meg
        # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
        test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
          data.cor[[m]]$Type.1 == "Carbonate"
        # Store the model information and the data used to make it
        norm.tert.model[[m]] <- c("1-pt", cur.tert.names.D17O, offset.tert)
        norm.tert.model.data[[m]] <- curData
        # Apply tertiary correction to D17O data
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <-
          data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.carb] + offset.tert
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("1-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep="")
        print(paste("1-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep=""))
        
      } else { # No correction possible. Give back SMOW-SLAP-normalized data and warning
        # Apply no correction to data - just copy the SMOW-SLAP-normalized data
        # Set up column for secondary-corrected d18O (default is no correction applied). No need this is done above
        # data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <- data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.carb] # this is just the SMOW-SLAP corrected column copied over
        # test.water <-data.cor[[m]]$Type.1 == "WaterStd" |
        #   data.cor[[m]]$Type.1 == "Water"
        # data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.water] <-
        #   data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.water] # Water data is still useful
        # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
        test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
          data.cor[[m]]$Type.1 == "Carbonate"
        norm.tert.model[[m]] <- c("none")
        norm.tert.model.data[[m]] <- curData
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("No tertiary normalization possible for this analysis, kept SMOW-SLAP corrected data", sep="")
        print(paste("No tertiary normalization possible for data.cor[[", m , "]]", sep=""))
      }
    }
  }
  
  ############################################################################################
  # Secondary data normalization for d18O and d17O (carbonates only)
  ############################################################################################   
  # Check to see if at least two of the O2-analyzed samples match ones that we know from traditional analysis (via CO2)
  flag.mult.carbstd <- matrix(0,length(data.cor),1) # default flag is -9999 = do not perform secondary correction
  for (m in 1:length(data.cor)){
    # Make a list of all the O2-analyzed carbonates in this reactor, for each correction scheme
    curCarbO2 <- subset(data.cor[[m]],data.cor[[m]]$Type.1 == "CarbonateStd" | data.cor[[m]]$Type.1 == "Carbonate")
    curCarbO2 <- unique(curCarbO2$sample.ID) # Only keep the relevant column
    # Now compare the list of O2-analyzed carbonates to the list of carbonates with known d18O(CO2) values
    for (mm in 1:length(curCarbO2)){
      curSamp <- curCarbO2[mm]
      testKnown <- trad.master$sample.ID == curSamp
      if (isTRUE(sum(testKnown) == 1)) {
        # match is found
        flag.mult.carbstd[m,1] <- flag.mult.carbstd[m,1]+1
      }
    }
  }
  
  
  # Make and apply the secondary data normalization if possible.
  # Holding lists
  norm.2nd.model <-
    vector(mode = "list", length = length(data.cor)) # holding list for correlations
  norm.2nd.model.data <-
    vector(mode = "list", length = length(data.cor)) # holding list for correlation data
  
  for (m in 1:length(data.cor)) {
    if (flag.mult.carbstd[m,1]>1){ # Need at least 2 samples to do the linearization
      # each correction scheme is done separately
      # Get all carbonate analyses from current dataset
      curData <-
        data.cor[[m]][data.cor[[m]]$Type.1 == "CarbonateStd" |
                        data.cor[[m]]$Type.1 == "Carbonate", ]
      # Assign d18O-mineral values from the trad.master sheet
      curData$trad.d18O.mineral.co2 <- NA # assign NA by default
      for (mm in 1:nrow(curData)) {
        # Get current sample
        curSamp = curData$sample.ID[mm]
        # Find and add the matching d18O-mineral value (if it exists)
        test.trad <-
          trad.master$sample.ID == curSamp # test for match, also keep for indexing
        if (sum(test.trad) == 1) {
          # match is found
          # is there data for this point?
          if (!is.na(trad.master$d18O.min.CO2.vpdb[test.trad])){
            curData$trad.d18O.mineral.co2[mm] <-
              trad.master$d18O.min.CO2.vpdb[test.trad] # On first match, this also sets all other rows to NA
          }
        }
      }
      # Get rid of all rows that do not have matching traditional isotope value
      curData <- subset(curData,!is.na(curData$trad.d18O.mineral.co2))
      
      # Convert mineral values from VPDB to VSMOW
      curData$trad.d18O.mineral.co2 <- 1.03092*curData$trad.d18O.mineral.co2 + 30.92
      # Convert the d18O-mineral value to d'18O(CO2/CacO3)
      acid.temp <- 90 # acid is at 90C in IPL
      acid.frac <- exp((3.59 * (10 ^ 3/(acid.temp + 273.15)) - 1.79) / 1000)
      curData$trad.d18O.co2.cc <-
        acid.frac * (1000 + curData$trad.d18O.mineral.co2) - 1000 # d18O(CO2/CaCO3)
      curData$trad.dp18O.co2.cc <-
        log(curData$trad.d18O.co2.cc / 1000 + 1) * 1000 # d'18O(CO2/CaCO3)
      
      # Determine the line of best fit for the CO2 vs. O2-based measurements
      lm.norm.2nd <-
        lm(curData$dp18O.SMOWSLAP ~ curData$trad.dp18O.co2.cc)
      lm.norm.2nd.slope <-
        lm.norm.2nd$coefficients[2] #pull slope from linear model
      lm.norm.2nd.intercept <-
        lm.norm.2nd$coefficients[1] #pull intercept from linear model
      lm.norm.2nd.rsq <-
        summary(lm.norm.2nd)$r.squared #pull rsq from linear model
      # Store the model information and the data used to make it
      norm.2nd.model[[m]] <-
        c(lm.norm.2nd.slope,
          lm.norm.2nd.intercept,
          lm.norm.2nd.rsq)
      norm.2nd.model.data[[m]] <- curData
      
      # Apply secondary correction (line of best fit) to data
      # Index for the carbonate data (model only applies to carbonates, which have a blank correction)
      test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
        data.cor[[m]]$Type.1 == "Carbonate"
      
      # Apply secondary correction to d18O data for the carbonates
      data.cor[[m]]$dp18O.SMOWSLAP.CO2norm[test.carb] <-
        (data.cor[[m]]$dp18O.SMOWSLAP[test.carb] - lm.norm.2nd.intercept) / lm.norm.2nd.slope
      # And note the type of correction
      data.cor[[m]]$dp18O.SMOWSLAP.CO2norm.type[test.carb] <- paste("secondary correction applied", sep="")
      
      # Apply secondary correction to d17O data
      # Note that this correction scheme technically requires knowledge of lambda-blank (see Passey et al. 2014)
      # In the case where no correction was applied to D'17O data, this carries over to the dp17O data
      # In the case where a correction was applied to D'17O data, this correction is implicitly carried over to the dp17O data.
      data.cor[[m]]$dp17O.SMOWSLAP.CO2norm[test.carb] <-
        (data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb]/10^6 + 0.528*data.cor[[m]]$dp18O.SMOWSLAP.CO2norm[test.carb]/1000)*1000
      # And note the type of correction
      data.cor[[m]]$dp17O.SMOWSLAP.CO2norm.type[test.carb] <- paste("secondary correction applied", sep="")
    } else { # the secondary correction is not possible for d18O data
      # While the secondary correction cannot be done for d18O data, it may still be required for d17O data if a correction was done for the D'17O data (i.e., if only 102-GC-AZ01 is known)
      # This also runs when a secondary correction cannot be applied to d18O or D'17O data - in this case the d'17O values do not change
      
      # Index for the carbonate data (model only applies to carbonates, which have a blank correction)
      test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
        data.cor[[m]]$Type.1 == "Carbonate"
      
      # Note no correction for d18O data
      data.cor[[m]]$dp18O.SMOWSLAP.CO2norm.type[test.carb] <- paste("no secondary correction", sep="")
      
      # Apply secondary correction to d17O data (covers case where D'17O received tertiary correction but d18O did not receive secondary correction)
      # Note that this correction scheme technically requires knowledge of lambda-blank (see Passey et al. 2014)
      # In the case where no correction was applied to D'17O data, this carries over to the dp17O data
      # In the case where a correction was applied to D'17O data, this correction is implicitly carried over to the dp17O data.
      data.cor[[m]]$dp17O.SMOWSLAP.CO2norm[test.carb] <-
        (data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb]/10^6 + 0.528*data.cor[[m]]$dp18O.SMOWSLAP.CO2norm[test.carb]/1000)*1000
      # And note the type of correction
      data.cor[[m]]$dp17O.SMOWSLAP.CO2norm.type[test.carb] <- paste("secondary correction applied but only meaningful if D'17O correction was applied", sep="")
    }
  }
  
  
  ############################################################################################
  # Tertiary data normalization for D17O - for the direct mineral correction only
  # This is done after the d18O-O2 vs. d18O-CO2 (lambda=0.528) is applied
  ############################################################################################
  if (tert.cor.option==4){ # Wostbrock-normalization (7/1/2021).  
    # Applies an  correction for D17O due to acid bath-reduction line. Uses Wostbrock et al. (2020) values for NBS-18, NBS-19, and IAEA-603. If these are not available, or have <2 replicates, the correction is applied based on 102-GCAZ-01 (average value = -125 per meg based on all-time average vs. NBS-18 and NBS-19 standards)
    for (m in 1:length(data.cor)){
      
      #Uses (1) the IAEA-defined 18alpha(m-O2, 90C) and IPL-long term-based 17alpha(m-O2,
      #90C) followed by (2) temporal drift correction for D'17O. 
      
      # This is based on IAEA-603 and IAEA-C1 (assuming IAEA-C1 is identical to IAEA-603)
      
      #### WARNING:
      #This option does not work prior to reactor 10, which was when we first
      #started measuring IAEA-C1 (IAEA-603 was first measured in reactor 13)
      
      ##### WARNING: This option does not work for JHU data as these standards
      #were not measured (IAEA-C1) or didn't exist (IAEA-603) 
      
      # Define the values of fractionation factors for (mineral-O2 90C)
      # For 18alpha, define in terms of IPL long term d18O(O2 90C) and IAEA-defined d18O(m)
      d18Om_IAEA = 28.470 # IAEA-603 d18O-calcite in VSMOW
      d18O_o290C = (exp(36.23316/1000)-1)*1000 # IPL IAEA-603 and IAEA-C1 long term average for d18O-o2-90C acid, corrected for d18O-O2 vs. d18O-CO2(known) (last updated 7/1/2021)
      alpha18_m_o290C = (d18Om_IAEA/1000+1) / (d18O_o290C/1000+1)
      # For 17alpha, define in terms of IPL long term d18O(O2 90C) and Wostbrock et al. (2020)-defined d17O(m)
      d17Om_defined = 14.831 # IAEA-603 d18O-calcite in VSMOW - from Wostbrock et al. (2020)
      Dp17O_o290C = -145.71901 # IPL long term IAEA-603 and IAEA-C1 D'17O average (last updated 7/1/2021)
      dp17O_o290C = (Dp17O_o290C/10^6 + 0.528*log(d18O_o290C/1000+1))*1000 # d'17O for IAEA-603 and IAEA-C1
      d17O_o290C = (exp(dp17O_o290C/1000)-1)*1000 # d17O for IAEA-603 and IAEA-C1
      # d17O_o290C = (exp(18.98539/1000)-1)*1000 # IPL IAEA-603 and IAEA-C1 long term average for d17O-o2-90C acid (last updated 7/1/2021)
      alpha17_m_o290C = (d17Om_defined/1000+1) / (d17O_o290C/1000+1)
      
      # Set up column for secondary-corrected D17O (default is no correction applied)
      data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm <- data.cor[[m]]$D17O.SMOWSLAP.per.meg
      # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
      test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
        data.cor[[m]]$Type.1 == "Carbonate"
      # Apply mineral correction to d'17O and d'18O data
      # Get d'18O-mineral
      data.cor[[m]]$dp18O.SMOWSLAP.CO2norm[test.carb] <-
        log(exp(data.cor[[m]]$dp18O.SMOWSLAP.CO2norm[test.carb]/1000)*alpha18_m_o290C)*1000
      # Get d'17O-mineral
      data.cor[[m]]$dp17O.SMOWSLAP.CO2norm[test.carb] <-
        log(exp(data.cor[[m]]$dp17O.SMOWSLAP.CO2norm[test.carb]/1000)*alpha17_m_o290C)*1000
      # Calculate the mineral value for D'17O
      data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <- 
        (data.cor[[m]]$dp17O.SMOWSLAP.CO2norm[test.carb]/1000 - 0.528*data.cor[[m]]$dp18O.SMOWSLAP.CO2norm[test.carb]/1000)*10^6
      
      
      # Account for temporal drift by applying 1-pt offset for D'17O data based on IAEA-603 and IAEA-C1
      ##### WARNING: This option does not work prior to reactor 10, which was when we first started measuring IAEA-C1 (IAEA-603 was first measured in reactor 13)
      ##### WARNING: This option does not work for JHU data as these standards were not measured (IAEA-C1) or didn't exist (IAEA-603)
      # List of standard names for correcting directly to calcite
      # Note that, after selecting both IAEA-603 and IAEA-C1 samples, most of the code below only actually calls the values for IAEA-603. This is because IAEA-603 and IAEA-C1 have the same composition.
      tert.names.D17O <- c("IAEA-603", "IAEA-C1")
      
      # Accepted true values of D17O (per meg), as calcite
      tert.true.D17O <- matrix(0,1,1)
      tert.true.D17O[1] <- -100 # IAEA-603 Wostbrock et al. (2020), as calcite (presumably also good for IAEA-C1)
      tert.true.D17O[2] <- -100 # IAEA-603 Wostbrock et al. (2020), as calcite (presumably also good for IAEA-C1)
      
      # Find carbonate standards for this correction scheme
      curData <-
        data.cor[[m]][data.cor[[m]]$Type.2 == tert.names.D17O[1] |
                        data.cor[[m]]$Type.2 == tert.names.D17O[2], ]
      # Find average observed value
      tert.meas.D17O <- matrix(0,1,1)
      tert.meas.D17O[1] <- mean(curData$D17O.SMOWSLAP.per.meg.carbNorm) # IAEA-603 and IAEA-C1 are the same, so lump them together
      
      # Set up flag.tert matrix - were sufficient analyses of a standard run?
      # Threshold value for number of analyses
      num.tert.thresh <- 2
      
      flag.tert <- matrix(0,1,1) # default to no values
      flag.tert[1] <- nrow(subset(curData,Type.2 == tert.names.D17O[1])) + nrow(subset(curData,Type.2 == tert.names.D17O[2]))>=num.tert.thresh
      len.flag.tert <- length(flag.tert)
      
      if (flag.tert[1]==1){ # do 1-pt (offset) on IAEA-603 and IAEA-C1 using the calcite value for IAEA-603 (-100)
        # Cut out all standards except 102-GC-AZ01
        tert.true.D17O <- tert.true.D17O[len.flag.tert]
        tert.meas.D17O <- tert.meas.D17O[len.flag.tert]
        tert.names.D17O <- tert.names.D17O[len.flag.tert]
        flag.tert <-flag.tert[len.flag.tert]
        # Pick the only standard value
        cur.tert.true.D17O <- tert.true.D17O[flag.tert==1]
        cur.tert.meas.D17O <- tert.meas.D17O[flag.tert==1]
        cur.tert.names.D17O <- "IAEA-603 and/or IAEA-C1 as calcite"
        # Determine and apply offset
        offset.tert <- cur.tert.true.D17O-cur.tert.meas.D17O
        # No need to set up the column for secondary-corrected D17O, as this was done above (default is no 1-pt temporal drift correction applied)
        # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
        test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
          data.cor[[m]]$Type.1 == "Carbonate"
        
        # Store the model information and the data used to make it
        norm.tert.model[[m]] <- c("mineral and 1-pt temporal drift", cur.tert.names.D17O, offset.tert)
        norm.tert.model.data[[m]] <- curData
        # Apply tertiary correction to D17O data
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <-
          data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] + offset.tert
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("mineral + 1-pt normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep="")
        print(paste("mineral and 1-pt temporal drift normalization applied to D17O for data.cor[[", m , "]] using ", cur.tert.names.D17O, ". Offset=", round(offset.tert,2), " per meg", sep=""))
        
        # Finally, recalculate d'17O data to account for the carbonate D'17O receiving the 1-pt offset correction
        data.cor[[m]]$dp17O.SMOWSLAP.CO2norm[test.carb] <-
          (data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb]/10^6 + 0.528*data.cor[[m]]$dp18O.SMOWSLAP.CO2norm[test.carb]/1000)*1000
        
      } else { # No correction possible. Give back SMOW-SLAP-normalized data and warning
        # Apply no correction to data - just copy the SMOW-SLAP-normalized data
        # Set up column for secondary-corrected d18O (default is no correction applied). No need this is done above
        # data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.carb] <- data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.carb] # this is just the SMOW-SLAP corrected column copied over
        # test.water <-data.cor[[m]]$Type.1 == "WaterStd" |
        #   data.cor[[m]]$Type.1 == "Water"
        # data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm[test.water] <-
        #   data.cor[[m]]$D17O.SMOWSLAP.per.meg[test.water] # Water data is still useful
        # Index for the carbonate data (model only applies to carbonates, which have a tertiary correction)
        test.carb <-data.cor[[m]]$Type.1 == "CarbonateStd" |
          data.cor[[m]]$Type.1 == "Carbonate"
        norm.tert.model[[m]] <- c("mineral, but no 1-pt temporal drift correction")
        norm.tert.model.data[[m]] <- curData
        # And note the type of correction
        data.cor[[m]]$D17O.SMOWSLAP.per.meg.carbNorm.type[test.carb] <- paste("No tertiary normalization possible for this analysis, kept SMOW-SLAP corrected data", sep="")
        print(paste("Mineral correction applied, but no 1-pt temporal drift normalization possible for data.cor[[", m , "]]", sep=""))
      }
    }
  }
  
  
  ############################################################################################
  # Summary tables
  ############################################################################################
  # Summary tables and plots to compare primary  standards - also by mode of correction
  # Set up the empty list - same number of rows as data.cor
  summary.SMOW <-
    data.frame(matrix(0, ncol = length(data.cor) + 1, nrow = 8))
  colnames(summary.SMOW) <- c(1:length(data.cor))
  
  # Get all of the column names
  m = colnames(summary.SMOW)
  summary.SMOW[, 1] <- c(
    "type",
    "d33 rsq",
    "d34 rsq",
    "seg. start" ,
    "seg. end",
    "SMOW SD (per mil)" ,
    "SMOW n" ,
    "SMOW range"
  )
  
  # Fill the list with the summary information
  plot.col.byCorr.names = matrix(NA, nrow = ncol(summary.SMOW), ncol = 1)
  for (k in 1:(ncol(summary.SMOW) - 1)) {
    # Find the correct datasets
    if (k == 1) {
      curData.line.flag <- -9999 # assume there is no linear data
      # BASIC correction
      curData <- data.cor[[1]]
      type = "basic"
    } else if (k == 2) {
      # LINEAR correction
      curData <- data.cor[[2]]
      curData.line <- data.cor.line.stats[[2]]
      curData.line.flag = 1
      type = "linear"
    } else if (k > 2) {
      # SEGMENT correction(s)
      curData <- data.cor[[k]]
      curData.line <- data.cor.line.stats[[k]]
      if (length(curData.line) == 1) {
        # segments run via BASIC correction
        type = paste("seg", k - 2, "basic", sep = "")
        curData.line.flag = -9999
      } else {
        # segments run via LINEAR correction
        curData.line.flag = 1
        type = paste("seg", k - (2 + nrow(segment.IPL)), "linear", sep =
                       "")
      }
    }
    # Identify names for later plotting
    plot.col.byCorr.names[k] = type
    names(plot.col.byCorr) = plot.col.byCorr.names
    
    # Now fill in the table
    cur.smow <- subset(curData, Type.2 == "SMOW" & flag == 0)
    if (curData.line.flag == 1) {
      # there is linear data
      # These are all written out so others can see where the numbers are coming from
      d33r2 = round(curData.line[3, 2], 2)
      d34r2 = round(curData.line[3, 3], 2)
      segStart = min(as.numeric(curData$IPL.num))
      segEnd = max(as.numeric(curData$IPL.num))
      smow.SD = round(sd(as.numeric(cur.smow$D17O.SMOWSLAP.per.meg.carbNorm)), 0)
      smow.n = nrow(cur.smow)
      smow.range = round(max(as.numeric(cur.smow$D17O.SMOWSLAP.per.meg.carbNorm)) - min(as.numeric(cur.smow$D17O.SMOWSLAP.per.meg.carbNorm)))
    } else {
      # there is NOT linear data
      d33r2 = NA
      d34r2 = NA
      segStart = min(as.numeric(curData$IPL.num))
      segEnd = max(as.numeric(curData$IPL.num))
      smow.SD = round(sd(as.numeric(cur.smow$D17O.SMOWSLAP.per.meg.carbNorm)), 0)
      smow.n = nrow(cur.smow)
      smow.range = round(max(as.numeric(cur.smow$D17O.SMOWSLAP.per.meg.carbNorm)) - min(as.numeric(cur.smow$D17O.SMOWSLAP.per.meg.carbNorm)))
    }
    # Make single set of data and put in the proper row
    curColData = c(type,
                   d33r2,
                   d34r2,
                   segStart,
                   segEnd,
                   smow.SD,
                   smow.n,
                   smow.range)
    summary.SMOW[[m[k + 1]]] = curColData
    
    # Create dataframe of the different SMOW corrections for comparison
    if (k == 1) {
      # add data
      corType = data.frame(matrix(
        plot.col.byCorr.names[k],
        ncol = 1,
        nrow = length(cur.smow$Date.Time)
      ))
      smow.all <-
        data.frame(
          cur.smow$IPL.num,
          cur.smow$Date.Time,
          cur.smow$d33,
          cur.smow$d34,
          cur.smow$D17O.SMOWSLAP.per.meg.carbNorm,
          corType
        )
      # change column names (can't be done at same time apparently...)
      names(smow.all) = c("IPL.num", "Date.Time", "d33", "d34", "D17O.SMOWSLAP.per.meg.carbNorm", "CorType")
      count <-
        ncol(smow.all) # keep track of number of columns in list
    } else {
      # just add on to the plot
      corType = data.frame(matrix(
        plot.col.byCorr.names[k],
        ncol = 1,
        nrow = length(cur.smow$Date.Time)
      ))
      smow.all.add = data.frame(
        cur.smow$IPL.num,
        cur.smow$Date.Time,
        cur.smow$d33,
        cur.smow$d34,
        cur.smow$D17O.SMOWSLAP.per.meg.carbNorm,
        corType
      )
      names(smow.all.add) = c("IPL.num", "Date.Time", "d33", "d34", "D17O.SMOWSLAP.per.meg.carbNorm", "CorType")
      smow.all = rbind(smow.all, smow.all.add)
      count <- count + ncol(smow.all)
    }
  }
  
  # Make the date format correct for ggplot (factor to date)
  smow.all[, 2] <- ymd_hms(smow.all[, 2])
  
  
  ############################################################################################
  # Summary plots for SMOW - d33 vs. d34
  ############################################################################################ 
  
  
  ############################################################################################
  # Plots using the different correction types
  # D'17O of SMOW through time
  # D'17O of all standards through time
  # D'17O of all samples through time
  ############################################################################################ 
  
  
  
  
  
  
  
  ############################################################################################
  # Summary plots for SMOW (d34, d34, and D17O vs. time)
  ############################################################################################
  # List of variables to plot. Sets are listed as
  # x variable
  # y variable
  # flag for how the y variable is defined/subset. If =0, it is a column. Otherwise it specifies the subsetting needed on the y variable.
  plotSet <- data.frame(xVar=character(),
                        yVar=character(),
                        col.subset=character(),
                        stringsAsFactors=FALSE)
  plotCon <- data.frame(con1=character(),
                        con2-character())
  
  
  plotSet[1,1:3] <- c("IPL.num","d33",'subset(data.cor[[r]], Type.2 == "SMOW" & flag == 0)')
  plotSet[2,1:3] <- c("IPL.num","d34",'subset(data.cor[[r]], Type.2 == "SMOW" & flag == 0)')
  plotSet[3,1:3] <- c("IPL.num","D17O.SMOWSLAP.per.meg.carbNorm", 'subset(data.cor[[r]], Type.2 == "SMOW" & flag == 0)')
  
  
  # plotSet[4,1:3] <- c("IPL.num","d33")
  # plotSet[5,1:3] <- c("IPL.num","d33")
  # plotSet[6,1:3] <- c("IPL.num","d33")
  # plotSet[7,1:3] <- c("IPL.num","d33")
  
  
  for (rr in 1:nrow(plotSet)){
    for (r in 1:length(data.cor)) {
      # Set current data
      curData <- eval(parse(text=plotSet[rr,3]))
      
      plot.SMOW.allCor <-
        ggplot() + geom_point(
          data = curData,
          aes(
            x = eval(parse(text=plotSet[rr,1])),
            y = eval(parse(text=plotSet[rr,2])),
            color = CorType
          ),
          size = 3
        ) + scale_color_manual(values = plot.col.byCorr)
      # add labels
      plot.SMOW.allCor = plot.SMOW.allCor + labs(x = "IPL #",
                                                 y = bquote(~ Delta ^ 17 ~ "O of SMOW (per meg, VSMOW-SLAP)"),
                                                 title = "all correction schemes")  +
        theme(legend.title = element_blank())
      
    }
  }

  
  
  
  
  
  
  
  
  ############################################################################################
  # Summary plots for SMOW (d34, d34, and D17O vs. time)
  ############################################################################################
  # Plot of d33 vs. d34 by correction scheme
  # BASIC and LINEAR draw from the same dataset, so these are plotted together. Segmented correction done separately.
  for (rr in 1:2) {
    # two loops for d33 and d34 separately
    plot.list.d33d34 = list() # make empty list each time
    if (rr == 1) {
      # d33 is current variable
      curSMOW <- select(smow.all, IPL.num, d33, CorType)
    } else {
      # d34 is current variable
      curSMOW <- select(smow.all, IPL.num, d34, CorType)
    }
    for (r in 1:length(data.cor)) {
      # Note that all data is different views of the same points - this is just for visually ascertaining data trends
      curSMOW2 = subset(curSMOW, CorType == plot.col.byCorr.names[r])
      plot.list.d33d34[[r]] <- ggplot.maker.d33d34(curSMOW2)
    }
    # Plot up the d33/d34 data
    multiplot(plotlist = plot.list.d33d34, cols = 3)
  }
  
  
  # Plot of SMOWS by correction scheme
  plot.list.smow = list()#  vector(mode = "list", length = (ncol(summary.SMOW)-1))
  for (r in 1:length(data.cor)) {
    curData = data.cor[[r]]
    curSMOW = subset(curData, Type.2 == "SMOW" & flag == 0)
    plot.list.smow[[r]] <- ggplot.maker.IPLD17O(curSMOW)
  }
  r.end <- r + 1
  # This allows a plot of the different correction schemes on top of each other
  plot.SMOW.allCor <-
    ggplot() + geom_point(
      data = smow.all,
      aes(
        x = IPL.num,
        y = D17O.SMOWSLAP.per.meg.carbNorm,
        color = CorType
      ),
      size = 3
    ) + scale_color_manual(values = plot.col.byCorr)
  # add labels
  plot.SMOW.allCor = plot.SMOW.allCor + labs(x = "IPL #",
                                             y = bquote(~ Delta ^ 17 ~ "O of SMOW (per meg, VSMOW-SLAP)"),
                                             title = "all correction schemes")  +
    theme(legend.title = element_blank())
  plot.list.smow[[r.end]] = plot.SMOW.allCor
  multiplot(plotlist = plot.list.smow, cols = 3)
  
  ############################################################################################
  # Summary tables plots for primary and secondary standards
  ############################################################################################
  # Summary tables and plots to compare primary and secondary standards - also by mode of correction
  # This plots up the different correction schemes next to each other
  plot.list.std = list()#  vector(mode = "list", length = (ncol(summary.SMOW)-1))
  all.std = list()
  summary.std = list()
  for (r in 1:length(data.cor))
  {
    for (rr in 1:length(standards))
    {
      curData = data.cor[[r]]
      cur.std = subset(curData, Type.2 == standards[[rr]] &
                         flag == 0)
      # Find current standard's accepted value
      cur.accepted <-
        subset(std.accepted, standard == standards[rr])
      # Calculate and add the current standard's residual to the dataframe
      cur.std$d18O.accept <-
        cur.std$dp18O.SMOWSLAP * 0 + cur.accepted$d18O.per.mil.accepted
      cur.std$D17O.accept <-
        cur.std$D17O.SMOWSLAP.per.meg.carbNorm * 0 + cur.accepted$D17O.per.meg.accepted
      cur.std$residual <-
        cur.std$D17O.SMOWSLAP.per.meg.carbNorm - cur.accepted$D17O.per.meg.accepted
      # calculate average info for the summary-standards list
      avg = round(mean(cur.std$D17O.SMOWSLAP.per.meg.carbNorm), 0)
      avg.resid = round(avg - cur.accepted$D17O.per.meg.accepted, 0)
      n = nrow(cur.std) # number of replicates
      SD = round(sd(cur.std$D17O.SMOWSLAP.per.meg.carbNorm), 0) # standard deviation of replicates
      if (rr == 1) {
        # note SMOW always exists in every run
        cur.std.mat <- cur.std
        cur.std.mat.avg <-
          data.frame(
            cur.accepted$d18O.per.mil.accepted,
            cur.accepted$D17O.per.meg.accepted,
            avg,
            avg.resid,
            n,
            SD
          )
        cur.std.mat.avg.rownames = standards[rr]
      } else if (rr > 1 &
                 nrow(cur.std) == 0) {
        # the standard was not analyzed during this reactor
        # fill with NAs
        a = 2
        na.replacer <-
          data.frame(matrix(NA, nrow = 1, ncol = ncol(cur.std.mat)))
        names(na.replacer) <- names(cur.std.mat)
        cur.std.mat <- rbind(cur.std.mat, na.replacer)
        na.replacer2 <-
          data.frame(matrix(
            NA,
            nrow = 1,
            ncol = ncol(cur.std.mat.avg)
          ))
        names(na.replacer2) <- names(cur.std.mat.avg)
        cur.std.mat.avg <- rbind(cur.std.mat.avg, na.replacer2)
        cur.std.mat.avg.rownames = rbind(cur.std.mat.avg.rownames, standards[rr])
      } else if (rr > 1 &
                 nrow(cur.std) > 0) {
        # the standard was analyzed during this reactor
        a = 3
        cur.std.mat <- rbind(cur.std.mat, cur.std)
        cur.std.mat.avg <-
          rbind(
            cur.std.mat.avg,
            c(
              cur.accepted$d18O.per.mil.accepted,
              cur.accepted$D17O.per.meg.accepted,
              avg,
              avg.resid,
              n,
              SD
            )
          )
        cur.std.mat.avg.rownames = rbind(cur.std.mat.avg.rownames, standards[rr])
      }
    }
    if (nrow(cur.std.mat) > 0) {
      # Update the plotting list if the standard was run in this reactor
      plot.list.std[[r]] <- ggplot.maker.IPLD17O.std(cur.std.mat)
    }
    # Update the all-standards list each correction loop
    all.std[[r]] <- cur.std.mat
    # Update summary-standards list each correction loop
    names(cur.std.mat.avg) <-
      c("d18O.accept",
        "D17O.accept",
        "avg.D17O",
        "avg.D17O.resid",
        "n",
        "SD")
    row.names(cur.std.mat.avg) <- cur.std.mat.avg.rownames
    summary.std[[r]] <- cur.std.mat.avg
    
    # add labels
    plot.list.std[[r]] = plot.list.std[[r]] + labs(x = "IPL #",
                                               y = bquote(~ Delta ^ 17 ~ "O residual (obs-exp, per meg) "),
                                               title = paste(plot.col.byCorr.names[r]," correction",sep=""))  +
      theme(legend.title = element_blank())
  }
  multiplot(plotlist = plot.list.std, cols = 3)
  
  ############################################################################################
  # Plots of all data through time
  ############################################################################################
  # This plots up all data in different correction schemes next to each other
  # plot.list.all = list()#  vector(mode = "list", length = (ncol(summary.SMOW)-1))
  # for (r in 1:length(data.cor))
  # {
  # 
  #     curData = subset(data.cor[[r]], flag==0)
  #    plot.list.all[[r]] <- ggplot.maker.IPLD17O(curData)
  #   
  #   # add labels
  #    plot.list.all[[r]] = plot.list.all[[r]] + labs(x = "IPL #",
  #                                                  y = bquote(~ Delta ^ 17 ~ "O (per meg VSMOW-SLAP) "),
  #                                                  title = paste(plot.col.byCorr.names[r]," correction",sep=""))
  #    
  #    if (r == length(data.cor)){
  #      plot.list.all[[r]] = plot.list.all[[r]] +
  #     theme(legend.title = element_blank())
  #    }
  # }
  # multiplot(plotlist = plot.list.all, cols = 3)
  
  
  
  
  
  
  ############################################################################################
  # Output corrected data for all and auto-preferred correction schemes
  ############################################################################################
  # Output corrected data and summary of standards - all correction schemes
  for (k in 1:length(summary.std)) {
    curFile.data.cor = paste(path.out,
                             "R",
                             prefix,
                             "_corData_",
                             plot.col.byCorr.names[k],
                             ".csv",
                             sep = "")
    curFile.data.cor.line.stats = paste(
      path.out,
      "R",
      prefix,
      "_corDataLineStats_",
      plot.col.byCorr.names[k],
      ".csv",
      sep = ""
    )
    curFile.summary.std = paste(path.out,
                                "R",
                                prefix,
                                "_summaryStd_",
                                plot.col.byCorr.names[k],
                                ".csv",
                                sep = "")
    write.csv(data.cor[[k]], curFile.data.cor, row.names = FALSE)
    write.csv(data.cor.line.stats[[k]],
              curFile.data.cor.line.stats,
              row.names = FALSE)
    write.csv(summary.std[[k]], curFile.summary.std, row.names = FALSE)
  }
  
  
  ############################################################################################
  # Set up input for data post-processing program
  ############################################################################################
  # prefer.auto is set as:
  # 1 - basic
  # 2 - linear
  # 3 - segmented - basic
  # 4 - segmented - linear
  
  if (segment.IPL[1, 1] == -9999) { # there are no segments
    # Start off by assuming only basic correction
    prefer.auto = 1 # basic correction preferred
    numSeg = 0 # There are no segments
    # If the rsq threshold was met, switch to the linear correction
    if (data.cor.line.stats[[2]][3, 2] > threshold.rsq &
        data.cor.line.stats[[2]][3, 3] > threshold.rsq) {
      prefer.auto = 2
    }
  } else { # segments exist
    prefer.auto = 3
    numSeg = length(segment.IPL.numList) - 1 # total number of segments to look for
    # If there are segments, add in the auto-preferred ones
    for (k in 1:numSeg) {
      # start off by assuming basic correction is preferred
      prefer.auto.segChoice = 1 # basic correction preferred for this segment
      # If the rsq threshold was met for this segment, switch to the linear correction
      if (data.cor.line.stats[[2+numSeg+k]][3, 2] > threshold.rsq &
          data.cor.line.stats[[2+numSeg+k]][3, 3] > threshold.rsq) { # '2+2' is for basic, linear, and segment-basic corrections. These are not used here
        prefer.auto.segChoice = 2
      }
      assign(paste("prefer.auto.seg", k, sep = ""), prefer.auto.segChoice)
    }
  }
  # Import and update the postProcessInput file
  # Pick the correct file - for JHU or UM data
  if (JHUorUM == 1){ # this is JHU data
    path.post <-  paste(path.out, "postProcessInputJHU.csv", sep = "")
  } else{ # this is UM data
    path.post <-  paste(path.out, "postProcessInput.csv", sep = "")
  }
  data.post <- read.csv(path.post, skip = 0)
  data.post$numSeg[reactor.file.number] = numSeg # How many segments does reactor have
  data.post$prefer.auto[reactor.file.number] = prefer.auto # auto-preferred calibration
  data.post$last.update[reactor.file.number] = format(Sys.Date(), "%Y%m%d") # time of last update (YYYYMMDD). Keeping this as a number avoids an error when saving in excel, which will probably be done.
  # Set the prefered calibrations for each segment
  if (numSeg>0){
    for (k in 1:numSeg) {
      data.post[[paste("prefer.auto.seg", k, sep = "")]][[reactor.file.number]] <-
        get(paste("prefer.auto.seg", k, sep = ""))
    }
  }
  
  # Reset all of the user-required values each time data is corrected ("prefer.user..."). These control the correction scheme used for data post-processing. This is done to force the user to verify the correction scheme they want by hand before data can be post-processed.
  data.post$prefer.user[reactor.file.number] = -9999 # no data flag
  for (k in 1:5) {
    # currently there are 5 allowed segments in a single run. If there are ever more, update this value
    data.post[[paste("prefer.user.seg", k, sep = "")]][[reactor.file.number]] <-
      -9999
  }
  
  # Export the revised file
  write.csv(data.post, path.post, row.names = FALSE)
  
  # Reminder that to sort the corrected data, user needs to select the preferred correction scheme.
  print(paste("Reminder: select preferred correction for Reactor ", reactor.file.number, " in postProcessInput.csv",sep=""))
}



