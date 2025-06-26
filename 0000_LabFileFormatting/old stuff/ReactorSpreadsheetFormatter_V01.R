# ---
#   title: "17O Spreadsheet Format Updates"
# author: "IsoPaleoLab"
# date: "`r (Sys.time())`"
# output: .csv
# ---
library(tidyverse)
library(dplyr)
library(lubridate)
library(knitr)
library(stringr)
library(readxl)


#Reactor Files - Add new reactor spreadsheets here when they're finished - copy and paste name of xlsx sheet
R1 = "REACTOR ONE_organizedIZW_7-22-19.xlsx"
R2 = "REACTOR TWO_organizedIZW_7-22-19.xlsx"
R3 = "REACTOR THREE_organizedIZW_7-22-19.xlsx"
R4 = "Cap17O Compiled REACTOR FOUR_combined_20200305.xlsx"
R5 = "Cap17O Compiled REACTOR FIVE 181205.xlsx"
R6 = "Cap17O Compiled REACTOR SIX 181119.xlsx"
R7 = "Cap17O Compiled REACTOR SEVEN 190122.xlsx"
R8 = "Cap17O Compiled REACTOR EIGHT 190409.xlsx"
R9 = "Cap17O Compiled REACTOR NINE 190708.xlsx"
R10 = "Cap17O Compiled REACTOR TEN 190703.xlsx"
R11 = "Cap17O Compiled REACTOR ELEVEN 190814.xlsx"
R12 = "Cap17O Compiled REACTOR TWELVE 191004.xlsx"
R13 = "Cap17O Compiled REACTOR THIRTEEN 191121.xlsx"
R14 = "Cap17O Compiled REACTOR FOURTEEN 200316.xlsx"

reactor.file.names <-
  c(R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14) #Also add variable name here for newly finished reactors

##############################################################################################
##############################################################################################
##############################################################################################
##############################################################################################
#Go through all reactor.file.names one by one and run full code for each
for (k in 1:14) {
  #:length(reactor.file.names)) {
  # clear all previous variables EXCEPT the reactor file names
  rm(list = ls()[!ls() %in% c("reactor.file.names", "k")])
  
  ##FILE GRAB## r
  curr.reactordata <- reactor.file.names[k]
  
  #### Set input and output files ####
  # Input path
  path.to.data.files <- "C:/Users/Tyler/Documents/000_Michigan/Laboratory Data Files/Data Reduction Procedure/0000_LabFileFormatting/000_Reactor Spreadsheet Raw/"
  #need to have the / at the end of this line ^ to get into the folder 
  setwd(path.to.data.files) 
  curr.reactorsafe <- read_excel(curr.reactordata,col_names = T)
  curr.reactor <- curr.reactorsafe
  
  # Output path
  #define a plot path for output files (where do you want them to go?)
  plot.path <- "C:/Users/Tyler/Documents/000_Michigan/Laboratory Data Files/Data Reduction Procedure/0000_LabFileFormatting/001_Reactor Spreadsheet Formatted/"
  
  setwd(path.to.data.files)
  curr.reactorsafe <-
    read_excel(curr.reactordata, col_names = T) # Imports file, unused
  curr.reactor <- curr.reactorsafe # This file is modified
  
  # Rename columns to get rid of spaces and match what the data correction script requires
  names(curr.reactor) = c(
    "IPL.num",
    "User",
    "Type.1",
    "Type.2",
    "NAME",
    "d17O",
    "d.17O",
    "d17O.err",
    "d18O",
    "d.18O",
    "d18O.err",
    "CAP.17O",
    "CAP17O.err",
    "d33",
    "d33.err",
    "d34",
    "d34.err",
    "d35",
    "d35.err",
    "d36",
    "d36.err",
    "Date.Time",
    "version",
    "33.mismatch.R2",
    "34.mismatch.R2",
    "d33.SMOW.REF",
    "d34.SMOW.REF",
    "d17O.SLAP",
    "d18O.SLAP",
    "d.17O.Final",
    "d.18O.Final",
    "D17O.Final",
    "D17O.per.meg",
    "Average",
    "Stdev",
    "comments",
    "reactor.ID",
    "primes",
    "flag.major",
    "flag.analysis"
  )
  
  #Pull out reactor number (Reactor 4 was split into two files as the reference gas was changed during the reactor)
  reactor.number <- curr.reactor$reactor.ID[4]
  if (reactor.number == "04a" | reactor.number == "04b") {
    reactor.number.short <- 4 #assign reactor number as 4
  } else {
    reactor.number.short <- as.numeric(reactor.number)
  }
  
  ###########################
  # Separate the actual sample ID from other information (reactor number, replicate, etc.)
  # Make holding column for the "NAME" column - will need later
  NAMEfull.hold = curr.reactor$NAME
  
  #Split Name column into these three
  curr.reactor <- curr.reactor %>%
    separate(NAME, c("Data.File", "IPL.ID", "Samp.ID"), extra = "merge", " ")
  
  if (reactor.number.short < 6) { # Reactors before 6 have a different naming convention
    reactor.details <- separate(
      curr.reactor,
      Samp.ID,
      into = c(
        "block1",
        "block2",
        "block3",
        "block4",
        "block5",
        "Replicate"
      ),
      sep = "-",
      fill = "left"
    )
    #add new column for new name - goes straight to the end
    reactor.details$sample.ID <- rep(NA, nrow(reactor.details))
    
    #loop through reactor.details and clear out "NA"s from block columns and combine the blocks that are filled
    #for loop is best thing to do here
    #loop using is.na function to combine all blocks that have a value, pass over blocks w/ NA
    #i is good for using as a variable, for(i in 1:nrow(reactor.details)) just means "for whatever row you're in in reactor.details..."
    for (i in 1:nrow(reactor.details)) {
      #combine all blocks
      reactor.details$sample.ID[i] <-
        paste(
          reactor.details$block1[i],
          reactor.details$block2[i],
          reactor.details$block3[i],
          reactor.details$block4[i],
          reactor.details$block5[i],
          sep = "-"
        )
      
      #if block1 is NA, paste block 2,3,4,5,6 separated by -
      if (is.na(reactor.details$block1[i] == TRUE)) {
        reactor.details$sample.ID[i] <-
          paste(
            reactor.details$block2[i],
            reactor.details$block3[i],
            reactor.details$block4[i],
            reactor.details$block5[i],
            sep = "-"
          )
      }
      #if blocks 1, 2 are NA, paste block 3,4,5,6
      if (is.na(reactor.details$block1[i] == TRUE &
                reactor.details$block2[i] == TRUE)) {
        reactor.details$sample.ID[i] <-
          paste(
            reactor.details$block3[i],
            reactor.details$block4[i],
            reactor.details$block5[i],
            sep = "-"
          )
      }
      #if blocks 1,2,3 are NA, paste block 4,5,6
      if (is.na(
        reactor.details$block1[i] == TRUE &
        reactor.details$block2[i] == TRUE &
        reactor.details$block3[i] == TRUE
      )) {
        reactor.details$sample.ID[i] <-
          paste(reactor.details$block4[i],
                reactor.details$block5[i],
                sep = "-")
      }
      
      #if blocks 1,2,3,4 are NA, paste block 5,6
      if (is.na(
        reactor.details$block1[i] == TRUE &
        reactor.details$block2[i] == TRUE &
        reactor.details$block3[i] == TRUE &
        reactor.details$block4[i] == TRUE
      )) {
        reactor.details$sample.ID[i] <-
          paste(reactor.details$block5[i], sep = "-")
      }
    }
    replicate.HOLD <- reactor.details$Replicate
    reactor.details$block1 <-
      reactor.details$block2 <-
      reactor.details$block3 <-
      reactor.details$block4 <-
      reactor.details$block5 <- reactor.details$Replicate <- NULL
  }  else { # For the rest of the reactors
    #separate Samp.ID into more enough components to hold the longest sample name (the ones with the most hyphens in this case)
    #Make a new table to preserve curr.reactor
    #Remember how to use the fill = function - has to be = to "word"
    reactor.details <- separate(
      curr.reactor,
      Samp.ID,
      into = c(
        "block1",
        "block2",
        "block3",
        "block4",
        "Reactor",
        "Replicate"
      ),
      sep = "-",
      extra = "merge",
      fill = "left"
    )
    
    #add new column for new name - goes straight to the end
    reactor.details$sample.ID <- rep(NA, nrow(reactor.details))
    
    #loop through reactor.details and clear out "NA"s from block columns and combine the blocks that are filled
    #for loop is best thing to do here
    #loop using is.na function to combine all blocks that have a value, pass over blocks w/ NA
    #i is good for using as a variable, for(i in 1:nrow(reactor.details)) just means "for whatever row you're in in reactor.details..."
    for (i in 1:nrow(reactor.details)) {
      #combine all blocks
      reactor.details$sample.ID[i] <-
        paste(
          reactor.details$block1[i],
          reactor.details$block2[i],
          reactor.details$block3[i],
          reactor.details$block4[i],
          sep = "-"
        )
      
      #if block1 is NA, paste block 2,3,4 separated by -
      if (is.na(reactor.details$block1[i] == TRUE)) {
        reactor.details$sample.ID[i] <-
          paste(
            reactor.details$block2[i],
            reactor.details$block3[i],
            reactor.details$block4[i],
            sep = "-"
          )
      }
      #if blocks 1, 2 are NA, paste block 3,4
      if (is.na(reactor.details$block1[i] == TRUE &
                reactor.details$block2[i] == TRUE)) {
        reactor.details$sample.ID[i] <-
          paste(reactor.details$block3[i],
                reactor.details$block4[i],
                sep = "-")
      }
      #if blocks 1,2,3 are NA, paste block 4
      if (is.na(
        reactor.details$block1[i] == TRUE &
        reactor.details$block2[i] == TRUE &
        reactor.details$block3[i] == TRUE
      )) {
        reactor.details$sample.ID[i] <- paste(reactor.details$block4[i])
      }
    }
    replicate.HOLD <- reactor.details$Replicate
    reactor.details$block1 <-
      reactor.details$block2 <-
      reactor.details$block3 <-
      reactor.details$block4 <-
      reactor.details$Reactor <- reactor.details$Replicate <- NULL
  }
  
  # Add in the replicate information to the dataframe
  reactor.details$rep.num <- replicate.HOLD
  # Add in column for the full sample name
  reactor.details$Name.full <- rep(NA, nrow(reactor.details))
  reactor.details$Name.full <- paste(NAMEfull.hold)
  
  
  # Assign group numbers to each sample (each set of replicates of a sample).
  # Do standards first. These are done separately from the unknowns because the Type.2 is a drop-down menu that mitigates any spelling errors in sample ID
  uniq.Type.2 <-
    unique(reactor.details$Type.2[reactor.details$Type.1 == "WaterStd" |
                                      reactor.details$Type.1 == "CarbonateStd"])
  uniq.Type.2 <- uniq.Type.2[!is.na(uniq.Type.2)]
  # Initialize the Group# column
  reactor.details$group.num <-
    matrix(0, nrow = nrow(reactor.details), ncol = 1)
  for (kj in 1:length(uniq.Type.2)) {
    # Find current subset of analyses
    curSamp <- subset(reactor.details, Type.2 == uniq.Type.2[kj])
    curSamp$group.num <- matrix(0, nrow = nrow(curSamp), ncol = 1)
    # Now assign group numbers by checking for near-sequential IPL numbers (change of <2 accounts for one non-sequential IPL#)
    curGroup <- 1
    curSamp$group.num[1] <- curGroup # always start with group 1
    if (nrow(curSamp) > 1) {
      # some samples may only have one replicate
      for (kk in 2:nrow(curSamp)) {
        test.sequential <-
          as.numeric(curSamp$IPL.num[kk]) - as.numeric(curSamp$IPL.num[kk - 1])
        if (abs(test.sequential) <= 2) {
          # if the sample is sequential (note some early IPL# are negative, requires absolute value of test)
          curSamp$group.num[kk] <- curGroup
        } else {
          # the sample is not sequential, start a new group
          curGroup <- curGroup + 1
          curSamp$group.num[kk] <- curGroup
        }
      }
    }
    # Assign the group numbers to their correct indices in reactor.details and then move to next standard
    for (kkk in 1:nrow(curSamp)) {
      row.match <-
        as.numeric(match(curSamp$IPL.num[kkk], reactor.details$IPL.num))
      reactor.details$group.num[row.match] <- curSamp$group.num[kkk]
    }
  }
  # Now do the unknowns samples. Note that this is dependent on users consistently entering the same sample name.
  uniq.sample.ID = unique(reactor.details$sample.ID[reactor.details$Type.1 ==
                                                      "Water" |
                                                      reactor.details$Type.1 == "Carbonate" |
                                                      reactor.details$Type.1 == "Apatite"])
  uniq.sample.ID <- uniq.sample.ID[!is.na(uniq.sample.ID)]
  for (kj in 1:length(uniq.sample.ID)) {
    # Find current subset of analyses
    curSamp <- subset(reactor.details, sample.ID == uniq.sample.ID[kj])
    curSamp$group.num <- matrix(0, nrow = nrow(curSamp), ncol = 1)
    # Now assign group numbers by checking for near-sequential IPL numbers (change of <2 accounts for one non-sequential IPL#)
    curGroup <- 1
    curSamp$group.num[1] <- curGroup # always start with group 1
    if (nrow(curSamp) > 1) {
      # some samples may only have one replicate
      for (kk in 2:nrow(curSamp)) {
        test.sequential <-
          as.numeric(curSamp$IPL.num[kk]) - as.numeric(curSamp$IPL.num[kk - 1])
        if (abs(test.sequential) <= 2) {
          # if the sample is sequential (note some early IPL# are negative, requires absolute value of test)
          curSamp$group.num[kk] <- curGroup
        } else {
          # the sample is not sequential, start a new group
          curGroup <- curGroup + 1
          curSamp$group.num[kk] <- curGroup
        }
      }
    }
    # Assign the group numbers to their correct indices in reactor.details and then move to next sample
    for (kkk in 1:nrow(curSamp)) {
      row.match <-
        as.numeric(match(curSamp$IPL.num[kkk], reactor.details$IPL.num))
      reactor.details$group.num[row.match] <- curSamp$group.num[kkk]
    }
  }
  
  # Make final dataframe - reorder the columns for ease of viewing and exclude unnecessary columns
  reactor.final <- select(
    reactor.details,
    "reactor.ID",
    "IPL.num",
    "User",
    "Type.1",
    "Type.2",
    "sample.ID",
    "group.num",
    "rep.num",
    "Date.Time",
    "Data.File",
    "IPL.ID",
    "Name.full",
    "flag.major",
    "flag.analysis",
    "primes",
    "comments",
    "version",
    "d17O",
    "d.17O",
    "d17O.err",
    "d18O",
    "d.18O",
    "d18O.err",
    "CAP.17O",
    "CAP17O.err",
    "d33",
    "d33.err",
    "d34",
    "d34.err",
    "d35",
    "d35.err",
    "d36",
    "d36.err",
    "33.mismatch.R2",
    "34.mismatch.R2",
  )
  # And include the "basic" corrected d'18O data, which is used to create a flag column in the data reduction
  reactor.final$d.18O.prelim <- reactor.details$d.18O.Final
  
  # Output the formatted, but uncorrected, data
  out.name <-
    paste(plot.path, "Reactor", reactor.number, "Final.csv", sep = "")
  write.csv(reactor.final,
            out.name,
            na = "",
            row.names = FALSE)
  
  
  
} #End big for loop that goes through reactors one by one
