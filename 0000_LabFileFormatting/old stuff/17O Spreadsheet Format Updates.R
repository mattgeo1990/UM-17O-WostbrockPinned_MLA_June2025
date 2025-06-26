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
R4a = "Cap17O Compiled REACTOR FOUR_Part_1_181206.xlsx"
R4b = "Cap17O Compiled REACTOR FOUR_Part_2_181206.xlsx"
R5 = "Cap17O Compiled REACTOR FIVE 181205.xlsx"
R6 = "Cap17O Compiled REACTOR SIX 181119.xlsx"
R7 = "Cap17O Compiled REACTOR SEVEN 190122.xlsx"
R8 = "Cap17O Compiled REACTOR EIGHT 190409.xlsx"
R9 = "Cap17O Compiled REACTOR NINE 190708.xlsx"
R10 = "Cap17O Compiled REACTOR TEN 190703.xlsx"
R11 = "Cap17O Compiled REACTOR ELEVEN 190814.xlsx"
R12 = "Cap17O Compiled REACTOR TWELVE 191004.xlsx"
R13 = "Cap17O Compiled REACTOR THIRTEEN 191121.xlsx"

reactor.file.names <- c(R1, R2, R3, R4a, R4b, R5, R6, R7, R8, R9, R10, R11, R12, R13) #Also add variable name here for newly finished reactors

#Go through all reactor.file.names one by one and run full code for each
for (k in 1){#:length(reactor.file.names)) {
  # clear all previous variables EXCEPT the reactor file names
  rm(list = ls()[!ls() %in% c("reactor.file.names","k")])
  
  ##FILE GRAB## r
  curr.reactordata <-reactor.file.names[k]
  
  #### Get to the files ####
  path.to.data.files <- "C:/Users/Tyler/Documents/000_Michigan/Laboratory Data Files/Data Reduction Procedure/0000_LabFileFormatting/000_Reactor Spreadsheet Raw/"
  #need to have the / at the end of this line ^ to get into the folder 
  
  setwd(path.to.data.files) 
  curr.reactorsafe <- read_excel(curr.reactordata,col_names = T)
  curr.reactor <- curr.reactorsafe
  #define a plot path for output files (where do you want them to go?)
  plot.path <- "C:/Users/Tyler/Documents/000_Michigan/Laboratory Data Files/Data Reduction Procedure/0000_LabFileFormatting/001_Reactor Spreadsheet Formatted/"
  
  #keep reactor csv files separate from data frames - this way you always keep a version of the original input data (no changes to original spreadsheets)
  #Use reactor# to call data from now on 
  # curr.reactor$X <- curr.reactor$X.1 <- curr.reactor$X.2 <- curr.reactor$X.3 <- curr.reactor$X.4 <- NULL
  ##Same here. When I read csv there were columns X, X.1, etc. added to the end all w/ NA values, this is how you delete them. 
  #curr.reactorsafe still has them on the end 
  
  #curr.reactor 
  
  #Pull out reactor number
  reactor.number <- curr.reactor$ReactorID[4]
  if (reactor.number=="04a" | reactor.number=="04b"){
    reactor.number.short <- 4 #assign reactor number as 4
  } else {
    reactor.number.short <- as.numeric(reactor.number)
  }
  
  ###########################
  
  # Make holding column for the "NAME" column - will need later
  NAME.hold = curr.reactor$NAME
  
  #Split Name column into these three 
  curr.reactor <- curr.reactor %>% 
    separate(NAME,c("Data.File","IPL.ID","Samp.ID"),extra = "merge"," ")
  
  if (reactor.number.short < 6){
    reactor.details <- separate(curr.reactor,Samp.ID,
                                into = c("block1",
                                         "block2",
                                         "block3",
                                         "block4",
                                         "block5",
                                         "Replicate"),
                                sep = "-",
                                fill = "left") 
    #add new column for new name - goes straight to the end 
    reactor.details$Name <- rep(NA,nrow(reactor.details))
    
    #loop through reactor.details and clear out "NA"s from block columns and combine the blocks that are filled 
    #for loop is best thing to do here
    #loop using is.na function to combine all blocks that have a value, pass over blocks w/ NA 
    #i is good for using as a variable, for(i in 1:nrow(reactor.details)) just means "for whatever row you're in in reactor.details..."
    for (i in 1:nrow(reactor.details)) {
      #combine all blocks
      reactor.details$Name[i] <- paste(reactor.details$block1[i],reactor.details$block2[i],reactor.details$block3[i],reactor.details$block4[i],reactor.details$block5[i],sep = "-")
      
      #if block1 is NA, paste block 2,3,4,5,6 separated by - 
      if (is.na(reactor.details$block1[i]==TRUE)) {
        reactor.details$Name[i] <- paste(reactor.details$block2[i],reactor.details$block3[i],reactor.details$block4[i],reactor.details$block5[i],sep = "-")
      }
      #if blocks 1, 2 are NA, paste block 3,4,5,6
      if (is.na(reactor.details$block1[i]==TRUE & reactor.details$block2[i]==TRUE)) {
        reactor.details$Name[i] <- paste(reactor.details$block3[i],reactor.details$block4[i],reactor.details$block5[i],sep = "-")
      }
      #if blocks 1,2,3 are NA, paste block 4,5,6 
      if (is.na(reactor.details$block1[i]==TRUE & reactor.details$block2[i]==TRUE & reactor.details$block3[i]==TRUE)) {
        reactor.details$Name[i] <- paste(reactor.details$block4[i],reactor.details$block5[i],sep = "-")
      }
      
      #if blocks 1,2,3,4 are NA, paste block 5,6 
      if (is.na(reactor.details$block1[i]==TRUE & reactor.details$block2[i]==TRUE & reactor.details$block3[i]==TRUE & reactor.details$block4[i]==TRUE)) {
        reactor.details$Name[i] <- paste(reactor.details$block5[i],sep = "-")
        
      }
      # #if blocks 1,2,3,4,5 are NA, paste block 6 
      # if (is.na(reactor.details$block1[i]==TRUE & reactor.details$block2[i]==TRUE & reactor.details$block3[i]==TRUE & reactor.details$block4[i]==TRUE & reactor.details$block5[i]==TRUE)) {
      #   reactor.details$Name[i] <- paste(reactor.details$Replicate[i])
      # }
    }  
    replicate.HOLD <- reactor.details$Replicate
    reactor.details$block1 <- reactor.details$block2 <- reactor.details$block3 <- reactor.details$block4 <- reactor.details$block5 <- reactor.details$Replicate <- NULL
  }  else {
    #separate Samp.ID into more enough components to hold the longest sample name (the ones with the most hyphens in this case)
    #Make a new table to preserve curr.reactor 
    #Remember how to use the fill = function - has to be = to "word" 
    reactor.details <- separate(curr.reactor,Samp.ID,
                                into = c("block1",
                                         "block2",
                                         "block3",
                                         "block4",
                                         "Reactor",
                                         "Replicate"),
                                sep = "-",
                                extra = "merge",
                                fill = "left") 
    
    #add new column for new name - goes straight to the end 
    reactor.details$Name <- rep(NA,nrow(reactor.details))
    
    #loop through reactor.details and clear out "NA"s from block columns and combine the blocks that are filled 
    #for loop is best thing to do here
    #loop using is.na function to combine all blocks that have a value, pass over blocks w/ NA 
    #i is good for using as a variable, for(i in 1:nrow(reactor.details)) just means "for whatever row you're in in reactor.details..."
    for (i in 1:nrow(reactor.details)) {
      #combine all blocks
      reactor.details$Name[i] <- paste(reactor.details$block1[i],reactor.details$block2[i],reactor.details$block3[i],reactor.details$block4[i],sep = "-")
      
      #if block1 is NA, paste block 2,3,4 separated by - 
      if (is.na(reactor.details$block1[i]==TRUE)) {
        reactor.details$Name[i] <- paste(reactor.details$block2[i],reactor.details$block3[i],reactor.details$block4[i],sep = "-")
      }
      #if blocks 1, 2 are NA, paste block 3,4
      if (is.na(reactor.details$block1[i]==TRUE & reactor.details$block2[i]==TRUE)) {
        reactor.details$Name[i] <- paste(reactor.details$block3[i],reactor.details$block4[i],sep = "-")
      }
      #if blocks 1,2,3 are NA, paste block 4 
      if (is.na(reactor.details$block1[i]==TRUE & reactor.details$block2[i]==TRUE & reactor.details$block3[i]==TRUE)) {
        reactor.details$Name[i] <- paste(reactor.details$block4[i])
      }
    }
    replicate.HOLD <- reactor.details$Replicate
    reactor.details$block1 <- reactor.details$block2 <- reactor.details$block3 <- reactor.details$block4 <- reactor.details$Reactor <- reactor.details$Replicate <- NULL
  }
  
  
  #FUCK YEAH THAT WORKED - MAKE SURE YOU REMEMBER TO HAVE THE [i] AT THE END OF EVERY SINGLE COLUMN YOU CALL THROUGHOUT 
  
  #Remove block columns now that the name is intact (col 39)
  
  
  # #Create flag column - this says replace every row in that column w/ NA - easy way to start it
  # reactor.details$Flag <- rep(NA,nrow(reactor.details))
  
  
  # ################################################################################################################################
  # # NEED TO GO THROUGH SPREADSHEETS AND FLAG SHIT OURSELVES SO WERE NOT JUST PULLING 1/0 FROM COMMENTS COLUMN
  # #Now for/if loop to fill in Flag column w/ 1 or 0
  # reactor.details$Flag <- grepl("[[:alnum:]]", reactor.details$Comments)
  # #Okay... this worked, but spits out T/F, not 1/0... but good to know this is a thing. 
  # ################################################################################################################################
  # 
  # 
  # 
  # #Let's try to convert the T/F in $Flag to 1/0 for ease 
  # reactor.details$Flag <- gsub("TRUE","1",reactor.details$Flag)
  # reactor.details$Flag <- gsub("FALSE","0",reactor.details$Flag)
  # #Fuck yeah 
  
  #reorder the columns
  reactor.details$rep.num <- replicate.HOLD
  reactor.details <- reactor.details[c(1:6,39,38,37,40,41,7:36)]
  names(reactor.details) = c("IPL.num","User","Type.1","Type.2","Data.File","IPL.ID","reactor.ID","flag","comments","sample.ID","rep.num",
                             "d17O","d.17O","d17O.err","d18O","d.18O","d18O.err","CAP.17O","CAP.17O.err","d33","d33.err",
                             "d34","d34.err","d35","d35.err","d36","d36.err","Date.Time","version","X33.mismatch.R2","X34.mismatch.R2",
                             "32","33","34","35","36","37","38","39","40","41")
  
  #save this as a new file 
  reactor.final <- reactor.details
  
  #add new column for new name - goes straight to the end 
  reactor.final$Name <- rep(NA,nrow(reactor.details))
  reactor.final$Name <- paste(NAME.hold)
  
  #re-order columns again - gets rid of unneeded rows
  reactor.final <- reactor.final[c(1:7,42,8:41)]
  
  # Assign group numbers to each sample (each set of replicates of a sample).
  # Do standards first. These are done separately from the unknowns because the Type.2 is a drop-down menu that mitigates any spelling errors in sample ID
  uniq.Type.2 <- unique(reactor.final$Type.2[reactor.final$Type.1=="WaterStd" | reactor.final$Type.1=="CarbonateStd"])
  uniq.Type.2 <- uniq.Type.2[!is.na(uniq.Type.2)]
  # Initialize the Group# column
  reactor.final$group.num <- matrix(0,nrow=nrow(reactor.final),ncol=1)
  for (kj in 1:length(uniq.Type.2)){
    # Find current subset of analyses
    curSamp <- subset(reactor.final,Type.2==uniq.Type.2[kj])
    curSamp$group.num <- matrix(0,nrow=nrow(curSamp),ncol=1)
    # Now assign group numbers by checking for near-sequential IPL numbers (change of <2 accounts for one non-sequential IPL#)
    curGroup <- 1
    curSamp$group.num[1] <- curGroup # always start with group 1
    if (nrow(curSamp)>1){ # some samples may only have one replicate
      for (kk in 2:nrow(curSamp)){
        test.sequential <- as.numeric(curSamp$IPL.num[kk])-as.numeric(curSamp$IPL.num[kk-1])
        if (abs(test.sequential)<=2){ # if the sample is sequential (note some early IPL# are negative, requires absolute value of test)
          curSamp$group.num[kk]<-curGroup
        } else { # the sample is not sequential, start a new group
          curGroup <- curGroup + 1
          curSamp$group.num[kk] <- curGroup
        }
      }
    }
    # Assign the group numbers to their correct indices in reactor.final and then move to next standard
    for (kkk in 1:nrow(curSamp)) {
      row.match <- as.numeric(match(curSamp$IPL.num[kkk],reactor.final$IPL.num))
      reactor.final$group.num[row.match] <- curSamp$group.num[kkk]
    }
  }
  # Now do the unknowns samples. Note that this is dependent on users consistently entering the same sample name.
  uniq.sample.ID = unique(reactor.final$sample.ID[reactor.final$Type.1=="Water" | reactor.final$Type.1=="Carbonate" | reactor.final$Type.1=="Apatite"])
  uniq.sample.ID <- uniq.sample.ID[!is.na(uniq.sample.ID)]
  for (kj in 1:length(uniq.sample.ID)){
    # Find current subset of analyses
    curSamp <- subset(reactor.final,sample.ID==uniq.sample.ID[kj])
    curSamp$group.num <- matrix(0,nrow=nrow(curSamp),ncol=1)
    # Now assign group numbers by checking for near-sequential IPL numbers (change of <2 accounts for one non-sequential IPL#)
    curGroup <- 1
    curSamp$group.num[1] <- curGroup # always start with group 1
    if (nrow(curSamp)>1){ # some samples may only have one replicate
      for (kk in 2:nrow(curSamp)){
        test.sequential <- as.numeric(curSamp$IPL.num[kk])-as.numeric(curSamp$IPL.num[kk-1])
        if (abs(test.sequential)<=2){ # if the sample is sequential (note some early IPL# are negative, requires absolute value of test)
          curSamp$group.num[kk]<-curGroup
        } else { # the sample is not sequential, start a new group
          curGroup <- curGroup + 1
          curSamp$group.num[kk] <- curGroup
        }
      }
    }
    # Assign the group numbers to their correct indices in reactor.final and then move to next standard
    for (kkk in 1:nrow(curSamp)) {
      row.match <- as.numeric(match(curSamp$IPL.num[kkk],reactor.final$IPL.num))
      reactor.final$group.num[row.match] <- curSamp$group.num[kkk]
    }
  }
  # Move the group.num column to appropriate position
  reactor.final <- reactor.final[c(1:12,43,13:42)]
  
  # Get rid of extraneous columns
  reactor.final <- reactor.final[-c(34:43)]
  
  # This isn't working because R1-R5 still have replicate numbers
  # # Assign group numbers to the sample.ID for input to Phoebe's data reduction code
  # # check that all replicates of a sample have the same exact name (also good for initial troubleshooting to see if all analyses of a sample have matching names)
  # # First, get rid of the replicate number - it is not needed
  # sample.ID.short <- data.frame(reactor.final$sample.ID,stringsAsFactors=FALSE)
  # sample.ID.short.unique = unique(sample.ID.short)
  # trouble.unique <- data.frame(sort(sample.ID.short.unique[,1]))
  # # Then get rid of everything after the final hyphen (the replicate number)
  # for (mm in 1:nrow(sample.ID.short)){
  #   cur.short <- sample.ID.short[mm,1] # current name to change
  #   cutoff <- max(unlist(gregexpr(pattern="-",cur.short))) # find the maximum hyphen
  #   cur.short.new <- substr(cur.short,1,cutoff-1) # cut the name short
  #   sample.ID.short[mm,1] <- cur.short.new # replace the name 
  # }
  # 
  # sample.ID.short.unique = unique(sample.ID.short)
  # trouble.unique <- data.frame(sort(sample.ID.short.unique[,1]))
  
  #Create proper sample.ID column for Phoebe's code - Name-R#-run.group
  # No, don't do this - keep sample.ID, reactor.ID, replicate, and group.ID separate
  # reactor.final$reactor.ID <- paste("R",reactor.final$reactor.ID,sep = "")
  # reactor.final$sample.ID <- paste(reactor.final$sample.ID,reactor.final$reactor.ID,sep = "-") 
  
  #
  
  #spit it out!
  out.name <- paste(plot.path,"Reactor",reactor.number,"Final.csv",sep="")
  write.csv(reactor.final,out.name,na="",row.names=FALSE)
  
  
  
} #End big for loop that goes through reactors one by one 







