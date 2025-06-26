# # This function finds segments in a dataset, as identified by a user-defined threshold.
# 
# 
# # TEHuth 2/11/2020
# 
# # Inputs
# # smow - The SMOW analyses - all columns as from Excel

segment.finder <- function(D17O.data.basic.cor,threshold.jump,slap.IPL.threshold) {
  
  # Some of the reactors IPL.num.uniform.uniform that are oddly configured - make them uniformly increase (this is converted back to the actual IPL.num at the end)
  D17O.data.basic.cor$IPL.num.uniform <- 1:nrow(D17O.data.basic.cor)
  
  # First, test to see if the SMOWs exhibit any sudden jumps. This is  done by comparing the average value of each set of SMOWs to the previous set (from the BASIC data reduction).
  # threshold <- 20; # threshold value for D17O jump (per meg), this is defined by tehe user
  # Next, pull out each group of SMOWs from the BASIC data reduction and compare them to each other. Keep track of how many 'jumps' there are.
  smow <- subset(D17O.data.basic.cor, Type.2=="SMOW" & flag==0)
  groupNums.smow = smow$group.num # identify the group numbers of the SMOW analyses
  # Sometimes there will be gaps in the group numbers if all SMOWs for a group are flagged out. Renumber the groups so that they are uniformly increasing
  all.group <- as.numeric(unique(groupNums.smow))
  groupNums.smow.uniform <- smow$group.num*0
  cur.groupNum <- 1
  for (k in 1:length(all.group)){
    test.group <- groupNums.smow==all.group[k]
    groupNums.smow.uniform[test.group] <- k
  }
# and replace the groupNums.smow used internally in this function
  groupNums.smow <- groupNums.smow.uniform
  
  slap <- subset(D17O.data.basic.cor, Type.2=="SLAP" & flag==0)
  
  groupMax <- max(unique(groupNums.smow)) # find the maximum group number
  st.end.smow.IPL = matrix(0,nrow=groupMax,ncol=2)
  for (k in 1:groupMax)
  {
    st.end.smow.IPL[k,1] = as.numeric(min(smow$IPL.num.uniform[groupNums.smow==k]))
    st.end.smow.IPL[k,2] = as.numeric(max(smow$IPL.num.uniform[groupNums.smow==k]))
  }
  groupD17O <- smow$D17O.per.meg # get the associated D17O values
  flag.jump <- matrix(0,nrow=groupMax,ncol=2)
  flag.jump[,1] <- 1:groupMax
  # flag.jump[1,2] <- 0 # redundant, specifies that you can't jump to the first SMOWs
  count <- 0
  for (k in 1:(groupMax-1))
  {
    count = count+1
    b = groupD17O[which(groupNums.smow==k)]
    avg1 <- mean(groupD17O[which(groupNums.smow==k)]) # first group average
    avg2 <- mean(groupD17O[which(groupNums.smow==k+1)]) # second group average
    if(abs(avg1-avg2) > threshold.jump){
      flag.jump[count,2] <- 1
    }
  }
  # Flag each group after a flag - this isolates the "jump" section
  flag.jump[2:nrow(flag.jump),2] <- flag.jump[1:(nrow(flag.jump)-1),2] + flag.jump[2:nrow(flag.jump),2]
  for (km in 1:nrow(flag.jump)){
    if (flag.jump[km,2]>1){
      flag.jump[km,2] <- 1
    }
  }
  # Always flag the final group - this is always a segment due to being last
  flag.jump[nrow(flag.jump),2]<-1
  # Always unflag the first group - can't jump from nowhere
  flag.jump[1,2]<-0
  
  
  # Now find the IPL.num that the segments should cover
  if (sum(flag.jump[,2])>1) { # if any jumps were found, record them (note that the function defines the very last set of SMOWs as a jump automatically, so we need to ignore one flag).
    # Build a matrix of the necessary start and end points for each segment
    cur.start.segment <- st.end.smow.IPL[1,1] # always start with the first smow
    cur.end.segment <- 0 # no current value, this is placeholder
    segment.IPL <- matrix(0,nrow=sum(flag.jump[,2]),ncol=2) # set up the matrix
    count <- 0
    for (k in 2:nrow(flag.jump)){
      if ((flag.jump[k,2])==1){ # if there is a jump between these SMOWs and next SMOWs, record information
        count<-count+1
        cur.end.segment <- st.end.smow.IPL[k,2] # this is the last IPL number of SMOWs
        segment.IPL[count,] <- c(cur.start.segment, cur.end.segment) # fill in current start
        # And set up for the next jump
        cur.start.segment = st.end.smow.IPL[k,1]
        
      }
    }
    # And the very first and last values must be the start and end of the reactor (does not necessarily coincide with SMOW analyses)
    IPL.num.uniform.list.a <- subset(D17O.data.basic.cor, flag<=1 & group.num>0)
    IPL.num.uniform.list.b <- matrix(0,nrow=nrow(IPL.num.uniform.list.a),ncol=1)
    IPL.num.uniform.list.b <- as.numeric(IPL.num.uniform.list.a$IPL.num.uniform)
    segment.IPL[1,1] <- min(IPL.num.uniform.list.b) # first analysis in reactor
    segment.IPL[nrow(segment.IPL),ncol(segment.IPL)] <- max(IPL.num.uniform.list.b) # last analysis in reactor
    
  } else { # If no jumps were found, use a default value
    segment.IPL = matrix(-9999,1,1); # no jumps found
    skip.segment.IPL = matrix(-9999,1,1) # no jumps found, no skips possible
  }
  
  
  
  
  
  
  
  
  # Final adjustments for the segment information
  skip.segment.IPL <- matrix(0,nrow=nrow(segment.IPL),ncol=ncol(segment.IPL)) # set up matrix for screening non-viable jumps (they have no associated SLAPS)
  
  if (sum(flag.jump[,2])>1) { # if any jumps were found, make final adjustments
    # Final segment adjustments if jumps are found
    # Include nearest SLAP analyses
    # Turn uniform IPL numbers back into IPL numbers
    
    # Include the nearest SLAP analyses to each group of SMOWs (if possible). This increases the chance that SLAPs will be found within the segment (note if only SMOWs are in the segment the correction will fail, correctly, in the main function)
    # SLAPs after segment end
    for (k in 1:nrow(segment.IPL)){
      cur.end.segment <- segment.IPL[k,2] # set current end segment value
      slap.test.end <- as.numeric(slap$IPL.num.uniform)<=(cur.end.segment+slap.IPL.threshold) & as.numeric(slap$IPL.num.uniform)>=(cur.end.segment-slap.IPL.threshold)
      if (sum(slap.test.end)>0){ # yes, there are associated SLAPS
        # if SLAPs were after cur.end.segment, change end value
        if (max(as.numeric(slap$IPL.num.uniform[slap.test.end]))>cur.end.segment){ # change the end value of the segment if there are SLAPs after identified segment end
          cur.end.segment <- max(as.numeric(slap$IPL.num.uniform[slap.test.end]))
        }
      } else { # There are no associated SLAPS
        skip.segment.IPL[k,2] <- 1 # flag for bad jump
      }
      
      # SLAPs before segment start
      cur.start.segment <- segment.IPL[k,1] # set current end segment value
      slap.test.start <- as.numeric(slap$IPL.num.uniform)<=(cur.start.segment+slap.IPL.threshold) & as.numeric(slap$IPL.num.uniform)>=(cur.start.segment-slap.IPL.threshold)
      if (sum(slap.test.start)>0){ # yes, there are associated SLAPS
        # cur.start.segment should be the min IPL.num.uniform of the SMOW-SLAP set
        if (min(as.numeric(slap$IPL.num.uniform[slap.test.start]))<cur.start.segment){ # change the start value of the segment if there are SLAPs before identified segment start
          cur.start.segment <- min(as.numeric(slap$IPL.num.uniform[slap.test.start]))
        }
      } else { # There are no associated SLAPS
        skip.segment.IPL[k,1] <- 1 # flag for bad jump
      }
      # Record segment, move to next
      segment.IPL[k,] <- c(cur.start.segment, cur.end.segment) # save the information
    }
    
    # Finally, switch back to the real IPL numbers
    segment.IPL[,] <- as.numeric(D17O.data.basic.cor$IPL.num[segment.IPL])
  }
  
  
  
  
  
  
  
  
  # return the desired parameters
  return(list(name1=segment.IPL, name2=skip.segment.IPL))
}