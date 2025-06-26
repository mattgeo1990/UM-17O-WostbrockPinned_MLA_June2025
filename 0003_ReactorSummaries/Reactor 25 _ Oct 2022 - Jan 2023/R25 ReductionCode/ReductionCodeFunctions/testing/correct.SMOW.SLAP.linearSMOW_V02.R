# TEHuth 1/2/2021
# This is a test file: Now attempting to run normalize smow and slap independently of each other. Each will have it's own linear correction.

# write.csv(  slap, "D:/Documents/000_Michigan/test files/test.csv", row.names = FALSE)


# This function runs the 'linear' SMOW-SLAP correction on a reactor dataset. This is taken from Phoebe's IPL_17O_corrections_TEHuth_V01.Rmd code and modified to be a function.
# TEHuth 2/9/2020

# Inputs
# smow - The SMOW analyses - all columns as from Excel
# slap - The SLAP analyses - all columns as from Excel
correct.SMOW.SLAP.linearSMOW_V02 <- function(D17O.data) {
  
  # Rename the data file to match below and subset to good data
  D17O.data.linearSMOW <- subset(D17O.data, flag==0)
  
  #subset good smow and slap data. 
  #flag: 0 = good; 1 = bad
  smow <- subset(D17O.data.linearSMOW, Type.2=="SMOW" & flag==0)
  slap <- subset(D17O.data.linearSMOW, Type.2=="SLAP" & flag==0)
  
  
  # 1. DEFINE CONSTANTS
  # d33.smow_ref <- mean(smow$d33) #SMOW!Z4
  # d34.smow_ref <- mean(smow$d34) #SMOW!AA4
  
  slope.MWL <- 0.528
  
  d18O.slap <- -55.5 #SMOW!AC4
  d17O.slap <- (exp(slope.MWL*log(d18O.slap/1000+1))-1)*1000 #SMOW!AB4
  
  # Do linear regression via time of analysis
  # Date.Time.smow.num = as.numeric(parse_date_time(smow$Date.Time, "mdy HM"))
  # Date.Time.slap.num = as.numeric(parse_date_time(slap$Date.Time, "mdy HM"))
  # Date.Time.num = as.numeric(parse_date_time(D17O.data.linearSMOW$Date.Time, "mdy HM"))
  
  Date.Time.smow.num = as.numeric(ymd_hms(smow$Date.Time))
  Date.Time.slap.num = as.numeric(ymd_hms(slap$Date.Time))
  Date.Time.num = as.numeric(ymd_hms(D17O.data.linearSMOW$Date.Time))
  
  # # Do linear regression via IPL number (proxy for time of analysis)
  # Date.Time.smow.num = as.numeric(smow$IPL.num)
  # Date.Time.slap.num = as.numeric(slap$IPL.num)
  # Date.Time.num = as.numeric(D17O.data.linearSMOW$IPL.num)
  
  # 1a. DEFINE LINEAR REGRESSION on SMOW d33 and d34 data vs. time
  # Use smow- and slap-defined slope (all notmalized to mean=0) and the smow intercept
  x <- c(smow$d33-mean(smow$d33), slap$d33-mean(slap$d33))
  y <- c(smow$d33-mean(smow$d34), slap$d33-mean(slap$d34))
  time.smow.slap = c(Date.Time.smow.num, Date.Time.slap.num)
  
  lm.time.d33.smow_x <- lm(x  ~ time.smow.slap) # generate linear model: time of analysis vs. observed smow d33
  lm.time.d33.smow.slope <- as.numeric(lm.time.d33.smow_x$coefficients[2]) #pull slope from linear model

  lm.time.d34.smow_y <- lm(y  ~ time.smow.slap) # generate linear model: time of analysis vs. observed smow d33
  lm.time.d34.smow.slope <- as.numeric(lm.time.d34.smow_y$coefficients[2]) #pull slope from linear model
  
  lm.time.d33.smow <- lm(smow$d33  ~ Date.Time.smow.num) # generate linear model: time of analysis vs. observed smow d33
  # lm.time.d33.smow.slope <- as.numeric(lm.time.d33.smow$coefficients[2]) #pull slope from linear model
  lm.time.d33.smow.intercept <- as.numeric(lm.time.d33.smow$coefficients[1]) #pull intercept from linear model
  lm.time.d33.smow.rsq <- summary(lm.time.d33.smow)$r.squared
  # return(lm.time.d33.smow.cor)
  
  lm.time.d34.smow <- lm(smow$d34  ~ Date.Time.smow.num) # generate linear model: time of analysis vs. observed smow d33
  # lm.time.d34.smow.slope <- as.numeric(lm.time.d34.smow$coefficients[2]) #pull slope from linear model
  lm.time.d34.smow.intercept <- as.numeric(lm.time.d34.smow$coefficients[1]) #pull intercept from linear model
  lm.time.d34.smow.rsq <- summary(lm.time.d34.smow)$r.squared
  # return(lm.time.d34.smow.cor)
  
  # Make a list of the output data
  D17O.line.stats.linearSMOW <- data.frame(matrix(ncol=3,nrow=3))
  x <- c("variable", "d33", "d34")
  colnames(D17O.line.stats.linearSMOW) <- x
  D17O.line.stats.linearSMOW[,1] = c("slope", "intercept", "rsq")
  D17O.line.stats.linearSMOW[,2] = c(lm.time.d33.smow.slope, lm.time.d33.smow.intercept, lm.time.d33.smow.rsq)
  D17O.line.stats.linearSMOW[,3] = c(lm.time.d34.smow.slope, lm.time.d34.smow.intercept, lm.time.d34.smow.rsq)
  
  # 2. NORMALIZE TO SMOW - USING LINEAR REGRESSION ABOVE
  #smow.osberved 
  smow$d33.smow_ref <- (((smow$d33/1000+1)/(((lm.time.d33.smow.slope*Date.Time.smow.num+lm.time.d33.smow.intercept)/1000+1))-1)*1000)
  smow$d34.smow_ref <- (((smow$d34/1000+1)/(((lm.time.d34.smow.slope*Date.Time.smow.num+lm.time.d34.smow.intercept)/1000+1))-1)*1000)
  
  #slap.observed
  slap$d33.smow_ref <- (((slap$d33/1000+1)/(((lm.time.d33.smow.slope*Date.Time.slap.num+lm.time.d33.smow.intercept)/1000+1))-1)*1000)
  slap$d34.smow_ref <- (((slap$d34/1000+1)/(((lm.time.d34.smow.slope*Date.Time.slap.num+lm.time.d34.smow.intercept)/1000+1))-1)*1000)
  
  # THESE ARE THE BASIC SMOW NORMALIZATIONS FOR REFERENCE. KEEP THESE COMMENTED.
  # # 2. NORMALIZE TO SMOW
  # #smow.osberved 
  # smow$d33.smow_ref <- ((((smow$d33/1000)+1)/((d33.smow_ref/1000)+1))-1)*1000
  # #=((((N17/1000)+1)/((SMOW!$Z$4/1000)+1))-1)*1000
  # #$N17 = measured d33
  
  # smow$d34.smow_ref <- ((((smow$d34/1000)+1)/((d34.smow_ref/1000)+1))-1)*1000
  # #=((((P18/1000)+1)/((SMOW!$AA$4/1000)+1))-1)*1000
  # #P18 = measured d34
  
  # #slap.observed
  # slap$d33.smow_ref <- ((((slap$d33/1000)+1)/((d33.smow_ref/1000)+1))-1)*1000
  # slap$d34.smow_ref <- ((((slap$d34/1000)+1)/((d34.smow_ref/1000)+1))-1)*1000
  
  # 3. GENERATE SMOW-SLAP TRANSFER FUNCTIONS
  #d17O linear model
  d17O.smow.accepted <- 0 #defined as 0
  d17O.smow.observed <- mean(smow$d33.smow_ref) #mean of observed smow
  d17O.slap.accepted <- d17O.slap #defined from slap d18O (-55.5) and slope.MWL
  d17O.slap.observed <- mean(slap$d33.smow_ref) #mean of observed slap
  lm.d17O.smow.slap <- lm(c(d17O.smow.accepted,d17O.slap.accepted) ~ c(d17O.smow.observed,d17O.slap.observed)) #generate linear model btw smow and slap accepted and observed values
  d17O.smow.slap.slope <- lm.d17O.smow.slap$coefficients[2] #pull slope from linear model
  d17O.smow.slap.intercept <- lm.d17O.smow.slap$coefficients[1] #pull intercept from linear model
  
  #d18O linear model 
  d18O.smow.accepted <- 0 
  d18O.smow.observed <- mean(smow$d34.smow_ref)
  d18O.slap.accepted <- -55.5 
  d18O.slap.observed <- mean(slap$d34.smow_ref) 
  lm.d18O.smow.slap <- lm(c(d18O.smow.accepted,d18O.slap.accepted) ~ c(d18O.smow.observed,d18O.slap.observed))
  d18O.smow.slap.slope <- lm.d18O.smow.slap$coefficients[2]
  d18O.smow.slap.intercept <- lm.d18O.smow.slap$coefficients[1]
  
  # 4. STRETCH TO SLAP
  smow$d17O.slap <- smow$d33.smow_ref*d17O.smow.slap.slope
  #=Z17*SMOW!$AN$6
  #Z17=smow$d33.smow_ref
  #SMOW!AN$6 = slope of the d17O linear model
  
  smow$d18O.slap <- smow$d34.smow_ref*d18O.smow.slap.slope
  #=AA17*SMOW!$AN$12
  #AA17 = smow$d34.smow_ref
  #SMOW!AN$12 = slop of the d18O linear model 
  
  slap$d17O.slap <- slap$d33.smow_ref*d17O.smow.slap.slope
  slap$d18O.slap <- slap$d34.smow_ref*d18O.smow.slap.slope  
  
  # 5. CALCULATE delta primes, D17O
  smow$dp17O.final <- log((smow$d17O.slap/1000)+1)*1000
  smow$dp18O.final <- log((smow$d18O.slap/1000)+1)*1000
  smow$D17O.final <- smow$dp17O.final - slope.MWL*smow$dp18O.final
  smow$D17O.per.meg <- smow$D17O.final * 1000
  
  slap$dp17O.final <- log((slap$d17O.slap/1000)+1)*1000
  slap$dp18O.final <- log((slap$d18O.slap/1000)+1)*1000
  slap$D17O.final <- slap$dp17O.final - slope.MWL*slap$dp18O.final
  slap$D17O.per.meg <- slap$D17O.final * 1000
  
  # ```
  # 
  # 
  # #### Correct Unknowns
  # Same steps as the standard data, but for unknowns:  
  #   1. normalize to smow  
  # 2. stretch to slap  
  # 3. calculate $\Delta$^17^O
  # ```{r}
  D17O.data.linearSMOW$d33.smow_ref <- ((((D17O.data.linearSMOW$d33/1000)+1)/(((lm.time.d33.smow.slope*Date.Time.num+lm.time.d33.smow.intercept)/1000)+1))-1)*1000
  #=((((N3/1000)+1)/((SMOW!$Z$4/1000)+1))-1)*1000
  #N3=measured d33
  #SMOW Z4 = d33.smow_ref
  
  D17O.data.linearSMOW$d34.smow_ref <- ((((D17O.data.linearSMOW$d34/1000)+1)/(((lm.time.d34.smow.slope*Date.Time.num+lm.time.d34.smow.intercept)/1000)+1))-1)*1000 
  #=((((P3/1000)+1)/((SMOW!$AA$4/1000)+1))-1)*1000
  #P3 = meausured d34
  #SMOW AA4 = d34.smow_ref
  
  D17O.data.linearSMOW$d17O.slap <- D17O.data.linearSMOW$d33.smow_ref*d17O.smow.slap.slope
  #=Z3*SMOW!$AN$6
  #Z3 = D17O.data$d33.smow_ref
  #SMOW!$AN$6 = slope of d17O linear model 
  
  D17O.data.linearSMOW$d18O.slap <- D17O.data.linearSMOW$d34.smow_ref*d18O.smow.slap.slope
  #=AA3*SMOW!$AN$12
  #AA3 = D17O.data$d34.smow_ref
  #SMOW!$AN$12 = slope of d18O linear model
  
  D17O.data.linearSMOW$dp17O.final <- log((D17O.data.linearSMOW$d17O.slap/1000)+1)*1000
  D17O.data.linearSMOW$dp18O.final <- log((D17O.data.linearSMOW$d18O.slap/1000)+1)*1000
  D17O.data.linearSMOW$D17O.final <- D17O.data.linearSMOW$dp17O.final - slope.MWL*D17O.data.linearSMOW$dp18O.final
  D17O.data.linearSMOW$D17O.per.meg <- D17O.data.linearSMOW$D17O.final*1000
  
  # Now output the corrected data
  D17O.data.linearSMOW.cor = D17O.data.linearSMOW
  return(list(name1=D17O.data.linearSMOW.cor, name2=D17O.line.stats.linearSMOW))
  #  return(D17O.data.linearSMOW)q)
  
  
}