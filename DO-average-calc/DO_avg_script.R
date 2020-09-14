# DO Daily Average Calculation
# calculates DO average for previous day(s) from continuous sonde and appends to output text file
# questions: seperry83@gmail.com

# import packages
suppressMessages(library(R.utils))

# --- User Defined Values ---
# threshold value
threshold <- 5.5 # mg/L

# --- Read in Data ---
# filepath for DO data (ie. txt file to write data to)
# assumes connection to S: drive
fp_DOdata <- 'S:/M & A BRANCH/Discrete EMP/Code/DO-average-calc/DO_Check.txt'

# define sonde fp (shouldn't change)
fp_sonde <- 'S:/OMBD/RRI.TXT'

# determine how far back to read file (1 day = ~100 lines) 
keepLines <- 1500 # lines
numLines <- R.utils::countLines(fp_sonde)

# import sonde data
sondeData <- read.csv(fp_sonde,
                      header = FALSE,
                      skip = numLines - keepLines)

# import DO Data (to determine which dates avg needs to be calc'd for)
doData <- read.csv(fp_DOdata,
                   header = FALSE,
                   sep = '\t',
                   skip = R.utils::countLines(fp_DOdata) - 1)

# --- Define date range ---
# define yesterday's date
yesterday <- as.Date(Sys.Date()-1)

# define last day avg was calc'd
lastDay <- as.Date(tail(doData,1)$V1)

# create vector consisting of dates to calc avg for
if(yesterday == lastDay) {
  {stop('DO values are up to date')}
} else {
  allDates = as.character(seq(lastDay+1, yesterday, 'days'))
}

# --- Populate Avg df ---
# create empty lists for relevant values
dayVals <- vector('list',length(allDates))
dayAvg <- vector('list',length(allDates))
criteria <- vector('list',length(allDates))

# looping over the days
for (iDay in 1:length(allDates)) {
  
  # extract relevant values and append to dayDates 
  for(r in 1:nrow(sondeData)) {
    if(sondeData$V2[r] == allDates[[iDay]]) { 
      timeMin <- min(sondeData$V10[r],sondeData$V11[r],sondeData$V12[r]) # 10-12 are DO vals; grab min
      dayVals[[iDay]] <- c(dayVals[[iDay]],timeMin) # append the min val from the 3 depth readings
    }
  
  # convert NaN to NA  
  dayVals[[iDay]][is.nan(dayVals[[iDay]])] <- NA
  dayVals[[iDay]][isZero(dayVals[[iDay]])] <- NA
  
  }
  
  # calculate average of dayDates vector (ignoring NA)
  dayAvg[[iDay]] = round(mean(dayVals[[iDay]], na.rm=TRUE), 2) #avg of the min values
  
  # determine pass/fail for DO check
  criteria[[iDay]] = ifelse(dayAvg[[iDay]] >= threshold, 'Passed', 'Failed')
  
  # create the df
  outputDf <- data.frame(date = allDates[[iDay]],
                         DO = dayAvg[[iDay]],
                         thres = threshold,
                         passed = criteria[[iDay]])
  
  # --- Export Data ---
  #append value to output text file
  write.table(outputDf,
              fp_DOdata,
              append = TRUE,
              sep = '\t',
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE)
}
