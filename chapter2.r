##############
############# DO Synthesis Report
############

############
###########  Chapter 2
#########

setwd("C:/Users/jrinde/OneDrive - California Department of Water Resources/DO Modification/Synthesis/Csv files")
library("ggplot2")
library("ggthemes")
library(cowplot)
theme_set(theme_bw())

CM = read.csv(file = "CM.RRI.data.csv")
head(CM)

#subset by station
CM40<- subset(CM, station== "CM40")
CM42<- subset(CM, station== "CM42")
CM43<- subset(CM, station== "CM43")
CM48<- subset(CM, station== "CM48")
RRI<- subset(CM, station== "CM42_RRI")

CM$column_new<-factor(CM$season, levels=c('Summer', 'Fall'))
new<-CM$column_new

## Jenna make/share code to calculate excursions in R for the "percent.exceed.csv

#now with the season order changed make plot
DO.ex = read.csv(file = "percent.exceed.csv")
DO.ex

summer.ex <- subset(DO.ex, season != "fall")
head(summer.ex)
fall.ex <- subset(DO.ex, season != "summer")

summer.explot <- ggplot(data=summer.ex, aes(x=station, y=percent, fill=station, shape=as.factor(station))) + geom_bar(stat="identity") + facet_wrap(~year) + ggtitle("Summer") + theme(legend.position = "none") + labs(y="DO Excursion (%)", x = "Station") + ylim(0,70)
summer.explot

fall.explot <- ggplot(data=fall.ex, aes(x=station, y=percent, fill=station, shape=as.factor(station))) + geom_bar(stat="identity") + facet_wrap(~year) + ggtitle("Fall") + theme(legend.position = "none") + labs(y="DO Excursion (%)", x = "Station") + ylim(0,70)
fall.explot

plot_grid(summer.explot, fall.explot)

#DO Stats
#compute stats for DO study
#output: .csv containing stats
#questions: sarah.perry@water.ca.gov

#import packages
library(tidyverse)
library(lubridate)

#import files
dfRRI <- read_csv('CM.RRI.data.csv')


#round dates
dfRRI$date.time <- mdy_hm(dfRRI$date.time) #convert to datetime object
dfRRI$Date <- format(as.Date(dfRRI$date.time, format = '%m/%d/%Y %H:%m:%s'), '%m/%d/%Y') #date col without time
dfRRI$date.time <- floor_date(dfRRI$date.time, '15 minutes')

#add season col
dfRRI <- dfRRI %>%
  mutate(Season =
           case_when(
             month == 3 | month == 4 | month == 5 ~ 'Spring',
             month == 6 | month == 7 | month == 8 ~ 'Summer',
             month == 9 | month == 10 | month == 11 ~ 'Fall',
             month == 12 | month == 1 | month == 2 ~ 'Winter'
           )
  )

#subset out DO
dfRRI_DO <- subset(dfRRI, select = c(station, day, month, year, Date, Season, DO))

#add summary stats by day
dfRRI_DayStats <- dfRRI_DO %>%   
  group_by(station, day, month, year, Date) %>%  
  summarise(
    Mean = mean(DO, na.rm = TRUE),
    Median = median(DO, na.rm = TRUE),
    `Std Dev` = sd(DO, na.rm = TRUE),
    Max = max(DO, na.rm = TRUE),
    Min = min(DO, na.rm = TRUE),
    `Number of Datapoints` = n())

#cleaup data
dfRRI_DayStats <- dfRRI_DayStats %>% arrange(station, year, month)
dfRRI_DayStats <- subset(dfRRI_DayStats, select = -c(month, year, day))
dfRRI_DayStats$`Std Dev`[is.nan(dfRRI_DayStats$`Std Dev`)] <- NA
dfRRI_DayStats$Mean[is.nan(dfRRI_DayStats$Mean)] <- NA
dfRRI_DayStats$Median[is.nan(dfRRI_DayStats$Median)] <- NA
dfRRI_DayStats$Max[is.infinite(dfRRI_DayStats$Max)] <- NA
dfRRI_DayStats$Min[is.infinite(dfRRI_DayStats$Min)] <- NA

#write file
write_csv(dfRRI_DayStats, 'C:/DOStudy/DO.Daily.Med.csv')


#****NOTE
#in excel added a new column to showr year
#see if Sarah P can show me how to add it in, also make station to Station with a capitol

#Boxplots with Daily Medians

CM.med = read.csv(file = "DO.Daily.Med.csv")


fall <- subset(CM.med, Season != "Summer")
summer <- subset(CM.med, Season != "Fall")


med.sum.box<-ggplot(summer, aes(x=station, y=Median, fill=station, shape=as.factor(station))) +
  geom_boxplot(aes(fill=factor(station))) + facet_wrap(~Year) + ggtitle("Summer Daily Median") + theme(legend.position = "none") + scale_y_continuous(limits = c(2,12), breaks=c(2,4,6,8,10,12)) + labs(y="Dissolved Oxygen (mg/L)", x = "Station") + geom_hline(yintercept=5, linetype="dashed", color = "red")

med.sum.box

med.fall.box<-ggplot(fall, aes(x=station, y=Median, fill=station, shape=as.factor(station))) +
  geom_boxplot(aes(fill=factor(station))) + facet_wrap(~Year) + ggtitle("Fall Daily Median") + theme(legend.position = "none") + scale_y_continuous(limits = c(2,12), breaks=c(2,4,6,8,10,12)) + labs(y="Dissolved Oxygen (mg/L)", x = "Station") + geom_hline(yintercept=6, linetype="dashed", color = "red")

med.fall.box

# Subset by year

summer <- subset(CM.med, Season != "Fall")
sum.2007 <- subset(summer, Year=="2007")
sum.2008 <- subset(summer, Year=="2008")
sum.2009 <- subset(summer, Year=="2009")
sum.2010 <- subset(summer, Year=="2010")


fall <- subset(CM.med, Season != "Summer")
fall.2007 <- subset(fall, Year=="2007")
fall.2008 <- subset(fall, Year=="2008")
fall.2009 <- subset(fall, Year=="2009")
fall.2010 <- subset(fall, Year=="2010")

#Summer comparisons
kruskal.test(Median~station, data = sum.2007) 
pairwise.wilcox.test(sum.2007$Median, sum.2007$station, p.adjust.method = "BH")


kruskal.test(Median~station, data = sum.2008)
pairwise.wilcox.test(sum.2008$Median, sum.2008$station, p.adjust.method = "BH")


kruskal.test(Median~station, data = sum.2009)
pairwise.wilcox.test(sum.2009$Median, sum.2009$station, p.adjust.method = "BH")


kruskal.test(Median~station, data = sum.2010)
pairwise.wilcox.test(sum.2010$Median, sum.2010$station, p.adjust.method = "BH")


#Fall comparisons
kruskal.test(Median~station, data = fall.2007) 
pairwise.wilcox.test(fall.2007$Median, fall.2007$station, p.adjust.method = "BH")


kruskal.test(Median~station, data = fall.2008)
pairwise.wilcox.test(fall.2008$Median, fall.2008$station, p.adjust.method = "BH")


kruskal.test(Median~station, data = fall.2009)
pairwise.wilcox.test(fall.2009$Median, fall.2009$station, p.adjust.method = "BH")

kruskal.test(Median~station, data = fall.2010)
pairwise.wilcox.test(fall.2010$Median, fall.2010$station, p.adjust.method = "BH")

####Section 3, compare CM40 to RRI
#~~~Import Data~~~#
#import packages
library('tidyverse')
library('readxl')
library('lubridate')

#~~~Variables to Edit~~~#
mainPath <- 'C:/R/DO Check/' #new csv's will be saved to here

fileName <- 'CM.RRI.data.csv' #name of data file
#~~~~~~~~~~~~~~~~~~#
#~~~Read In Data~~~#
#~~~~~~~~~~~~~~~~~~#
#data filepath
filePath <- paste(mainPath,fileName,sep = '')

#data filepath
doDatRaw <- read_csv(filePath)

#drop qual column
keepVars <- c('station','date.time','year','season','DO')
doDat <- doDatRaw[keepVars]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~Calculate % Under Threshold~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#grab lists of variables
stations <- unique(doDat$station)
seasons <- unique(doDat$season)
years <- unique(doDat$year)

#initiate empty df
underDf <- NULL

#initiate for loops
for (stat in stations) { #this a for loop, it goes through the list of stations
  for (sea in seasons) { #before moving to next station, goes through list of seasons
    for (yr in years) { #before going to next season, goes through list of years
      
      #create name for saving 
      name <- paste(stat,sea,yr,sep = '')
      
      #create filtered df (overwritten during each iteration of the nested for loop)
      df <-
        doDat %>%
        filter(station == stat &
                 season == sea &
                 year == yr &
                 !is.na(DO))
      
      #assign threshold based on fall/summer
      if (grepl('Fall',name)) { #based on if fall is in the variable 'name'
        threshold <- 6 
      } else {
        threshold <- 5
      }
      
      #do the calculation
      countBelow <- nrow(df[df['DO'] < threshold,]) #comma tells it to call rows, not columns
      countTotal <- nrow(df)
      perUnder <- round(countBelow/(countTotal)*100,2)
      
      #bind row to output df
      underDf <- rbind(underDf, data.frame('station' = name, 'percent under threshold' = perUnder))
    }
  }
}

#create output file path
underPath <- paste(mainPath,'percentUnder.csv',sep = '')

#append output df to text file
write.table(
  underDf,
  underPath,
  sep = ',',
  col.names = TRUE,
  row.names = FALSE,
  quote = FALSE)

```{r}
#~~~~~~~~~~~~~~~~~~~~~~#
#~~~Compare Stations~~~#
#~~~~~~~~~~~~~~~~~~~~~~#
#initiate empty df
compDf <- NULL

for (sea in seasons) {
  for (yr in years) {
    #create name for saving 
    name <- paste(sea,yr,sep = '')
    
    #create filtered dfs
    dfFourty <-
      doDat %>%
      filter(station == 'CM40' &
               season == sea &
               year == yr &
               !is.na(DO))
    
    dfRRI <-
      doDat %>%
      filter(station == 'CM44_RRI' &
               season == sea &
               year == yr &
               !is.na(DO))
    
    
    #change dates columns to type(datetime) (or whatevs R uses :/) and round to nearest 15min
    dfFourty$date.time <- parse_date_time(dfFourty$date.time,'mdy HM')
    dfFourty$date.time <- round_date(dfFourty$date.time, '15 minutes')
    
    #same, but for RRI, accounting for the fact that in '07 it was collected hourly
    if (yr == '2007') {
      dfRRI$date.time <- parse_date_time(dfRRI$date.time,'mdy HM')
      dfRRI$date.time <- round_date(dfRRI$date.time, '1 hour')
    } else {
      dfRRI$date.time <- parse_date_time(dfRRI$date.time,'mdy HM')
      dfRRI$date.time <- round_date(dfRRI$date.time, '15 minutes')
    }
    
    #assign threshold based on fall/summer
    if (grepl('Fall',name)) {
      threshold <- 6
    } else {
      threshold <- 5
    }
    
    #merge the two dfs
    dfMerge <- merge(dfFourty,dfRRI,by = c('season','year','date.time'))
    
    #initalize counts
    doBoth <- 0
    doRRI <- 0
    doFourty <- 0
    
    #start for loop (element-wise :/)
    for (i in 1:nrow(dfMerge)) {
      #populate the count variables
      if ((dfMerge[i,]['DO.x'][[1]] < threshold) && (dfMerge[i,]['DO.y'][[1]] < threshold)) {
        doBoth = doBoth + 1
      } else if ((dfMerge[i,]['DO.x'][[1]] < threshold) && (dfMerge[i,]['DO.y'][[1]] >= threshold)) {
        doFourty = doFourty + 1
      } else if ((dfMerge[i,]['DO.x'][[1]] >= threshold) && (dfMerge[i,]['DO.y'][[1]] < threshold)) {
        doRRI = doRRI + 1
      }
      
    }
    
    #calculate percents
    calcBoth <- round(doBoth/nrow(dfMerge) * 100,2)
    calcFourty <- round(doFourty/nrow(dfMerge) * 100,2)
    calcRRI <- round(doRRI/nrow(dfMerge) * 100,2)
    
    #bind rows to output df
    compDf <-
      rbind(
        compDf,
        data.frame(
          'time period' = name,
          'both under threshold' = calcBoth,
          'fourty under threshold' = calcFourty,
          'RRI under threshold' = calcRRI))
    
  }
}

#create output file path
compPath <- paste(mainPath,'percentCompare.csv',sep = '')


#append output df to text file
write.table(
  compDf,
  compPath,
  sep = ',',
  col.names = TRUE,
  row.names = FALSE,
  quote = FALSE)

#Bar Plot with percentcompare.csv

setwd("C:/Users/jrinde/OneDrive - California Department of Water Resources/DO Modification/Synthesis/Csv files")
library("ggplot2")
library("ggthemes")
theme_set(theme_bw())

compare = read.csv(file = "percent.compare.csv")
head(compare)

compare.plot <- ggplot(data=compare, aes(x=station, y=percent, fill=station, shape=as.factor(order))) + geom_bar(stat="identity") + facet_wrap(~season) + ggtitle("Summer") + theme(legend.position = "none") + labs(y="Dissolved Oxygen (%)", x = "Station") + ylim(0,50)
compare.plot
