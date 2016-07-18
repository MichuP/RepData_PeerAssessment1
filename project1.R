library(dplyr)
library(ggplot2)
library(timeDate)
wd <- 'C:/Users/Micha³.Micha³-PC/Documents/reproducible_research/'
setwd(wd)

### LOADING AND PROCESSING DATA ###
rawData <- read.csv(paste(wd, 'activity.csv', sep=''), header = TRUE, na.strings = "NA")
data <- filter(rawData, steps != 'NA')
data$date <- as.Date(data$date)


### Constructing data - for median I'm omitting 0 as otherwise most median values for days would be 0
sumByDay <- data %>% group_by(date) %>% summarize(total_steps=sum(steps), mean_steps=mean(steps), median_steps=median(steps[steps>0]))

#total
ggplot(sumByDay, aes(x=sumByDay$date, y=sumByDay$total_steps), width=sumByDay$length) +
  geom_bar(aes(fill=sumByDay$total_steps), stat="identity", position="identity") + 
  xlab('') + ylab('Total steps') 

#mean
ggplot(sumByDay, aes(x=sumByDay$date, y=sumByDay$mean_steps), width=sumByDay$length) +
  geom_bar(aes(fill=sumByDay$mean_steps), stat="identity", position="identity") + 
  xlab('') + ylab('Mean of steps') 

#median
ggplot(sumByDay, aes(x=sumByDay$date, y=sumByDay$median_steps), width=sumByDay$length) +
  geom_bar(aes(fill=sumByDay$median_steps), stat="identity", position="identity") + 
  xlab('') + ylab('Mean of steps') 


### WHAT IS THE AVERAGE DAILY ACTIVITY PATTERN?

#time series
sumByInterval <- data %>% group_by(interval) %>% summarize(total_steps=sum(steps), mean_steps=mean(steps))

plot(sumByInterval$interval, sumByInterval$total_steps, type = "l", ylab='Avg Total Steps by Interval', xlab='')

#max steps in time interval
maxStepsByInterval <- filter(sumByInterval, total_steps == max(sumByInterval$total_steps))

### IMPUTING MISSING VALUES ###
cleanData <- filter(rawData, complete.cases(rawData))

#missing rows
missingRows <- nrow(rawData) - nrow(cleanData)

#imputing data
inputData <- filter(rawData, !complete.cases(rawData))
inputData$steps <- replace(inputData$steps, inputData$interval %in% sumByInterval$interval, sumByInterval$mean_steps)

finalData <- rbind(cleanData, inputData) %>% arrange( date, interval)
finalData$date <- as.Date(finalData$date)

#histogram
sumByDay2 <- finalData %>% group_by(date) %>% summarize(total_steps=sum(steps), mean_steps=mean(steps), median_steps=median(steps[steps>0]))

ggplot(sumByDay2, aes(x=sumByDay2$date, y=sumByDay2$total_steps), width=sumByDay2$length) +
  geom_bar(aes(fill=sumByDay2$total_steps), stat="identity", position="identity") + 
  xlab('') + ylab('Mean steps') 



### ARE THERE DIFFERENCES IN ACTIVITY PATTERNS BETWEEN WEEKDAYS AND WEEKENDS ###

sumByDay3 <- finalData %>% group_by(date) %>% summarize(total_steps=sum(steps), mean_steps=mean(steps), median_steps=median(steps[steps>0])) %>%
  mutate(day_type = ifelse(isWeekday(date, wday=1:5), 'weekday', 'weekend'))


ggplot(sumByDay3, aes(date, mean_steps)) + geom_line() + facet_grid(day_type ~ .) + xlab('') + ylab('Mean steps')
