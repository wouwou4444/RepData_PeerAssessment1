## Usefull commands to do a vlookup like search and replacement

  imputed_data <- transform(data, steps = ifelse(is.na(data$steps), 
  steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))
  
  ### Other alternative
  activityDataImputed <- activityData
  naIndices <- which(is.na(activityDataImputed$steps))
  intervalMeans <- tapply(activityDataImputed$steps, 
                          activityDataImputed$interval, 
                          mean, 
                          na.rm = TRUE, 
                          simplify = T)
  activityDataImputed$steps[naIndices] <- intervalMeans[as.character(activityDataImputed$interval[naIndices])]
