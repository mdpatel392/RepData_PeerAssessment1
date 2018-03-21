to_hmm <- function(interval) {
        library(stringi)
        library(lubridate)
        minutes <- stri_sub(interval, -2, -1)
        if (nchar(interval) < 3) {
                hours <- 0
        } else {
                hours <- stri_sub(interval, 1, nchar(interval)-2)
        }
        
        result <- paste(hours, minutes, sep=":")
        result <- hm(result)
        result
}
activity <- mutate(activity, interval = to_hmm(interval) )

# version 2
to_hmm <- function(interval) {
        library(stringi)
        library(lubridate)
        minutes <- stri_sub(interval, -2, -1)
        if (nchar(interval) < 3) {
                hours <- 0
        } else {
                hours <- stri_sub(interval, 1, nchar(interval)-2)
        }
        
        result <- paste(hours, minutes, sep=":")
        result <- hm(result)
        result
}
activity <- mutate(activity, interval = to_hmm(interval)) %>%
        mutate(interval=as.character(interval))



```{r weekend_comparison, cache=TRUE, fig.width=10}
library(ggplot2)
daily_patterns <- activity_without_NAs %>% mutate(day=factor(day,levels=c("weekend","weekday")),steps_no_NAs=as.numeric(steps_no_NAs)) %>% group_by(interval,day) %>% summarise(average=mean(steps_no_NAs))
qplot(interval,average,data=daily_patterns,geom="line",facets=day~.)
```