setwd("/Users/hadoop/RepData_PeerAssessment2/")
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile= tmpfile,method="curl")
file.size(tmpfile)
file.size(data.file)
data.file <-"repdata-data-StormData.csv"
topData <- read.table(data.file, sep=",", nrows = 100, header=TRUE)
(col.classes <- sapply(topData, class))

#using data fread dev to using fread
#remove.packages("data.table")         # First remove the current version
#require("devtools")                        
#install_github("Rdatatable/data.table", build_vignettes = FALSE)  
require(data.table)



if(require(data.table)  && packageVersion("data.table")  >= "1.9.5" ) {
        raw.data <- fread(data.file, header= TRUE, sep=",", colClasses = col.classes, verbose = TRUE)
} else {
        raw.data <- read.table(data.file, sep=",", header = TRUE)
}

head(data)
library(dplyr)



# uppercase EVTYPE, TIME_ZONE to reduce typos
# cut BGN_DATE by year into c('1950-1959', '1960-1969','1970-1979','1980-1989','1990-1999','2000-2011')
# ignore EVTYPE contains "/"
# ignore PROPDMGEXP/CROPDMGEXP not in (B,M,K)
# adjust PROPDMG Amount by PROPDMGEXP (K,M,B)
# adjust CROPDMG Amount by CROPDMGEXP (K,M,B)
cleanData <- function(data) {
        clean.data <- data %>%
                mutate(EVTYPE = gsub("[^(a-zA-Z0-9/,)]",".", toupper(EVTYPE)) ) %>%
                mutate(TIME_ZONE = toupper(TIME_ZONE)) %>%
                mutate(PROPDMGEXP = toupper(PROPDMGEXP)) %>%
                mutate(CROPDMGEXP = toupper(CROPDMGEXP)) %>%
                filter(PROPDMGEXP %in% c("B","M","K")) %>%
                filter(CROPDMGEXP %in% c("B","M","K")) %>%
                mutate(BGN_DATE = as.Date(BGN_DATE, format="%m/%d/%Y")) %>%
                mutate(BGN_YR = year(BGN_DATE)) %>%
                mutate(BGN_DECADE = cut(BGN_YR, breaks = c(1949, 1959, 1969, 1979, 1989, 1999,2012) ) )%>%
                mutate(PROPDMG = ifelse(PROPDMGEXP == "K", ifelse(PROPDMG > 1000, PROPDMG, PROPDMG * 1000),
                                        ifelse(PROPDMGEXP == "M", ifelse(PROPDMG > 1000000, PROPDMG, PROPDMG * 1000000), 
                                               ifelse(PROPDMGEXP == "B", ifelse(PROPDMG > 1000000000, PROPDMG, PROPDMG * 1000000000), PROPDMG ) ) )) %>%
                mutate(CROPDMG = ifelse(CROPDMGEXP == "K", CROPDMG * ifelse(CROPDMG > 1000, CROPDMG, CROPDMG * 1000) , 
                                        ifelse(CROPDMGEXP == "M", ifelse(CROPDMG > 1000000, CROPDMG, CROPDMG * 1000000), 
                                               ifelse(CROPDMGEXP == "B", ifelse(CROPDMG > 1000000000, CROPDMG, CROPDMG * 1000000000) , CROPDMG ) ) )) 
        levels(clean.data$BGN_DECADE) <- c('1950-1959', '1960-1969','1970-1979','1980-1989','1990-1999','2000-2011')
        clean.data <- subset(clean.data, PROPDMG != 115000000000)
        return (clean.data)
}
if(!require(dplyr)) {
        install.packages("dplyr")
        require(dplyr)
}
clean.data <- cleanData(raw.data)



clean.data.1 <- subset(clean.data, PROPDMG != 115000000000)
clean.data[which(clean.data$CROPDMG == max(clean.data$CROPDMG))]
# find the average INJURIES and FATALITIES 
factor <- mean(subset(clean.data, INJURIES > 3 )$INJURIES, na.rm = TRUE) /
          mean(subset(clean.data, FATALITIES > 3 )$FATALITIES, na.rm = TRUE)
factor

# - sum.DMG = sum.PROPDMG + sum.CROPDMG
# - sum.casualties = sum.FATALITIES  + sum.INJURIES
# - index.casualties = sum.FATALITIES * 3 + sum.INJURIES
#   I gave FATALITIES a factor of 3 to weight impact
summaryEventData <- function(cleanData) {
        summary.data <- cleanData %>%
                group_by(EVTYPE) %>%
                summarise(event.count=n() ,
                          sum.FATALITIES = sum(FATALITIES, na.rm= TRUE),
                          sum.INJURIES = sum(INJURIES, na.rm= TRUE),
                          sum.PROPDMG = sum(PROPDMG, na.rm= TRUE),
                          sum.CROPDMG = sum(CROPDMG, na.rm= TRUE)) %>%
                mutate(sum.DMG = sum.PROPDMG + sum.CROPDMG)  %>%
                mutate(sum.casualties = sum.FATALITIES  + sum.INJURIES) %>%
                mutate(index.casualties = sum.FATALITIES * 3 + sum.INJURIES)
        return (summary.data)
}


select(summary.event.data[which(summary.event.data$sum.DMG == max(summary.event.data$sum.DMG))], c(1,4,5,6,7))


head(clean.data)
summary.event.data <- summaryEventData(clean.data)
summary.event.data
summary.dmg.data <- summary.event.data %>%
        arrange(desc(sum.DMG)) %>%
        top_n(20) %>%
        select(EVTYPE, c(1,5,6,7))

library(ggplot2)
p1 <- ggplot(summary.dmg.data, aes(x=EVTYPE, y=sum.DMG))  +
        geom_bar(stat="identity")  +
        labs(title = "Total damages casuse by Top 20 Weather Event") +
        labs(y = "Estimates of Damage (property + crop) ") +
        labs(x = "Event Type") +
        theme(axis.text.x= element_text(angle=90, vjust=0.6, size=8))+
        theme(title= element_text(vjust=0.6, size=8))
plot(p1)

select(subset(summary.event.data, EVTYPE =="FLOOD"), c(1,5,6,7))
# plot 
summary.casualties.data <- summary.event.data %>%
        arrange(desc(index.casualties)) %>%
        top_n(20) %>%
        select(EVTYPE, c(1,3,4,8,9))
summary.casualties.data
p2 <- ggplot(summary.casualties.data, aes(x=EVTYPE, y=sum.casualties))  +
        geom_bar(stat="identity")  +
        labs(title = "Total Casualties casuse by Top 20 Weather Event") +
        labs(y = "Casualties (fatalities + injuries) ") +
        labs(x = "Event Type") +
        theme(axis.text.x= element_text(angle=90, vjust=0.6, size=8))+
        theme(title= element_text(vjust=0.6, size=8))
plot(p2)

source("multiplot.R")
multiplot(p1, p2, cols=2)

#Flooe/ Hurican/Typhoon 
#summary top 3 damage by Time_zone, decade
summary.damage <- clean.data %>%
            filter(EVTYPE %in% as.data.frame(summary.dmg.data)[1:3,1]) %>%
            filter(TIME_ZONE %in% c("PST","MST","EST","CST")) %>%
            group_by(EVTYPE,BGN_YR,TIME_ZONE) %>%
            summarise(event.count=n() ,
                  sum.PROPDMG = sum(PROPDMG, na.rm= TRUE),
                  mean.PROPDMG = mean(PROPDMG, na.rm= TRUE),
                  sum.CROPDMG = sum(CROPDMG, na.rm= TRUE),
                  mean.CROPDMG = mean(CROPDMG, na.rm= TRUE)) %>%
        mutate(sum.Damage = sum.PROPDMG + sum.CROPDMG)  %>%
        mutate(mean.Damage = mean.PROPDMG + mean.CROPDMG) 

library(ggplot2)
pd1 <-  ggplot(aes(x=BGN_YR, col=EVTYPE), data=summary.damage) +        
        geom_line(aes(y= sum.Damage))+
        geom_point(aes(y= sum.Damage, shape=EVTYPE))+
        facet_wrap(~ TIME_ZONE, ncol=2) +  
        labs(title = "Total damages by Top3 Event Across the United States" ) + 
        labs(y = "Estimates of Damage (property + crop)") +
        labs(x = "Year of Event")  +
        theme(title=element_text(vjust=0.5,hjust=0.5, size=7))+ 
        theme(axis.text.x= element_text(angle=0, vjust=0.5, size=8))    
plot(pd1)

#Flooe/ Hurican/Typhoon 
#summary top 3 damage by Time_zone, decade
summary.casualties <- clean.data %>%
        filter(EVTYPE %in% as.data.frame(summary.casualties.data)[1:3,1]) %>%
        filter(TIME_ZONE %in% c("PST","MST","EST","CST")) %>%
        group_by(EVTYPE,BGN_YR,TIME_ZONE) %>%
        summarise(event.count=n() ,
                  sum.FATALITIES = sum(FATALITIES, na.rm= TRUE),
                  sum.INJURIES = sum(INJURIES, na.rm= TRUE)) %>%
        mutate(sum.Casualties = sum.FATALITIES + sum.INJURIES)  


pc1 <- ggplot(aes(x=BGN_YR, col=EVTYPE), data=summary.casualties) +        
        geom_line(aes(y= sum.Casualties, col=EVTYPE))+
        scale_x_continuous(breaks=unique(summary.casualties$BGN_YR)) +
        facet_wrap(~ TIME_ZONE) +  
        labs(title = "Total Casualties by Top3 Event Across the United States" ) + 
        labs(y = "Casualties(Fatalities + Injuries)") +
        labs(x = "Year of Event")  +
        theme(axis.text.y=element_text(vjust=0.5,hjust=0.5, size=8))+ 
        theme(axis.text.x= element_text(angle=90, vjust=0.5, size=8))    

plot(pc1)

