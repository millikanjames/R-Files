#James Millikan, SJ
#Verbum Dei High School
#Charts introduced in Chapter 1 of Statistical Reasoning in Sports by Tabor and Franklin
#September 4, 2021

#Pie Chart
slices<- c(113, 246)
lbls<-c("Hit", "Missed")
pie(slices, labels = lbls, main="Pie Chart LeBron Threes Reg Season (2008)")

#Pie Chart with Percentages
slices<- c(113, 246)
lbls<-c("Hit", "Missed")
pct<-round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls, "%", sep="") #add percents to labels
pie(slices,labels=lbls, col=rainbow(length(lbls)), main="Pie Chart of LeBron Threes Reg Season (2008)")

#Bar Chart with Percentages
performances<-c(31.5, 26)
barplot(performances, names.arg=c("Regular Season", "Playoffs"), 
        main="LeBron James 3-Point Shooting Performance (2008)", ylab="Percent of 3-Point Shots Made")

#Dot Chart 
SimPerf<-c(26, 27, 36, 33, 30, 27, 30, 27, 30, 41, 39, 34, 27, 38, 36, 33,
           27, 31, 37, 24, 29, 40, 29, 30, 33, 39, 24, 40, 37, 28,
           28, 37, 30, 30, 30, 27, 28, 30, 32, 32, 28, 37, 23, 42, 23,
           27, 33, 23, 33, 40)
stripchart(SimPerf, method="stack")

#Make the dot chart more aesthetically pleasing and add labels.
stripchart(SimPerf, method = "stack", offset = .5, at = 0, pch = 19,
  col = "steelblue", main = "LeBron Simulated Playoff Performances", xlab = "Simulated Percentage of Made 3-Point Shots")

#Graphics can also be made using the ggplot2 R package:

#Install the ggplot2 package by downloading the tidyverse
install.packages("tidyverse")

#Update ggplot2
update.packages("ggplot2")

#Stacked Bar Plot with Colors and Legend

#library
library(ggplot2)

#create a dataset
Season<-c(rep("Regular Season", 2), rep("Playoffs", 2))
Outcome<-rep(c("Missed", "Made"), 2)
Performance<-c(246, 113, 54, 14)
data <- data.frame(Season, Outcome, Performance)

#Stacked
ggplot(data, aes(fill=Outcome, y=Performance, x=Season)) + geom_bar(position="stack", stat="identity")+scale_fill_manual(values = c("chartreuse4","firebrick"))

#Stacked + percent
ggplot(data, aes(fill=Outcome, y=Performance, x=Season)) + geom_bar(position="fill", stat="identity")+scale_fill_manual(values = c("chartreuse4","firebrick"))

#I'll try to upload scripts of charts for Chapter 2 in a couple of weeks. Feel free to reach out to me
#at jmillikan@jesuits.org if you have any questions. AMDG.
